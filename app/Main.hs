-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- | Notes: an example of supabase-miso.
--
-- * Auth: sign up and sign in with an email and password, a magic link,
--   anonymously, or with GitHub; the session is read when the page loads,
--   and followed with onAuthStateChange.
-- * Database: the notes table (supabase/migrations), each user's own (row
--   level security): select with a filter, an order and a limit; insert,
--   update and delete.
-- * Storage: a file for a note, in the private attachments bucket, read
--   through signed URLs.
--
-- static/index.html makes the supabase client (static/config.js says which
-- project) before it loads the app.
module Main (main) where
-----------------------------------------------------------------------------
import           Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust, mapMaybe)
-----------------------------------------------------------------------------
import           Miso
import           Miso.JSON
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.String (null)
-----------------------------------------------------------------------------
import           Supabase.Miso.Auth
import qualified Supabase.Miso.Database as DB
import qualified Supabase.Miso.Storage as S
-----------------------------------------------------------------------------
import           Prelude hiding (null)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app { mount = Just Init }
-----------------------------------------------------------------------------
-- | Who is signed in
data Me
  = Me
  { meId       :: MisoString
  , meName     :: MisoString
  -- ^ The email, or the phone; empty for an anonymous user
  , meProvider :: MisoString
  -- ^ email, github, ... (empty for an anonymous user)
  } deriving (Eq)
-----------------------------------------------------------------------------
-- | A row of the notes table
data Note
  = Note
  { noteId         :: Int
  , noteTitle      :: MisoString
  , noteDone       :: Bool
  , noteAttachment :: Maybe MisoString
  -- ^ Its path in the attachments bucket
  } deriving (Eq)
-----------------------------------------------------------------------------
instance FromJSON Note where
  parseJSON = withObject "Note" $ \o ->
    Note
      <$> o .: "id"
      <*> o .: "title"
      <*> o .: "done"
      <*> o .:? "attachment"
-----------------------------------------------------------------------------
data Status = Idle | Info MisoString | Problem MisoString
  deriving (Eq)
-----------------------------------------------------------------------------
data Model
  = Model
  { me       :: Maybe Me
  , origin   :: MisoString
  -- ^ The page's origin, where magic links and GitHub come back to
  , github   :: Bool
  -- ^ The project has GitHub sign-in (static/index.html asks)
  , email    :: MisoString
  , password :: MisoString
  , search   :: MisoString
  , newTitle :: MisoString
  , notes    :: [Note]
  , links    :: M.Map MisoString MisoString
  -- ^ Signed URLs of the attachments, by path
  , banner   :: Status
  } deriving (Eq)
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model Nothing "" False "" "" "" "" [] M.empty Idle
-----------------------------------------------------------------------------
data Action
  = Init
  | GotOrigin MisoString
  | GotGitHub Bool
  | GotSession (Maybe Session)
  | AuthChanged AuthChangeEvent (Maybe Session)
  | Subscribed Subscription
  | SetEmail MisoString
  | SetPassword MisoString
  | SignUp
  | SignedUp AuthResponse
  | SignIn
  | MagicLink
  | Anonymously
  | WithGitHub
  | SignOut
  | Said MisoString
  | Failed MisoString
  | Load
  | Loaded Value
  | Signed Value
  | SetSearch MisoString
  | SetNewTitle MisoString
  | Add
  | Toggle Note
  | Delete Note
  | Pick Note DOMRef
  | Upload Note MisoString File
  | Uploaded Note MisoString
  | Detach Note
  | Changed
-----------------------------------------------------------------------------
app :: App Model Action
app = component emptyModel update_ view_
-----------------------------------------------------------------------------
-- | The bucket of the attachments
bucket :: MisoString
bucket = "attachments"
-----------------------------------------------------------------------------
update_ :: Action -> Effect () () Model Action
update_ = \case
  Init -> do
    io (GotOrigin <$> (fromJSValUnchecked =<< (jsg "location" ! "origin")))
    io (GotGitHub <$> (fromJSValUnchecked =<< jsg "supabaseGitHub"))
    -- the session in local storage (or in the URL, back from a magic link
    -- or GitHub), and then its changes
    getSession GotSession Failed
    onAuthStateChange AuthChanged Subscribed Failed
  GotOrigin o ->
    modify $ \m -> m { origin = o }
  GotGitHub g ->
    modify $ \m -> m { github = g }
  GotSession s ->
    signedIn s
  AuthChanged _ s ->
    signedIn s
  Subscribed _ ->
    pure ()
  SetEmail e ->
    modify $ \m -> m { email = e }
  SetPassword p ->
    modify $ \m -> m { password = p }
  SignUp -> do
    Model {..} <- get
    working "Signing up..."
    signUpEmail (SignUpEmail (Email email) password Nothing) SignedUp Failed
  SignedUp r ->
    -- with confirmations on (auth.email.enable_confirmations), there is
    -- no session until the email is confirmed
    case adSession (arData r) of
      Nothing -> modify $ \m -> m { banner = Info "Check your email for the confirmation link." }
      Just _ -> modify $ \m -> m { banner = Idle }
  SignIn -> do
    Model {..} <- get
    working "Signing in..."
    signInWithPassword (SignInCredentials (Email email) (Password password)) (const (Said "")) Failed
  MagicLink -> do
    Model {..} <- get
    working "Sending the link..."
    signInWithOtp (Email email) defaultOtpOptions { otpEmailRedirectTo = Just origin }
      (const (Said ("A sign-in link is on its way to " <> email <> ".")))
      Failed
  Anonymously -> do
    working "Signing in..."
    signInAnonymously (const (Said "")) Failed
  WithGitHub -> do
    Model {..} <- get
    signInWithOAuth "github" defaultOAuthOptions { oauthRedirectTo = Just origin } (const (Said "")) Failed
  SignOut ->
    signOut defaultSignOutOptions (const (Said "")) Failed
  Said s ->
    modify $ \m -> m { banner = if null s then Idle else Info s }
  Failed e ->
    modify $ \m -> m { banner = Problem e }
  Load -> do
    Model {..} <- get
    -- row level security keeps it to this user's notes
    DB.selectWithFilters "notes" "id, title, done, attachment"
      ([ DB.ilike "title" ("%" <> search <> "%") | not (null search) ]
        ++ [ DB.orderBy "created_at" False, DB.limitTo 100 ])
      (DB.FetchOptions Nothing Nothing) Loaded Failed
  Loaded v ->
    case fromJSON v of
      Error e -> issue (Failed (ms e))
      Success ns -> do
        modify $ \m -> m { notes = ns }
        -- signed URLs for the attachments, for an hour
        case mapMaybe noteAttachment ns of
          [] -> pure ()
          paths -> S.createSignedUrls bucket paths 3600 Signed Failed
  Signed v ->
    -- [{ path, signedUrl, error }]
    case v of
      Array xs -> modify $ \m -> m { links = M.fromList (mapMaybe (parseMaybe signedUrl) xs) }
      _ -> pure ()
  SetSearch s -> do
    modify $ \m -> m { search = s }
    issue Load
  SetNewTitle t ->
    modify $ \m -> m { newTitle = t }
  Add -> do
    Model {..} <- get
    if null newTitle then pure () else do
      modify $ \m -> m { newTitle = "" }
      DB.insert "notes" (object [ "title" .= newTitle ]) (DB.InsertOptions Nothing Nothing) (const Changed) Failed
  Toggle n ->
    DB.updateTable "notes" (object [ "done" .= not (noteDone n) ]) [ DB.eq "id" (noteId n) ]
      (DB.UpdateOptions Nothing) (const Changed) Failed
  Delete n -> do
    forM_ (noteAttachment n) $ \p -> S.deleteFiles bucket [p] (const (Said "")) Failed
    DB.deleteFrom "notes" [ DB.eq "id" (noteId n) ] (DB.DeleteOptions Nothing) (const Changed) Failed
  Pick n input ->
    io $ do
      -- input.files is a FileList (not an Array): its first file, by index
      fl <- input ! "files"
      len <- fromJSValUnchecked =<< (fl ! "length")
      if len < (1 :: Int) then pure (Said "") else do
        f <- fl ! "0"
        name <- fromJSValUnchecked =<< (f ! "name")
        -- so that picking the same file again is a change too
        setValue input ""
        pure (Upload n name (File f))
  Upload n name file -> do
    Model {..} <- get
    forM_ me $ \Me {..} -> do
      -- the folder of the user's id: the storage policies allow only that
      let path = meId <> "/" <> ms (noteId n) <> "-" <> name
      working ("Uploading " <> name <> "...")
      S.uploadFile bucket path file (object [ "upsert" .= True ]) (const (Uploaded n path)) Failed
  Uploaded n path -> do
    -- the old file, if it had another name
    forM_ (noteAttachment n) $ \old ->
      if old == path then pure () else S.deleteFiles bucket [old] (const (Said "")) Failed
    attach n (toJSON path)
  Detach n ->
    forM_ (noteAttachment n) $ \p -> do
      S.deleteFiles bucket [p] (const (Said "")) Failed
      attach n Null
  Changed -> do
    modify $ \m -> m { banner = Idle }
    issue Load
  where
    signedUrl = withObject "SignedUrl" $ \o -> (,) <$> o .: "path" <*> o .: "signedUrl"
    working s = modify $ \m -> m { banner = Info s }
    attach n v =
      DB.updateTable "notes" (object [ "attachment" .= v ]) [ DB.eq "id" (noteId n) ]
        (DB.UpdateOptions Nothing) (const Changed) Failed
    signedIn s = do
      before <- gets me
      let after = fmap (whoIs . sessionUser) s
      modify $ \m -> m { me = after, banner = if isJust after then Idle else banner m }
      if after == before then pure () else
        case after of
          Nothing -> modify $ \m -> m { notes = [], links = M.empty, search = "" }
          Just _ -> issue Load
    whoIs u =
      Me (userId u)
         (if null (userEmail u) then userPhone u else userEmail u)
         (amProvider (userAppMetadata u))
-----------------------------------------------------------------------------
view_ :: Model -> View () () Model Action
view_ m = H.main_ [ P.class_ "app" ]
  [ H.header_ []
    [ H.h1_ [] [ "Notes" ]
    , H.p_ [ P.class_ "tagline" ] [ "miso + Supabase: auth, a database, and storage" ]
    ]
  , statusView (banner m)
  , maybe (signInView m) (notesView m) (me m)
  ]
-----------------------------------------------------------------------------
statusView :: Status -> View () () Model Action
statusView = \case
  Idle -> text ""
  Info s -> H.p_ [ P.class_ "status info" ] [ text s ]
  Problem s -> H.p_ [ P.class_ "status problem" ] [ text s ]
-----------------------------------------------------------------------------
signInView :: Model -> View () () Model Action
signInView Model {..} = H.section_ [ P.class_ "card" ]
  [ H.h2_ [] [ "Sign in" ]
  , H.form_ [ E.onSubmit SignIn ]
    [ H.label_ [] [ "Email"
      , H.input_ [ P.type_ "email", P.value_ email, P.placeholder_ "you@example.com"
                 , P.autocomplete_ "email", E.onInput SetEmail ] ]
    , H.label_ [] [ "Password"
      , H.input_ [ P.type_ "password", P.value_ password, P.placeholder_ "at least 6 characters"
                 , E.onInput SetPassword ] ]
    , H.div_ [ P.class_ "row" ]
      [ H.button_ [ P.type_ "submit" ] [ "Sign in" ]
      , H.button_ [ P.type_ "button", P.class_ "secondary", E.onClick SignUp ] [ "Sign up" ]
      ]
    ]
  , H.div_ [ P.class_ "or" ] [ "or" ]
  , H.div_ [ P.class_ "providers" ]
    [ H.button_ ([ P.class_ "secondary", E.onClick MagicLink ] ++ [ P.disabled_ | null email ]) [ "Email me a link" ]
    , if github
        then H.button_ [ P.class_ "secondary", E.onClick WithGitHub ] [ "Continue with GitHub" ]
        else H.button_ [ P.class_ "secondary", P.disabled_, P.title_ "Set up in supabase/config.toml" ]
               [ "GitHub (not set up)" ]
    , H.button_ [ P.class_ "secondary", E.onClick Anonymously ] [ "Try it anonymously" ]
    ]
  ]
-----------------------------------------------------------------------------
notesView :: Model -> Me -> View () () Model Action
notesView Model {..} Me {..} = H.section_ [ P.class_ "card" ]
  [ H.div_ [ P.class_ "account" ]
    [ H.span_ []
      [ text (if null meName then "Anonymous" else meName)
      , H.span_ [ P.class_ "badge" ] [ text (if null meProvider then "anonymous" else meProvider) ]
      ]
    , H.button_ [ P.class_ "link", E.onClick SignOut ] [ "Sign out" ]
    ]
  , H.form_ [ P.class_ "row", E.onSubmit Add ]
    [ H.input_ [ P.value_ newTitle, P.placeholder_ "A new note", E.onInput SetNewTitle ]
    , H.button_ ([ P.type_ "submit" ] ++ [ P.disabled_ | null newTitle ]) [ "Add" ]
    ]
  , H.input_ [ P.class_ "search", P.type_ "search", P.value_ search
             , P.placeholder_ "Search", E.onInput SetSearch ]
  , if notes == []
      then H.p_ [ P.class_ "empty" ] [ if null search then "No notes yet." else "No notes match." ]
      else H.ul_ [ P.class_ "notes" ] (map noteView notes)
  ]
  where
    noteView n = H.li_ [ P.class_ (if noteDone n then "note done" else "note") ]
      [ H.input_ [ P.type_ "checkbox", P.checked_ (noteDone n), E.onClick (Toggle n) ]
      , H.span_ [ P.class_ "title" ] [ text (noteTitle n) ]
      , case noteAttachment n of
          Nothing -> text ""
          Just p -> H.span_ [ P.class_ "file" ]
            [ case M.lookup p links of
                Just url -> H.a_ [ P.href_ url, P.target_ "_blank" ] [ text (fileName p) ]
                Nothing -> text (fileName p)
            , H.button_ [ P.class_ "link", E.onClick (Detach n) ] [ "×" ]
            ]
      , H.label_ [ P.class_ "attach" ]
        [ "Attach"
        , H.input_ [ P.type_ "file", E.onChangeWith (\_ ref -> Pick n ref) ]
        ]
      , H.button_ [ P.class_ "link delete", E.onClick (Delete n) ] [ "Delete" ]
      ]
    -- <user id>/<note id>-<name>
    fileName p = ms (drop 1 (dropWhile (/= '-') (dropWhile (/= '/') (fromMisoString p :: String))))
-----------------------------------------------------------------------------
