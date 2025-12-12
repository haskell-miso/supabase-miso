-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Auth
  ( -- * Functions
    signUpEmail
  , signInWithPassword
  , signOut
  , resetPasswordForEmail
  , onAuthStateChange
    -- * Types
  , User                 (..)
  , AuthData             (..)
  , Identity             (..)
  , SignUpPhone          (..)
  , SignUpEmail          (..)
  , AppMetadata          (..)
  , SignUpChannel        (..)
  , AuthResponse         (..)
  , SignUpEmailOptions   (..)
  , SignUpPhoneOptions   (..)
  , SignInCredentials    (..)
  , SignOutOptions       (..)
  , SignOutScope         (..)
  , ResetPasswordOptions (..)
  , AuthChangeEvent      (..)
  , AuthChangeCallback
  , Subscription         (..)
  -- * Smart constructors
  , defaultSignUpEmailOptions
  , defaultSignUpPhoneOptions
  , defaultSignOutOptions
  , defaultResetPasswordOptions
  -- * Helpers
  , parseAuthChangeEvent
  ) where
-----------------------------------------------------------------------------
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.HashMap.Strict (HashMap)
import           Data.Time
import           Data.Aeson
import           Control.Monad
import           Language.Javascript.JSaddle hiding (Success)
import           Miso hiding ((<#))
-----------------------------------------------------------------------------
import           Supabase.Miso.Core
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/auth-api
data SignUpEmail
  = SignUpEmail
  { sueEmail :: Email
  , suePassword :: MisoString
  , sueOptions :: Maybe SignUpEmailOptions
  }
-----------------------------------------------------------------------------
data SignUpPhone
  = SignUpPhone
  { supPhone :: Phone
  , supPassword :: MisoString
  , supOptions :: Maybe SignUpPhoneOptions
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
newtype Password = Password MisoString
  deriving (Show, Eq, ToJSVal)
-----------------------------------------------------------------------------
newtype Phone = Phone MisoString
  deriving (Show, Eq, ToJSVal)
-----------------------------------------------------------------------------
newtype Email = Email MisoString
  deriving (Show, Eq, ToJSVal)
-----------------------------------------------------------------------------
data SignUpChannel = SMS | WhatsApp
 deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal SignUpChannel where
  toJSVal = \case
    SMS -> toJSVal "sms"
    WhatsApp -> toJSVal "whatsapp"
-----------------------------------------------------------------------------
defaultSignUpEmailOptions :: SignUpEmailOptions
defaultSignUpEmailOptions = SignUpEmailOptions Nothing Nothing Nothing
-----------------------------------------------------------------------------
defaultSignUpPhoneOptions :: SignUpPhoneOptions
defaultSignUpPhoneOptions = SignUpPhoneOptions Nothing Nothing Nothing
-----------------------------------------------------------------------------
data SignInCredentials
  = SignInCredentials
  { sicEmail :: Email
  , sicPassword :: Password
  }
-----------------------------------------------------------------------------
data SignOutScope = Global | Local | Others
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal SignOutScope where
  toJSVal = \case
    Global -> toJSVal "global"
    Local -> toJSVal "local"
    Others -> toJSVal "others"
-----------------------------------------------------------------------------
data SignOutOptions
  = SignOutOptions
  { soScope :: Maybe SignOutScope
  }
-----------------------------------------------------------------------------
defaultSignOutOptions :: SignOutOptions
defaultSignOutOptions = SignOutOptions Nothing
-----------------------------------------------------------------------------
data ResetPasswordOptions
  = ResetPasswordOptions
  { rpoRedirectTo :: Maybe MisoString
  , rpoCaptchaToken :: Maybe MisoString
  }
-----------------------------------------------------------------------------
defaultResetPasswordOptions :: ResetPasswordOptions
defaultResetPasswordOptions = ResetPasswordOptions Nothing Nothing
-----------------------------------------------------------------------------
data AuthChangeEvent
  = InitialSession
  | SignedIn
  | SignedOut
  | PasswordRecovery
  | TokenRefreshed
  | UserUpdated
  | UnknownAuthEvent MisoString  -- For forward compatibility
  deriving (Show, Eq)
-----------------------------------------------------------------------------
parseAuthChangeEvent :: MisoString -> AuthChangeEvent
parseAuthChangeEvent event = case event of
  "INITIAL_SESSION" -> InitialSession
  "SIGNED_IN" -> SignedIn
  "SIGNED_OUT" -> SignedOut
  "PASSWORD_RECOVERY" -> PasswordRecovery
  "TOKEN_REFRESHED" -> TokenRefreshed
  "USER_UPDATED" -> UserUpdated
  other -> UnknownAuthEvent other
-----------------------------------------------------------------------------
type AuthChangeCallback action = AuthChangeEvent -> Maybe Session -> action
-----------------------------------------------------------------------------
data Subscription
  = Subscription
  { subUnsubscribe :: JSM ()
  }
-----------------------------------------------------------------------------
data SignUpEmailOptions
  = SignUpEmailOptions
  { sueCaptchaToken :: Maybe MisoString
  , sueSignUpData :: Maybe Value
  , sueEmailRedirectTo :: Maybe MisoString
  }
-----------------------------------------------------------------------------
data SignUpPhoneOptions
  = SignUpPhoneOptions
  { supCaptchaToken :: Maybe MisoString
  , supChannel :: Maybe SignUpChannel
  , supSignUpData :: Maybe Value
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal SignUpPhoneOptions where
  toJSVal SignUpPhoneOptions {..} = do
    o <- create
    forM supCaptchaToken $ \captchaToken_ ->
      set "captchaToken" captchaToken_ o
    forM supChannel $ \email_ ->
      set "channel" email_ o
    forM supSignUpData $ \data_ ->
      flip (set "data") o =<< toJSVal data_
    toJSVal o
-----------------------------------------------------------------------------
instance ToJSVal SignUpEmailOptions where
  toJSVal SignUpEmailOptions {..} = do
    o <- create
    forM sueCaptchaToken $ \captchaToken_ ->
      set "captchaToken" captchaToken_ o
    forM sueEmailRedirectTo $ \email_ ->
      set "emailRedirectTo" email_ o
    forM sueSignUpData $ \data_ ->
      flip (set "data") o =<< toJSVal data_
    toJSVal o
-----------------------------------------------------------------------------
instance ToJSVal SignUpEmail where
  toJSVal = \case
    SignUpEmail {..} -> do
      o <- create
      email_ <- toJSVal sueEmail
      password_ <- toJSVal suePassword
      set "email" sueEmail o
      set "password" suePassword o
      forM_ sueOptions $ \opts -> do
        opts_ <- toJSVal opts
        set "options" opts_ o
      toJSVal o
-----------------------------------------------------------------------------
instance ToJSVal SignInCredentials where
  toJSVal SignInCredentials {..} = do
    o <- create
    set "email" sicEmail o
    set "password" sicPassword o
    toJSVal o
-----------------------------------------------------------------------------
instance ToJSVal SignOutOptions where
  toJSVal SignOutOptions {..} = do
    o <- create
    forM_ soScope $ \scope -> set "scope" scope o
    toJSVal o
-----------------------------------------------------------------------------
instance ToJSVal ResetPasswordOptions where
  toJSVal ResetPasswordOptions {..} = do
    o <- create
    forM_ rpoRedirectTo $ \redirectTo -> set "redirectTo" redirectTo o
    forM_ rpoCaptchaToken $ \captchaToken -> set "captchaToken" captchaToken o
    toJSVal o
-----------------------------------------------------------------------------
instance ToJSVal SignUpPhone where
  toJSVal = \case
    SignUpPhone {..} -> do
      o <- create
      phone_ <- toJSVal supPhone
      password_ <- toJSVal supPassword
      set "phone" phone_ o
      set "password" password_ o
      forM_ supOptions $ \opts -> do
        opts_ <- toJSVal opts
        set "options" opts_ o
      toJSVal o
-----------------------------------------------------------------------------
data SupabaseResult
  = SupabaseResult
  { supabaseData :: Value
  , supabaseError :: Value
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
signUpEmail
  :: SignUpEmail
  -- ^ SignUp options
  -> (AuthResponse -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
signUpEmail args successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  runSupabase "auth" "signUp" [args] successful_ errorful_
-----------------------------------------------------------------------------
signUpPhone
  :: SignUpPhone
  -- ^ SignUp options
  -> (AuthResponse -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
signUpPhone args successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  runSupabase "auth" "signUp" [args] successful_ errorful_
-----------------------------------------------------------------------------
signInWithPassword
  :: SignInCredentials
  -- ^ Sign in credentials (email and password)
  -> (AuthResponse -> action)
  -- ^ Success callback
  -> (MisoString -> action)
  -- ^ Error callback
  -> Effect parent model action
signInWithPassword credentials successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  runSupabase "auth" "signInWithPassword" [credentials] successful_ errorful_
-----------------------------------------------------------------------------
signOut
  :: SignOutOptions
  -- ^ Sign out options (optional scope)
  -> (Value -> action)
  -- ^ Success callback
  -> (MisoString -> action)
  -- ^ Error callback
  -> Effect parent model action
signOut options successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  runSupabase "auth" "signOut" [options] successful_ errorful_
-----------------------------------------------------------------------------
resetPasswordForEmail
  :: Email
  -- ^ User's email address
  -> ResetPasswordOptions
  -- ^ Password reset options (redirectTo, captchaToken)
  -> (Value -> action)
  -- ^ Success callback
  -> (MisoString -> action)
  -- ^ Error callback
  -> Effect parent model action
resetPasswordForEmail email options successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  emailVal <- toJSVal email
  optionsVal <- toJSVal options
  runSupabase "auth" "resetPasswordForEmail" [emailVal, optionsVal] successful_ errorful_
-----------------------------------------------------------------------------
onAuthStateChange
  :: (AuthChangeEvent -> Maybe Session -> action)
  -- ^ Callback function invoked on auth state changes
  -> (Subscription -> action)
  -- ^ Success callback with subscription object
  -> (MisoString -> action)
  -- ^ Error callback
  -> Effect parent model action
onAuthStateChange callback successful errorful = withSink $ \sink -> do
  -- Wrap the callback to parse the event string and session
  let wrappedCallback eventStr sessionMaybe = 
        let parsedSession = case sessionMaybe of
              Nothing -> Nothing
              Just val -> case fromJSON val of
                Success sess -> Just sess
                Error _ -> Nothing
        in callback (parseAuthChangeEvent eventStr) parsedSession
  callback_ <- authStateChangeCallback sink wrappedCallback
  successful_ <- subscriptionCallback sink (successful . Subscription)
  errorful_ <- errorCallback sink errorful
  runSupabase "auth" "onAuthStateChange" [callback_] successful_ errorful_
-----------------------------------------------------------------------------
data AuthResponse
  = AuthResponse
  { arData  :: AuthData
  , arError :: Maybe Value
  } deriving (Show)
-----------------------------------------------------------------------------
data AuthData
  = AuthData
  { adUser    :: User
  , adSession :: Session
  } deriving (Show)
-----------------------------------------------------------------------------
data User
  = User
  { userId               :: MisoString
  , userAud              :: MisoString
  , userRole             :: MisoString
  , userEmail            :: MisoString
  , userEmailConfirmedAt :: Maybe MisoString
  , userPhone            :: MisoString
  , userLastSignInAt     :: Maybe UTCTime
  , userAppMetadata      :: AppMetadata
  , userUserMetadata     :: HashMap MisoString Value  -- Empty object as arbitrary JSON
  , userIdentities       :: [Identity]
  , userCreatedAt        :: UTCTime
  , userUpdatedAt        :: UTCTime
  } deriving (Show)
-----------------------------------------------------------------------------
data AppMetadata
  = AppMetadata
  { amProvider  :: MisoString
  , amProviders :: [MisoString]
  } deriving (Show)
-----------------------------------------------------------------------------
data Identity
  = Identity
  { identityIdentityId   :: MisoString
  , identityId           :: MisoString
  , identityUserId       :: MisoString
  , identityIdentityData :: IdentityData
  , identityProvider     :: MisoString
  , identityLastSignInAt :: UTCTime
  , identityCreatedAt    :: UTCTime
  , identityUpdatedAt    :: UTCTime
  , identityEmail        :: MisoString
  } deriving (Show)
-----------------------------------------------------------------------------
data IdentityData
  = IdentityData
  { idEmail          :: MisoString
  , idEmailVerified  :: Bool
  , idPhoneVerified  :: Bool
  , idSub            :: MisoString
  } deriving (Show)
-----------------------------------------------------------------------------
data Session
  = Session
  { sessionAccessToken  :: MisoString
  , sessionTokenType    :: MisoString
  , sessionExpiresIn    :: Int
  , sessionExpiresAt    :: Int
  , sessionRefreshToken :: MisoString
  , sessionUser         :: User
  } deriving (Show)
-----------------------------------------------------------------------------
instance FromJSON AuthResponse where
  parseJSON = withObject "AuthResponse" $ \v ->
    AuthResponse
      <$> v .: "data"
      <*> v .: "error"
-----------------------------------------------------------------------------
instance FromJSON AuthData where
  parseJSON = withObject "AuthData" $ \v ->
    AuthData
      <$> v .: "user"
      <*> v .: "session"
-----------------------------------------------------------------------------
instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User
      <$> v .: "id"
      <*> v .: "aud"
      <*> v .: "role"
      <*> v .: "email"
      <*> v .: "email_confirmed_at"
      <*> v .: "phone"
      <*> v .: "last_sign_in_at"
      <*> v .: "app_metadata"
      <*> v .: "user_metadata"
      <*> v .: "identities"
      <*> v .: "created_at"
      <*> v .: "updated_at"
-----------------------------------------------------------------------------
instance FromJSON AppMetadata where
  parseJSON = withObject "AppMetadata" $ \v ->
    AppMetadata
      <$> v .: "provider"
      <*> v .: "providers"
-----------------------------------------------------------------------------
instance FromJSON Identity where
  parseJSON = withObject "Identity" $ \v ->
    Identity
      <$> v .: "identity_id"
      <*> v .: "id"
      <*> v .: "user_id"
      <*> v .: "identity_data"
      <*> v .: "provider"
      <*> v .: "last_sign_in_at"
      <*> v .: "created_at"
      <*> v .: "updated_at"
      <*> v .: "email"
-----------------------------------------------------------------------------
instance FromJSON IdentityData where
  parseJSON = withObject "IdentityData" $ \v ->
    IdentityData
      <$> v .: "email"
      <*> v .: "email_verified"
      <*> v .: "phone_verified"
      <*> v .: "sub"
-----------------------------------------------------------------------------
instance FromJSON Session where
  parseJSON = withObject "Session" $ \v ->
    Session
      <$> v .: "access_token"
      <*> v .: "token_type"
      <*> v .: "expires_in"
      <*> v .: "expires_at"
      <*> v .: "refresh_token"
      <*> v .: "user"
-----------------------------------------------------------------------------
instance ToJSON AuthResponse where
  toJSON (AuthResponse data_ error_) = object
    [ "data" .= data_
    , "error" .= error_
    ]
-----------------------------------------------------------------------------
instance ToJSON AuthData where
  toJSON (AuthData user session) = object
    [ "user" .= user
    , "session" .= session
    ]
-----------------------------------------------------------------------------
instance ToJSON User where
  toJSON User {..} = object
    [ "id"                 .= userId
    , "aud"                .= userAud
    , "role"               .= userRole
    , "email"              .= userEmail
    , "email_confirmed_at" .= userEmailConfirmedAt
    , "phone"              .= userPhone
    , "last_sign_in_at"    .= userLastSignInAt
    , "app_metadata"       .= userAppMetadata
    , "user_metadata"      .= userUserMetadata
    , "identities"         .= userIdentities
    , "created_at"         .= userCreatedAt
    , "updated_at"         .= userUpdatedAt
    ]
-----------------------------------------------------------------------------
instance ToJSON AppMetadata where
  toJSON (AppMetadata provider providers) = object
    [ "provider" .= provider
    , "providers" .= providers
    ]
-----------------------------------------------------------------------------
instance ToJSON Identity where
  toJSON Identity {..} = object
    [ "identity_id"     .= identityId
    , "id"              .= identityId
    , "user_id"         .= identityUserId
    , "identity_data"   .= identityIdentityData
    , "provider"        .= identityProvider
    , "last_sign_in_at" .= identityLastSignInAt
    , "created_at"      .= identityCreatedAt
    , "updated_at"      .= identityUpdatedAt
    , "email"           .= identityEmail
    ]
-----------------------------------------------------------------------------
instance ToJSON IdentityData where
  toJSON IdentityData{..} = object
    [ "email"          .= idEmail
    , "email_verified" .= idEmailVerified
    , "phone_verified" .= idPhoneVerified
    , "sub"            .= idSub
    ]
-----------------------------------------------------------------------------
instance ToJSON Session where
  toJSON Session {..} = object
    [ "access_token"  .= sessionAccessToken
    , "token_type"    .= sessionTokenType
    , "expires_in"    .= sessionExpiresIn
    , "expires_at"    .= sessionExpiresAt
    , "refresh_token" .= sessionRefreshToken
    , "user"          .= sessionUser
    ]
-----------------------------------------------------------------------------
deriving instance Hashable MisoString
-----------------------------------------------------------------------------
