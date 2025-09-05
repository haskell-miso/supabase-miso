-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Auth
  ( -- * Functions
    signUpEmail
    -- * Types
  , User               (..)
  , AuthData           (..)
  , Identity           (..)
  , SignUpPhone        (..)
  , SignUpEmail        (..)
  , AppMetadata        (..)
  , SignUpChannel      (..)
  , AuthResponse       (..)
  , SignUpEmailOptions (..)
  , SignUpPhoneOptions (..)
  -- * Smart constructors
  , defaultSignUpEmailOptions
  , defaultSignUpPhoneOptions
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
deriving instance FromJSONKey MisoString
deriving instance ToJSONKey MisoString
deriving instance Hashable MisoString
-----------------------------------------------------------------------------
