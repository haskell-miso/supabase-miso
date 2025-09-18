-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Storage
  ( -- * Functions
    createBucket
  , getBucket
  , listBuckets
  , updateBucket
  , deleteBucket
  , emptyBucket
  , uploadFile
  , downloadFile
  , listAllFiles
  , replaceFile
  , moveFile
  , copyFile
  , deleteFiles
  , createSignedUrl
  , createSignedUrls
  , createSignedUploadUrl
  , uploadToSignedUrl
  , getPublicUrl
    -- * Types
  , BucketOptions(..)
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
import           Miso.FFI
-----------------------------------------------------------------------------
import           Supabase.Miso.Core
-----------------------------------------------------------------------------
data BucketOptions
  = BucketOptions
  { boPublic :: Bool
  , boAllowedMimeTypes :: [MisoString]
  , boFileSizeLimit :: Int
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal BucketOptions where
  toJSVal BucketOptions {..} = do
    o <- create
    set "public" boPublic o
    set "allowedMimeTypes" boAllowedMimeTypes o
    set "fileSizeLimit" boFileSizeLimit o
    toJSVal o
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-createbucket
createBucket
  :: MisoString
  -- ^ Bucket identifier
  -> BucketOptions
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
createBucket args bucketOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  bucketOptions_ <- toJSVal bucketOptions
  runSupabase "storage" "createBucket" [args_, bucketOptions_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-getbucket
getBucket
  :: MisoString
  -- ^ Bucket identifier
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
getBucket args successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  runSupabase "storage" "getBucket" [args_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-listbuckets
listBuckets
  :: ([Value] -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
listBuckets successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  runSupabase "storage" "listBuckets" emptyArgs successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-updatebucket
updateBucket
  :: MisoString
  -- ^ Bucket identifier
  -> BucketOptions
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
updateBucket args bucketOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  bucketOptions_ <- toJSVal bucketOptions
  runSupabase "storage" "updateBucket" [args_, bucketOptions_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-deletebucket
deleteBucket
  :: MisoString
  -- ^ Bucket identifier
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
deleteBucket args successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  runSupabase "storage" "deleteBucket" [args_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-emptybucket
emptyBucket
  :: MisoString
  -- ^ Bucket identifier
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
emptyBucket args successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  runSupabase "storage" "emptyBucket" [args_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-uploadfile
uploadFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ The file name
  -> File
  -- ^ The file to upload
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
uploadFile bucket fileName file options successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  options_ <- toJSVal options
  file_ <- toJSVal file
  fileName_ <- toJSVal fileName
  runSupabaseFrom "storage" bucket "upload" [fileName_, file_, options_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-downloadfile
downloadFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ The file name
  -> Value
  -- ^ Options
  -> (File -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Errorful
  -> Effect parent model action
downloadFile bucket fileName opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallbackFile sink errorful successful
  errorful_ <- errorCallback sink errorful
  fileName_ <- toJSVal fileName
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "download" [fileName_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-list
listAllFiles
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ The file name
  -> Value
  -- ^ Options
  -> ([Value] -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Errorful
  -> Effect parent model action
listAllFiles bucket fileName options successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  options_ <- toJSVal options
  fileName_ <- toJSVal fileName
  runSupabaseFrom "storage" bucket "list" [fileName_, options_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-list
replaceFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ The file name
  -> File
  -- ^ The file to upload
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
replaceFile bucket fileName file options successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  options_ <- toJSVal options
  file_ <- toJSVal file
  fileName_ <- toJSVal fileName
  runSupabaseFrom "storage" bucket "update" [fileName_, file_, options_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-move
moveFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ Target file
  -> MisoString
  -- ^ New file
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
moveFile bucket target destination opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  target_ <- toJSVal target
  destination_ <- toJSVal destination
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "move" [target_, destination_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-copy
copyFile
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ Target file
  -> MisoString
  -- ^ New file
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
copyFile bucket target destination opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  target_ <- toJSVal target
  destination_ <- toJSVal destination
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "copy" [target_, destination_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-remove
deleteFiles
  :: MisoString
  -- ^ Bucket identifier
  -> [MisoString]
  -- ^ Files to remove
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
deleteFiles bucket files successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  files_ <- toJSVal files
  runSupabaseFrom "storage" bucket "remove" [files_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-createsignedurl
createSignedUrl
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ File to sign
  -> Int
  -- ^ Expiration time
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
createSignedUrl bucket file expiry successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  file_ <- toJSVal file
  expiry_ <- toJSVal expiry
  runSupabaseFrom "storage" bucket "createSignedUrl" [file_, expiry_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-createsignedurls
createSignedUrls
  :: MisoString
  -- ^ Bucket identifier
  -> [MisoString]
  -- ^ Files to sign
  -> Int
  -- ^ Expiration time
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
createSignedUrls bucket files expiry successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  files_ <- toJSVal files
  expiry_ <- toJSVal expiry
  runSupabaseFrom "storage" bucket "createSignedUrls" [files_, expiry_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-createsigneduploadurl
createSignedUploadUrl
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ File to upload
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
createSignedUploadUrl bucket file opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  file_ <- toJSVal file
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "createSignedUploadUrl" [file_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-uploadtosignedurl
uploadToSignedUrl
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ File name
  -> MisoString
  -- ^ Token
  -> File
  -- ^ File to upload
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
uploadToSignedUrl bucket fileName token file opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  fileName_ <- toJSVal fileName
  token_ <- toJSVal token
  file_ <- toJSVal file
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "uploadToSignedUrl" [fileName_, token_, file_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/storage-from-getpublicurl
getPublicUrl
  :: MisoString
  -- ^ Bucket identifier
  -> MisoString
  -- ^ File name
  -> Value
  -- ^ Options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
getPublicUrl bucket fileName opts successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  fileName_ <- toJSVal fileName
  opts_ <- toJSVal opts
  runSupabaseFrom "storage" bucket "getPublicUrl" [fileName_, opts_] successful_ errorful_
-----------------------------------------------------------------------------
