-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Storage
  ( -- * Functions
    -- * Types
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
  :: MisoString
  -- ^ Bucket identifier
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
listBuckets args successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  runSupabase "storage" "listBuckets" [args_] successful_ errorful_
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
