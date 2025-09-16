-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Database
  ( -- * Functions
    select
    -- * Types
  , Count (..)
  , FetchOptions (..)
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
data Count = Exact | Planned | Estimated
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal Count where
  toJSVal = \case
    Exact -> toJSVal "exact"
    Planned -> toJSVal "planned"
    Estimated -> toJSVal "estimated"
-----------------------------------------------------------------------------
data FetchOptions
  = FetchOptions
  { foCount :: Maybe Count
  , foHead :: Maybe Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal FetchOptions where
  toJSVal FetchOptions {..} = do
    o <- create
    set "count" foCount o
    set "head" foHead o
    toJSVal o
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/select
select
  :: MisoString
  -- ^ Table name
  -> MisoString
  -- ^ Query string
  -> FetchOptions
  -- ^ Fetch options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
select table args fetchOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  args_ <- toJSVal args
  fetchOptions_ <- toJSVal fetchOptions
  runSupabaseQuery table "select" [args_, fetchOptions_] successful_ errorful_
-----------------------------------------------------------------------------
