-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Core
  ( -- * Functions
    runSupabase
  , emptyArgs
  , successCallback
  , errorCallback
  ) where
-----------------------------------------------------------------------------
import Data.Aeson
import Miso.String
import Miso.FFI (syncCallback1)
-----------------------------------------------------------------------------
import Control.Monad
import Language.Javascript.JSaddle hiding (Success)
-----------------------------------------------------------------------------
-- | runSupabase('auth','signUp', args, successCallback, errorCallback);
runSupabase
  :: ToJSVal args
  => MisoString
  -- ^ Namespace
  -> MisoString
  -- ^ Method
  -> [args]
  -- ^ args
  -> Function
  -- ^ successful callback
  -> Function
  -- ^ errorful callback
  -> JSM ()
runSupabase namespace fnName args successful errorful = do
  args_ <- makeArgs args
  void $ jsg "globalThis" # "runSupabase" $
    (namespace, fnName, args_, successful, errorful)
-----------------------------------------------------------------------------
emptyArgs :: [JSVal]
emptyArgs = []
-----------------------------------------------------------------------------
successCallback
  :: FromJSON t
  => (action -> JSM ())
  -> (MisoString -> action)
  -> (t -> action)
  -> JSM Function
successCallback sink errorful successful = do
  syncCallback1 $ \result -> do
    fromJSON <$> fromJSValUnchecked result >>= \case
      Error msg -> do
        sink $ errorful (ms msg)
      Success result ->
        sink (successful result)
-----------------------------------------------------------------------------
errorCallback
  :: (action -> JSM ())
  -> (MisoString -> action)
  -> JSM Function
errorCallback sink errorful =
  syncCallback1 $ \result -> do
    fromJSON <$> fromJSValUnchecked result >>= \case
      Error msg -> do
        sink $ errorful (ms msg)
      Success result ->
        sink (errorful result)
-----------------------------------------------------------------------------
