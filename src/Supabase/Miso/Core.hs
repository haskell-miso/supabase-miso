-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Core
  ( -- * Functions
    runSupabase
  , runSupabaseFrom
  , runSupabaseSelect
  , runSupabaseUpdate
  , runSupabaseDelete
  , runSupabaseQuery
  , emptyArgs
  , successCallback
  , successCallbackFile
  , errorCallback
  ) where
-----------------------------------------------------------------------------
import Data.Aeson
import Miso.String
import Miso.FFI (syncCallback1, File)
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
-----------------------------------------------------------------------------
-- | runSupabase('auth','signUp', args, successCallback, errorCallback);
runSupabaseQuery
  :: ToJSVal args
  => MisoString
  -- ^ From
  -> MisoString
  -- ^ Method
  -> [args]
  -- ^ args
  -> Function
  -- ^ successful callback
  -> Function
  -- ^ errorful callback
  -> JSM ()
runSupabaseQuery from fnName args successful errorful = do
  args_ <- makeArgs args
  void $ jsg "globalThis" # "runSupabaseQuery" $
    (from, fnName, args_, successful, errorful)
-----------------------------------------------------------------------------
-- | runSupabase('auth','signUp', args, successCallback, errorCallback);
runSupabaseFrom
  :: ToJSVal args
  => MisoString
  -- ^ Namespace
  -> MisoString
  -- ^ From
  -> MisoString
  -- ^ Method
  -> [args]
  -- ^ args
  -> Function
  -- ^ successful callback
  -> Function
  -- ^ errorful callback
  -> JSM ()
runSupabaseFrom namespace from fnName args successful errorful = do
  args_ <- makeArgs args
  void $ jsg "globalThis" # "runSupabaseFrom" $
    (namespace, from, fnName, args_, successful, errorful)
-----------------------------------------------------------------------------
runSupabaseSelect
  :: ToJSVal args
  => MisoString
  -- ^ Table
  -> MisoString
  -- ^ Columns
  -> [args]
  -- ^ Filters and fetch options
  -> Function
  -- ^ successful callback
  -> Function
  -- ^ errorful callback
  -> JSM ()
runSupabaseSelect table columns args successful errorful = do
  args_ <- makeArgs args
  void $ jsg "globalThis" # "runSupabaseSelect" $
    (table, columns, args_, successful, errorful)
-----------------------------------------------------------------------------
runSupabaseUpdate
  :: ToJSVal args
  => MisoString
  -- ^ Table
  -> Value
  -- ^ Values
  -> [args]
  -- ^ Filters and update options
  -> Function
  -- ^ successful callback
  -> Function
  -- ^ errorful callback
  -> JSM ()
runSupabaseUpdate table values args successful errorful = do
  args_ <- makeArgs args
  void $ jsg "globalThis" # "runSupabaseUpdate" $
    (table, values, args_, successful, errorful)
-----------------------------------------------------------------------------
runSupabaseDelete
  :: ToJSVal args
  => MisoString
  -- ^ Table
  -> [args]
  -- ^ Filters and delete options
  -> Function
  -- ^ successful callback
  -> Function
  -- ^ errorful callback
  -> JSM ()
runSupabaseDelete table args successful errorful = do
  args_ <- makeArgs args
  void $ jsg "globalThis" # "runSupabaseDelete" $
    (table, args_, successful, errorful)
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
successCallbackFile
  :: (action -> JSM ())
  -> (MisoString -> action)
  -> (File -> action)
  -> JSM Function
successCallbackFile sink errorful successful = do
  syncCallback1 $ \result -> do
    fromJSValUnchecked result >>= sink . successful
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
