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
  , authStateChangeCallback
  , subscriptionCallback
  ) where
-----------------------------------------------------------------------------
import Miso.JSON
import Miso.String
import Miso.DSL
import Miso.FFI (syncCallback1, File)
-----------------------------------------------------------------------------
import Control.Monad
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
  -> IO ()
runSupabase namespace fnName args successful errorful = do
  args_ <- toArgs args
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
  -> IO ()
runSupabaseQuery from fnName args successful errorful = do
  args_ <- toArgs args
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
  -> IO ()
runSupabaseFrom namespace from fnName args successful errorful = do
  args_ <- toArgs args
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
  -> IO ()
runSupabaseSelect table columns args successful errorful = do
  args_ <- toArgs args
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
  -> IO ()
runSupabaseUpdate table values args successful errorful = do
  args_ <- toArgs args
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
  -> IO ()
runSupabaseDelete table args successful errorful = do
  args_ <- toArgs args
  void $ jsg "globalThis" # "runSupabaseDelete" $
    (table, args_, successful, errorful)
-----------------------------------------------------------------------------
emptyArgs :: [JSVal]
emptyArgs = []
-----------------------------------------------------------------------------
successCallback
  :: FromJSON t
  => (action -> IO ())
  -> (MisoString -> action)
  -> (t -> action)
  -> IO Function
successCallback sink errorful successful =
  Function <$> (syncCallback1 $ \result ->
    fromJSON <$> fromJSValUnchecked result >>= \case
      Error msg ->
        sink $ errorful (ms msg)
      Success result ->
        sink (successful result))
-----------------------------------------------------------------------------
authStateChangeCallback
  :: (action -> IO ())
  -> (MisoString -> Maybe Value -> action)
  -> IO Function
authStateChangeCallback sink callback = do
  Function <$> (syncCallback1 $ \args -> do
    event <- fromJSValUnchecked =<< (args ! "0")
    session <- fromJSValUnchecked =<< (args ! "1")
    sink (callback event session))
-----------------------------------------------------------------------------
subscriptionCallback
  :: (action -> IO ())
  -> (IO () -> action)
  -> IO Function
subscriptionCallback sink makeAction = do
  Function <$> (syncCallback1 $ \result -> do
    -- Extract the unsubscribe function from the subscription object
    unsubscribeFn <- result ! "unsubscribe"
    let unsubscribeAction = void $ call unsubscribeFn result ([] :: [JSVal])
    sink (makeAction unsubscribeAction))
-----------------------------------------------------------------------------
successCallbackFile
  :: (action -> IO ())
  -> (MisoString -> action)
  -> (File -> action)
  -> IO Function
successCallbackFile sink errorful successful =
  Function <$> (syncCallback1 $ \result ->
    fromJSValUnchecked result >>= sink . successful)
-----------------------------------------------------------------------------
errorCallback
  :: (action -> IO ())
  -> (MisoString -> action)
  -> IO Function
errorCallback sink errorful =
  Function <$> (syncCallback1 $ \result -> do
    fromJSON <$> fromJSValUnchecked result >>= \case
      Error msg -> do
        sink $ errorful (ms msg)
      Success result ->
        sink (errorful result))
-----------------------------------------------------------------------------
