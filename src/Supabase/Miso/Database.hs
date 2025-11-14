-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
module Supabase.Miso.Database
  ( -- * Functions
    select
  , selectWithFilters
  , insert
  , updateTable
  , upsert
  , deleteFrom
    -- * Types
  , Count (..)
  , FetchOptions (..)
  , InsertOptions (..)
  , UpdateOptions (..)
  , UpsertOptions (..) 
  , DeleteOptions (..)
  , Filter (..)
  , FilterOperator (..)
    -- * Filter builders
  , eq
  , neq
  , gt
  , gte
  , lt
  , lte
  , like
  , ilike
  , is
  , in_
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
data InsertOptions
  = InsertOptions
  { ioCount :: Maybe Count
  , ioDefaultToNull :: Maybe Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal InsertOptions where
  toJSVal InsertOptions {..} = do
    o <- create
    set "count" ioCount o
    set "defaultToNull" ioDefaultToNull o
    toJSVal o
-----------------------------------------------------------------------------
data UpdateOptions
  = UpdateOptions
  { uoCount :: Maybe Count
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal UpdateOptions where
  toJSVal UpdateOptions {..} = do
    o <- create
    set "count" uoCount o
    toJSVal o
-----------------------------------------------------------------------------
data UpsertOptions
  = UpsertOptions
  { upCount :: Maybe Count
  , upOnConflict :: Maybe MisoString  -- ^ Columns to use for conflict resolution
  , upIgnoreDuplicates :: Maybe Bool  -- ^ Skip rows with unique constraint violations
  , upDefaultToNull :: Maybe Bool     -- ^ Make missing columns null during upsert
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal UpsertOptions where
  toJSVal UpsertOptions {..} = do
    o <- create
    set "count" upCount o
    set "onConflict" upOnConflict o
    set "ignoreDuplicates" upIgnoreDuplicates o
    set "defaultToNull" upDefaultToNull o
    toJSVal o
-----------------------------------------------------------------------------
data DeleteOptions
  = DeleteOptions
  { doCount :: Maybe Count
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal DeleteOptions where
  toJSVal DeleteOptions {..} = do
    o <- create
    set "count" doCount o
    toJSVal o
-----------------------------------------------------------------------------
-- | Filter operators for building queries
data FilterOperator
  = Eq     -- ^ Equal to
  | Neq    -- ^ Not equal to
  | Gt     -- ^ Greater than
  | Gte    -- ^ Greater than or equal to
  | Lt     -- ^ Less than
  | Lte    -- ^ Less than or equal to
  | Like   -- ^ Pattern matching
  | ILike  -- ^ Case-insensitive pattern matching
  | Is     -- ^ Is (for null checks)
  | In     -- ^ In array
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal FilterOperator where
  toJSVal = \case
    Eq    -> toJSVal "eq"
    Neq   -> toJSVal "neq"
    Gt    -> toJSVal "gt"
    Gte   -> toJSVal "gte"
    Lt    -> toJSVal "lt"
    Lte   -> toJSVal "lte"
    Like  -> toJSVal "like"
    ILike -> toJSVal "ilike"
    Is    -> toJSVal "is"
    In    -> toJSVal "in"
-----------------------------------------------------------------------------
-- | Filter for building queries
-- Example: Filter "id" Eq (toJSON (1 :: Int))
data Filter
  = Filter
  { fColumn   :: MisoString
  , fOperator :: FilterOperator
  , fValue    :: Value
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal Filter where
  toJSVal (Filter col op val) = do
    o <- create
    set "column" col o
    opStr <- toJSVal op
    set "operator" opStr o
    val_ <- toJSVal val
    set "value" val_ o
    toJSVal o
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/select
-- Basic select without filters. For filtered queries, use 'selectWithFilters'
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
-- | https://supabase.com/docs/reference/javascript/insert
insert
  :: MisoString
  -- ^ Table name
  -> Value
  -- ^ Values to be inserted
  -> InsertOptions
  -- ^ Insert options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
insert table values insertOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  values_ <- toJSVal values
  insertOptions_ <- toJSVal insertOptions
  runSupabaseQuery table "insert" [values_, insertOptions_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/update
-- Update should always be combined with filters to target specific rows
-- Note: Filters must be applied in JavaScript, so we pass them as part of the effect
updateTable
  :: MisoString
  -- ^ Table name
  -> Value
  -- ^ Values to update
  -> [Filter]
  -- ^ Filters to target specific rows (will be applied sequentially)
  -> UpdateOptions
  -- ^ Update options
  -> (Value -> action)
  -- ^ Response (requires calling .select() on returned data to get updated rows)
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
updateTable table values filters updateOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  values_ <- toJSVal values
  updateOptions_ <- toJSVal updateOptions
  filters_ <- toJSVal filters
  runSupabaseUpdate table values [values_, filters_, updateOptions_] successful_ errorful_
  
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/select
-- Select with filters applied
selectWithFilters
  :: MisoString
  -- ^ Table name
  -> MisoString
  -- ^ Query string (columns to select)
  -> [Filter]
  -- ^ Filters to apply
  -> FetchOptions
  -- ^ Fetch options
  -> (Value -> action)
  -- ^ Response
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
selectWithFilters table args filters fetchOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  filters_ <- toJSVal filters
  fetchOptions_ <- toJSVal fetchOptions
  runSupabaseSelect table args [filters_, fetchOptions_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/upsert
-- Upsert performs an INSERT if the row doesn't exist, or UPDATE if it does
-- Primary keys should be included in the values
upsert
  :: MisoString
  -- ^ Table name
  -> Value
  -- ^ Values to be upserted (must include primary keys)
  -> UpsertOptions
  -- ^ Upsert options
  -> (Value -> action)
  -- ^ Response (use .select() to return data)
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
upsert table values upsertOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  values_ <- toJSVal values
  upsertOptions_ <- toJSVal upsertOptions
  runSupabaseQuery table "upsert" [values_, upsertOptions_] successful_ errorful_
-----------------------------------------------------------------------------
-- | https://supabase.com/docs/reference/javascript/delete
-- Delete should always be combined with filters to target specific rows
-- By default, deleted rows are not returned. Use .select() in response to return them.
deleteFrom
  :: MisoString
  -- ^ Table name
  -> [Filter]
  -- ^ Filters to target specific rows for deletion
  -> DeleteOptions
  -- ^ Delete options
  -> (Value -> action)
  -- ^ Response (use .select() to return deleted rows)
  -> (MisoString -> action)
  -- ^ Error case
  -> Effect parent model action
deleteFrom table filters deleteOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  filters_ <- toJSVal filters
  deleteOptions_ <- toJSVal deleteOptions
  runSupabaseDelete table [filters_, deleteOptions_] successful_ errorful_
-----------------------------------------------------------------------------
-- * Filter builder helpers
-----------------------------------------------------------------------------
-- | Equal to filter
eq :: ToJSON a => MisoString -> a -> Filter
eq col val = Filter col Eq (toJSON val)
-----------------------------------------------------------------------------
-- | Not equal to filter
neq :: ToJSON a => MisoString -> a -> Filter
neq col val = Filter col Neq (toJSON val)
-----------------------------------------------------------------------------
-- | Greater than filter
gt :: ToJSON a => MisoString -> a -> Filter
gt col val = Filter col Gt (toJSON val)
-----------------------------------------------------------------------------
-- | Greater than or equal to filter
gte :: ToJSON a => MisoString -> a -> Filter
gte col val = Filter col Gte (toJSON val)
-----------------------------------------------------------------------------
-- | Less than filter
lt :: ToJSON a => MisoString -> a -> Filter
lt col val = Filter col Lt (toJSON val)
-----------------------------------------------------------------------------
-- | Less than or equal to filter
lte :: ToJSON a => MisoString -> a -> Filter
lte col val = Filter col Lte (toJSON val)
-----------------------------------------------------------------------------
-- | Like pattern matching filter
like :: MisoString -> MisoString -> Filter
like col pattern = Filter col Like (toJSON pattern)
-----------------------------------------------------------------------------
-- | Case-insensitive like pattern matching filter
ilike :: MisoString -> MisoString -> Filter
ilike col pattern = Filter col ILike (toJSON pattern)
-----------------------------------------------------------------------------
-- | Is (for null/boolean checks)
is :: ToJSON a => MisoString -> a -> Filter
is col val = Filter col Is (toJSON val)
-----------------------------------------------------------------------------
-- | In array filter
in_ :: ToJSON a => MisoString -> [a] -> Filter
in_ col vals = Filter col In (toJSON vals)
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------