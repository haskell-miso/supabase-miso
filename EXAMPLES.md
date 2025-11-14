# Supabase-Miso Database Examples

This document provides examples of how to use the database functions in `supabase-miso`.

## Table of Contents

- [Update Function](#update-function)
- [Upsert Function](#upsert-function)
- [Delete Function](#delete-function)
- [Filters](#filters)
- [Filter Builder Functions](#filter-builder-functions)
- [Select with Filters](#select-with-filters)

## Update Function

The `update` function allows you to modify existing rows in a table. It requires filters to target specific rows.

### Basic Update Example

```haskell
import Supabase.Miso.Database
import Data.Aeson (object, (.=))

-- Update a user's name where id equals 1
updateUserName :: Effect parent model Action
updateUserName = update
  "users"                                    -- Table name
  (object ["name" .= ("John Doe" :: String)])  -- Values to update
  [eq "id" (1 :: Int)]                       -- Filters
  (UpdateOptions Nothing)                    -- Options
  UserUpdateSuccess                          -- Success callback
  UserUpdateError                            -- Error callback
```

### Update with Multiple Filters

```haskell
-- Update all active users with age greater than 18
updateAdultUsers :: Effect parent model Action
updateAdultUsers = update
  "users"
  (object ["verified" .= True])
  [ eq "active" True
  , gt "age" (18 :: Int)
  ]
  (UpdateOptions Nothing)
  UserUpdateSuccess
  UserUpdateError
```

### Update with Count Option

```haskell
-- Update and get count of affected rows
updateWithCount :: Effect parent model Action
updateWithCount = update
  "users"
  (object ["last_login" .= ("2024-01-01" :: String)])
  [eq "id" (42 :: Int)]
  (UpdateOptions (Just Exact))  -- Return exact count
  UserUpdateSuccess
  UserUpdateError
```

## Upsert Function

The `upsert` function performs an INSERT if the row doesn't exist, or UPDATE if it does. Primary keys must be included in the values.

### Basic Upsert Example

```haskell
import Supabase.Miso.Database
import Data.Aeson (object, (.=))

-- Upsert a single user (insert or update based on primary key)
upsertUser :: Effect parent model Action
upsertUser = upsert
  "users"                                    -- Table name
  (object ["id" .= (1 :: Int), "name" .= ("John Doe" :: String)])  -- Values (must include primary key)
  (UpsertOptions Nothing Nothing Nothing Nothing)  -- Options
  UserUpsertSuccess                          -- Success callback
  UserUpsertError                            -- Error callback
```

### Upsert with Conflict Resolution

```haskell
-- Specify which column to use for conflict resolution
upsertUserByEmail :: Effect parent model Action
upsertUserByEmail = upsert
  "users"
  (object ["email" .= ("user@example.com" :: String), "name" .= ("John" :: String)])
  (UpsertOptions
    { upCount = Just Exact
    , upOnConflict = Just "email"  -- Use email for conflict detection
    , upIgnoreDuplicates = Nothing
    , upDefaultToNull = Nothing
    })
  UserUpsertSuccess
  UserUpsertError
```

### Bulk Upsert

```haskell
-- Upsert multiple rows at once
bulkUpsertProducts :: Effect parent model Action
bulkUpsertProducts = upsert
  "products"
  (toJSON [ object ["id" .= (1 :: Int), "name" .= ("Product A" :: String)]
          , object ["id" .= (2 :: Int), "name" .= ("Product B" :: String)]
          ])
  (UpsertOptions Nothing Nothing Nothing Nothing)
  ProductsUpsertSuccess
  ProductsUpsertError
```

### Upsert with Duplicate Handling

```haskell
-- Ignore rows that would violate unique constraints
upsertIgnoreDuplicates :: Effect parent model Action
upsertIgnoreDuplicates = upsert
  "users"
  (object ["email" .= ("user@example.com" :: String), "name" .= ("John" :: String)])
  (UpsertOptions
    { upCount = Nothing
    , upOnConflict = Nothing
    , upIgnoreDuplicates = Just True  -- Skip duplicates instead of failing
    , upDefaultToNull = Nothing
    })
  UserUpsertSuccess
  UserUpsertError
```

## Delete Function

The `deleteFrom` function removes rows from a table. Always use filters to target specific rows.

### Basic Delete Example

```haskell
import Supabase.Miso.Database
import Data.Aeson (object, (.=))

-- Delete a single user by ID
deleteUser :: Int -> Effect parent model Action
deleteUser uid = deleteFrom
  "users"                          -- Table name
  [eq "id" uid]                    -- Filters
  (DeleteOptions Nothing)          -- Options
  UserDeleteSuccess                -- Success callback
  UserDeleteError                  -- Error callback
```

### Delete with Multiple Filters

```haskell
-- Delete all inactive users older than a certain date
deleteInactiveUsers :: Effect parent model Action
deleteInactiveUsers = deleteFrom
  "users"
  [ eq "status" ("inactive" :: String)
  , lt "last_login" ("2023-01-01" :: String)
  ]
  (DeleteOptions Nothing)
  UsersDeleteSuccess
  UsersDeleteError
```

### Delete with Count

```haskell
-- Delete and get count of deleted rows
deleteWithCount :: Effect parent model Action
deleteWithCount = deleteFrom
  "users"
  [eq "status" ("deleted" :: String)]
  (DeleteOptions (Just Exact))
  (\result -> case result of
    Object obj -> case H.lookup "count" obj of
      Just (Number n) -> LogDeletedCount (floor n)
      _ -> LogDeletedRows result
    _ -> LogDeletedRows result)
  DeleteError
```

### Delete Multiple Rows by IDs

```haskell
-- Delete users by their IDs
deleteUsersByIds :: [Int] -> Effect parent model Action
deleteUsersByIds ids = deleteFrom
  "users"
  [in_ "id" ids]
  (DeleteOptions Nothing)
  UsersDeleteSuccess
  UsersDeleteError
```

## Filters

Filters are built using the `Filter` data type, which consists of:

- `fColumn`: The column name to filter on
- `fOperator`: The comparison operator
- `fValue`: The value to compare against

### Filter Operators

The library provides the following operators:

- `Eq` - Equal to
- `Neq` - Not equal to
- `Gt` - Greater than
- `Gte` - Greater than or equal to
- `Lt` - Less than
- `Lte` - Less than or equal to
- `Like` - Pattern matching
- `ILike` - Case-insensitive pattern matching
- `Is` - For null/boolean checks
- `In` - In array

### Manual Filter Construction

```haskell
import Data.Aeson (toJSON)

-- Manually construct a filter
myFilter :: Filter
myFilter = Filter
  { fColumn = "email"
  , fOperator = Like
  , fValue = toJSON ("%@example.com" :: String)
  }
```

## Filter Builder Functions

For convenience, the library provides helper functions to create filters more easily:

### Equality Filters

```haskell
-- Equal to
eq "status" ("active" :: String)
eq "age" (25 :: Int)

-- Not equal to
neq "role" ("admin" :: String)
neq "count" (0 :: Int)
```

### Comparison Filters

```haskell
-- Greater than
gt "score" (100 :: Int)
gt "price" (99.99 :: Double)

-- Greater than or equal to
gte "age" (18 :: Int)

-- Less than
lt "stock" (10 :: Int)

-- Less than or equal to
lte "attempts" (3 :: Int)
```

### Pattern Matching

```haskell
-- Case-sensitive pattern matching
like "email" "%@gmail.com"

-- Case-insensitive pattern matching
ilike "name" "%john%"
```

### Null and Boolean Checks

```haskell
-- Check for null
is "deleted_at" Null

-- Check boolean values
is "active" True
```

### Array Membership

```haskell
-- Check if value is in array
in_ "status" ["pending", "active", "completed"]
in_ "id" [1, 2, 3, 4, 5]
```

## Select with Filters

The `selectWithFilters` function allows you to query data with filters applied.

### Basic Select with Filter

```haskell
-- Select all columns for users with a specific email
selectUserByEmail :: Effect parent model Action
selectUserByEmail = selectWithFilters
  "users"                          -- Table name
  "*"                              -- Select all columns
  [eq "email" "user@example.com"]  -- Filters
  (FetchOptions Nothing Nothing)   -- Options
  UsersFetched                     -- Success callback
  FetchError                       -- Error callback
```

### Select Specific Columns with Multiple Filters

```haskell
-- Select name and email for active adult users
selectActiveAdults :: Effect parent model Action
selectActiveAdults = selectWithFilters
  "users"
  "name,email,age"
  [ eq "active" True
  , gte "age" (18 :: Int)
  ]
  (FetchOptions Nothing Nothing)
  UsersFetched
  FetchError
```

### Select with Count

```haskell
-- Get users with count
selectWithCount :: Effect parent model Action
selectWithCount = selectWithFilters
  "users"
  "*"
  [eq "verified" True]
  (FetchOptions (Just Exact) Nothing)  -- Return exact count
  UsersFetched
  FetchError
```

### Complex Query Example

```haskell
-- Find premium users who have logged in recently
selectPremiumRecentUsers :: Effect parent model Action
selectPremiumRecentUsers = selectWithFilters
  "users"
  "id,name,email,subscription_tier,last_login"
  [ eq "subscription_tier" ("premium" :: String)
  , gt "last_login" ("2024-01-01" :: String)
  , neq "status" ("suspended" :: String)
  ]
  (FetchOptions (Just Planned) Nothing)
  PremiumUsersFetched
  FetchError
```

## Complete Example with Model and Update

Here's a complete example showing how to integrate these functions into a Miso application:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Miso
import Miso.String
import Data.Aeson
import Supabase.Miso.Database

-- Model
data Model = Model
  { users :: [User]
  , error :: Maybe MisoString
  } deriving (Eq, Show)

data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  } deriving (Eq, Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "email"

-- Actions
data Action
  = FetchUsers
  | UsersFetched Value
  | UpdateUser Int String
  | UserUpdated Value
  | UpsertUser Int String
  | UserUpserted Value
  | DeleteUser Int
  | UserDeleted Value
  | HandleError MisoString
  | NoOp

-- Update
updateModel :: Action -> Model -> Effect Action Model
updateModel action model@Model{..} = case action of
  FetchUsers -> model <# fetchAllUsers

  UsersFetched value -> case fromJSON value of
    Success users' -> noEff model { users = users' }
    Error msg -> noEff model { error = Just (ms msg) }

  UpdateUser uid newName -> model <# updateUserName uid newName

  UserUpdated _ -> model <# fetchAllUsers  -- Refresh the list

  UpsertUser uid newName -> model <# upsertUserName uid newName

  UserUpserted _ -> model <# fetchAllUsers  -- Refresh the list

  DeleteUser uid -> model <# deleteUserById uid

  UserDeleted _ -> model <# fetchAllUsers  -- Refresh the list

  HandleError err -> noEff model { error = Just err }

  NoOp -> noEff model

-- Effects
fetchAllUsers :: Effect Action Model
fetchAllUsers = selectWithFilters
  "users"
  "*"
  [eq "active" True]
  (FetchOptions Nothing Nothing)
  UsersFetched
  HandleError

updateUserName :: Int -> String -> Effect Action Model
updateUserName uid newName = updateTable
  "users"
  (object ["name" .= newName])
  [eq "id" uid]
  (UpdateOptions Nothing)
  UserUpdated
  HandleError

upsertUserName :: Int -> String -> Effect Action Model
upsertUserName uid newName = upsert
  "users"
  (object ["id" .= uid, "name" .= newName])
  (UpsertOptions Nothing Nothing Nothing Nothing)
  UserUpserted
  HandleError

deleteUserById :: Int -> Effect Action Model
deleteUserById uid = deleteFrom
  "users"
  [eq "id" uid]
  (DeleteOptions Nothing)
  UserDeleted
  HandleError
```

## Notes

1. **Filter Chaining**: Filters are applied sequentially in the order they appear in the list.

2. **Type Safety**: The filter builder functions use `ToJSON` constraint, allowing you to pass any serializable value while maintaining type safety.

3. **Error Handling**: Always provide both success and error callbacks. The error callback receives a `MisoString` with the error message.

4. **Options**: All database operations support options like `Count`, which can be used to return the count of affected/fetched rows.

5. **JavaScript Integration**: The filters are converted to JavaScript and applied using the Supabase JavaScript client's filter methods (`eq`, `gt`, `in`, etc.).

6. **Upsert Primary Keys**: When using `upsert`, you must include the primary key in the values object. If the primary key exists, the row will be updated; otherwise, a new row will be inserted.

7. **Delete Filters**: Always use filters with `deleteFrom` to target specific rows. Using `deleteFrom` without filters would attempt to delete all rows in the table (subject to RLS policies).

8. **Returning Data**: By default, `update`, `upsert`, and `delete` operations don't return the affected rows. To return data, you need to chain `.select()` in the response handling (typically done in the JavaScript layer or by modifying the returned Value in your success callback).
