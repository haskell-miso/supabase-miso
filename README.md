⚡ :ramen: supabase-miso
=======================

[![Made with Supabase](https://supabase.com/badge-made-with-supabase.svg)](https://supabase.com)

A [Haskell](https://haskell.org) [miso](https://haskell-miso.org) client library for [supabase](https://supabase.com). This is a [Haskell](https://haskell.org) front-end library, meant for use with the JS, or [Web Assembly](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) backend with GHC.

> [!WARNING]
> This is a work in-progress, many things are not implemented. Please consider contributing.

### Usage

For local development with [miso](https://github.com/dmjio/miso) you can include [supabase](https://github.com/supabase) as a JavaScript module.

```haskell
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main (main) where
-----------------------------------------------------------------------------
import Supabase.Miso.Auth
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startApp myComponent
  { scripts =
     [ Module
        """
        import { createClient } from 'https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm'
        const supabase = createClient('https://<app-id>.supabase.co', '<anon-key>');
        globalThis['supabase'] = supabase;
        console.log('Supabase Instance: ', supabase)
        """
     ]
  }
-----------------------------------------------------------------------------
data Action
  = SuccessfullyRegistered AuthResponse
  | ErrorRegistering MisoString
  | NewUserWithEmail SignUpEmail
  | NewUserWithPhone SignUpPhone
  deriving (Show, Eq)
-----------------------------------------------------------------------------
update :: Action -> Effect parent model Action
update = \case
  SuccessfullyRegistered response ->
    io_ $ do 
      consoleLog "success"
      consoleLog $ ms (show response)
  ErrorRegistering errorMessage ->
    io_ (consoleError errorMessage)
  NewUserWithEmail email ->
    signUpEmail email SuccessfullyRegistered ErrorRegistering
  NewUserWithPhone phone ->
    signUpPhone email SuccessfullyRegistered ErrorRegistering
-----------------------------------------------------------------------------
```

### Build

```bash
$ cabal build
```

### API Coverage

#### Auth

  - [x] [Create a new user](https://supabase.com/docs/reference/javascript/auth-signup)
  - [ ] [Listen to auth events](https://supabase.com/docs/reference/javascript/auth-onauthstatechange)
  - [ ] [Create an anonymous user](https://supabase.com/docs/reference/javascript/auth-signinanonymously)
  - [ ] [Sign in a user](https://supabase.com/docs/reference/javascript/auth-signinwithpassword)
  - [ ] [Sign in with ID Token](https://supabase.com/docs/reference/javascript/auth-signinwithidtoken)
  - [ ] [Sign in a user through OTP](https://supabase.com/docs/reference/javascript/auth-signinwithotp)
  - [ ] [Sign in a user through OAuth](https://supabase.com/docs/reference/javascript/auth-signinwithoauth)
  - [ ] [Sign in a user through SSO](https://supabase.com/docs/reference/javascript/auth-signinwithsso)
  - [ ] [Get user claims from verified JWT](https://supabase.com/docs/reference/javascript/auth-getclaims)
  - [ ] [Sign out a user](https://supabase.com/docs/reference/javascript/auth-signout)
  - [ ] [Send a password reset request](https://supabase.com/docs/reference/javascript/auth-resetpasswordforemail)
  - [ ] [Verify and log in through OTP](https://supabase.com/docs/reference/javascript/auth-verifyotp)
  - [ ] [Retrieve a session](https://supabase.com/docs/reference/javascript/auth-getsession)
  - [ ] [Retrieve a new session](https://supabase.com/docs/reference/javascript/auth-refreshsession)
  - [ ] [Retrieve a user](https://supabase.com/docs/reference/javascript/auth-getuser)
  - [ ] [Update a user](https://supabase.com/docs/reference/javascript/auth-getuser)
  - [ ] [Retrieve identities linked to a user](https://supabase.com/docs/reference/javascript/auth-getuseridentities)
  - [ ] [Link an identity to a user](https://supabase.com/docs/reference/javascript/auth-linkidentity)
  - [ ] [Unlink an identity from a user](https://supabase.com/docs/reference/javascript/auth-unlinkidentity)
  - [ ] [Send a password reauthentication nonce](https://supabase.com/docs/reference/javascript/auth-reauthentication)
  - [ ] [Resend an OTP](https://supabase.com/docs/reference/javascript/auth-resend)
  - [ ] [Set the session data](https://supabase.com/docs/reference/javascript/auth-setsession)
  - [ ] [Exchange an auth code for a session](https://supabase.com/docs/reference/javascript/auth-exchangecodeforsession)
  - [ ] [Start auto-refresh session (non-browser)](https://supabase.com/docs/reference/javascript/auth-startautorefresh)
  - [ ] [Stop auto-refresh session (non-browser)](https://supabase.com/docs/reference/javascript/auth-stopautorefresh)

#### Realtime

  - [ ] [Subscribe to channel](https://supabase.com/docs/reference/javascript/subscribe)
  - [ ] [Unsubscribe from a channel](https://supabase.com/docs/reference/javascript/removechannel)
  - [ ] [Unsubscribe from all channels](https://supabase.com/docs/reference/javascript/removeallchannels)
  - [ ] [Retrieve all channels](https://supabase.com/docs/reference/javascript/getchannels)
  - [ ] [Broadcast a message](https://supabase.com/docs/reference/javascript/broadcastmessage)

#### Edge

  - [ ] [Invoke a Supabase Edge Function](https://supabase.com/docs/reference/javascript/functions-invoke)

#### Storage

  - [x] [Create a bucket](https://supabase.com/docs/reference/javascript/storage-createbucket)
  - [x] [Retrieve a bucket](https://supabase.com/docs/reference/javascript/storage-getbucket)
  - [x] [List all buckets](https://supabase.com/docs/reference/javascript/storage-listbuckets)
  - [x] [Update a bucket](https://supabase.com/docs/reference/javascript/storage-updatebucket)
  - [x] [Delete a bucket](https://supabase.com/docs/reference/javascript/storage-deletebucket)
  - [x] [Empty a bucket](https://supabase.com/docs/reference/javascript/storage-emptybucket)
  - [x] [Upload a file](https://supabase.com/docs/reference/javascript/storage-from-upload)
  - [x] [Download a file](https://supabase.com/docs/reference/javascript/storage-from-download)
  - [x] [List all files in a bucket](https://supabase.com/docs/reference/javascript/storage-from-list)
  - [x] [Replace an existing file](https://supabase.com/docs/reference/javascript/storage-from-update)
  - [x] [Move an existing file](https://supabase.com/docs/reference/javascript/storage-from-move)
  - [x] [Copy an existing file](https://supabase.com/docs/reference/javascript/storage-from-copy)
  - [x] [Delete files in a bucket](https://supabase.com/docs/reference/javascript/storage-from-remove)
  - [x] [Create a signed URL](https://supabase.com/docs/reference/javascript/storage-from-createsignedurl)
  - [x] [Create signed URLs](https://supabase.com/docs/reference/javascript/storage-from-createsignedurls)
  - [x] [Create signed upload URL](https://supabase.com/docs/reference/javascript/storage-from-createsigneduploadurl)
  - [x] [Upload to a signed URL](https://supabase.com/docs/reference/javascript/storage-from-uploadtosignedurl)
  - [x] [Retrieve public URL](https://supabase.com/docs/reference/javascript/storage-from-getpublicurl)

### Database

  - [x] [Fetch data](https://supabase.com/docs/reference/javascript/select)
  - [ ] [Insert data](https://supabase.com/docs/reference/javascript/insert)
  - [ ] [Update data](https://supabase.com/docs/reference/javascript/update)
  - [ ] [Upsert data](https://supabase.com/docs/reference/javascript/upsert)
  - [ ] [Delete data](https://supabase.com/docs/reference/javascript/delete)
  - [ ] [Call a Postgres function](https://supabase.com/docs/reference/javascript/rpc)
