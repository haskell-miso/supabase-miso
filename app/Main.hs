-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultilineStrings    #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Miso
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents (component () noop (\_props () -> "foo"))
#ifndef WASM
  { scripts =
    [ Module 
       """
       import { createClient } from 'https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm'
       const supabase = createClient('https://bufjmcerlanfijatbwxu.supabase.co', 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.kyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJ1ZmptY2VybGFuZmlqYXRid3h1Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NTUzMTgwNzgsImV4cCI6MjA3MDg5NDA3OH0.Cv8xhZyZTQuLVphhQ-fxbf6C4zTzu85I7leNyTMggKU')
       globalThis['supabase'] = supabase;
       console.log('Supabase Instance: ', supabase)
       """
    ]
  }
#endif
-----------------------------------------------------------------------------
