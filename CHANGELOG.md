# Revision history for supabase-miso

## Unreleased

Fixes, checked against supabase-js 2.117.3:

* Every call reports back: the success callback gets the data also when it
  is null (insert or upsert without `.select()`, update, delete, signOut),
  and the error callback gets the error's message, also when the request
  fails (a rejected promise) or the call throws.
* `onAuthStateChange` and `getPublicUrl` no longer throw (their results are
  not promises).  The auth listener gets the event and the session, and the
  subscription's `unsubscribe` works.
* `updateTable` passes its values, filters and options as the glue reads
  them (they did not match).
* `selectWithFilters` and `deleteFrom` pass `count` and `head` as options of
  `select` and `delete` (there is no `query.count()` or `query.head()`).
* Options left as `Nothing` are left out, not sent as null: `upsert` sent
  `on_conflict=null`, and `defaultToNull` changed.
* `AuthResponse` reads what the success callback gets, `{ user, session }`
  (it expected `{ data, error }`, so signing up or in never decoded).
  `adSession` is a `Maybe Session` (no session until the email is
  confirmed), optional user fields may be missing, and timestamps are ISO
  8601 (`2026-10-13T12:00:00.123456Z`).  `identityLastSignInAt` is a
  `Maybe UTCTime`.
* `ToJSON Identity` wrote its `id` as `identity_id`.
* Exported `signUpPhone`, `Session`, `IdentityData`, `Email`, `Phone` and
  `Password`.
* `js/supabase-miso.js` is a `js-sources` of the library, not only of the
  example app.
* `Supabase.Miso.Core.runSupabaseUpdate` takes the table and
  `[values, filters, options]`.
* The README and example import supabase-js 2.117.3, not the latest.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
