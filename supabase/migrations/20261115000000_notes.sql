-- The example app (app/Main.hs): notes, each user's own, with an attachment
-- in the attachments bucket.

create table public.notes (
  id         bigint generated always as identity primary key,
  user_id    uuid not null default auth.uid() references auth.users (id) on delete cascade,
  title      text not null check (char_length(title) between 1 and 200),
  done       boolean not null default false,
  -- the path of a file in the attachments bucket: <user id>/<name>
  attachment text,
  created_at timestamptz not null default now()
);

create index notes_user_id_created_at on public.notes (user_id, created_at desc);

-- Row level security: a user (anonymous ones too) sees and changes only
-- their own notes.
alter table public.notes enable row level security;

create policy "notes: read own" on public.notes
  for select to authenticated using ((select auth.uid()) = user_id);
create policy "notes: add own" on public.notes
  for insert to authenticated with check ((select auth.uid()) = user_id);
create policy "notes: change own" on public.notes
  for update to authenticated using ((select auth.uid()) = user_id) with check ((select auth.uid()) = user_id);
create policy "notes: delete own" on public.notes
  for delete to authenticated using ((select auth.uid()) = user_id);

-- A private bucket: files are read through signed URLs, and a user's files
-- are in the folder of their id.
insert into storage.buckets (id, name, public, file_size_limit)
values ('attachments', 'attachments', false, 5242880);

create policy "attachments: read own" on storage.objects
  for select to authenticated
  using (bucket_id = 'attachments' and (storage.foldername(name))[1] = (select auth.uid())::text);
create policy "attachments: add own" on storage.objects
  for insert to authenticated
  with check (bucket_id = 'attachments' and (storage.foldername(name))[1] = (select auth.uid())::text);
create policy "attachments: change own" on storage.objects
  for update to authenticated
  using (bucket_id = 'attachments' and (storage.foldername(name))[1] = (select auth.uid())::text);
create policy "attachments: delete own" on storage.objects
  for delete to authenticated
  using (bucket_id = 'attachments' and (storage.foldername(name))[1] = (select auth.uid())::text);
