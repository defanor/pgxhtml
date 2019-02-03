-- pgcrypto is needed for UUIDs.
create extension pgcrypto;

-- A table with defaults and constraints.
create table bugs (
  id uuid not null primary key default gen_random_uuid(),
  reported timestamp with time zone not null default now(),
  reporter varchar not null default current_user,
  project varchar(256) not null check (project <> ''),
  description varchar(10240) not null check (description <> '')
);

-- No indexes, since they are irrelevant to this example (but normally
-- they should be there).

-- Additional restrictions.
create policy bugs_select_policy on bugs for select using (true);
create policy bugs_insert_policy on bugs for insert
  with check (reported = now() and reporter = current_user);
-- Update and delete policies can be added later.
alter table bugs enable row level security;

-- A search function for convenience.
create or replace function bug_search (proj varchar, descr varchar, lim int, offs int)
returns xml
as $$
  select query_to_xml(
    'select id, date_trunc(''second'', reported) as reported, reporter, '
    || 'project, substring(description from ''[^\n\r]+'') as summary from bugs '
    || 'where (project like ' || quote_literal('%' || proj || '%')
    || ') and (description like ' || quote_literal('%' || descr || '%')
    || ') order by reported desc limit ' || lim || ' offset ' || offs,
    false, false, 'urn:x-bugs')
$$ language sql;

-- Now users can be added with select and/or insert privileges,
-- including a guest user for unauthenticated requests.
