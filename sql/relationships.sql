begin;
create schema if not exists temporal_relationships;
grant usage on schema temporal_relationships to public;
set local search_path to temporal_relationships, public;
-- create a domain if not exists 
DO $d$
DECLARE
  domain_range_name text default 'timeperiod';
  domain_range_type text default 'tstzrange';
  domain_i_name text default 'time_endpoint';
  domain_i_type text default 'timestamptz';
BEGIN
-- Create timeperiod domain
PERFORM n.nspname as "Schema",
        t.typname as "Name",
        pg_catalog.format_type(t.typbasetype, t.typtypmod) as "Type"
FROM pg_catalog.pg_type t
      LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
WHERE t.typtype = 'd'
       AND n.nspname <> 'pg_catalog'
       AND n.nspname <> 'information_schema'
       AND pg_catalog.pg_type_is_visible(t.oid)
   AND t.typname = domain_range_name;
   if FOUND then
     raise NOTICE 'Domain % already exists', domain_range_name;
   else
     execute format('create domain %I as %I', domain_range_name, domain_range_type);
   end if;
-- Create time_endpoint domain
PERFORM n.nspname as "Schema",
        t.typname as "Name",
        pg_catalog.format_type(t.typbasetype, t.typtypmod) as "Type"
FROM pg_catalog.pg_type t
      LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
WHERE t.typtype = 'd'
       AND n.nspname <> 'pg_catalog'
       AND n.nspname <> 'information_schema'
       AND pg_catalog.pg_type_is_visible(t.oid)
   AND t.typname = domain_i_name;
   if FOUND then
     raise NOTICE 'Domain % already exists', domain_i_name;
   else
     execute format('create domain %I as %I', domain_i_name, domain_i_type);
   end if;
END;
$d$;
create or replace
function timeperiod( p_range_start time_endpoint, p_range_end time_endpoint)
RETURNS timeperiod
language sql IMMUTABLE
as
$func$
   select tstzrange(p_range_start, p_range_end,'[)')::timeperiod;
$func$
SET search_path = 'temporal_relationships';
-- backwards compatible
create or replace
function timeperiod_range( _s time_endpoint, _e time_endpoint, _ignored text)
returns timeperiod
language sql
as
$func$
   select timeperiod(_s,_e);
$func$
SET search_path = 'temporal_relationships';
create or replace 
function xor(a boolean, b boolean) returns boolean
language sql IMMUTABLE
as 
$$ select  ( (not a) <> (not b)); $$;

create or replace 
function fst( x anyrange ) returns anyelement
language SQL IMMUTABLE 
as
$$ select lower(x); $$;

create or replace
function snd( x anyrange ) returns anyelement
language SQL IMMUTABLE 
as
$$ select upper(x); $$;
--
-- [starts] [starts^-1]
--
-- [starts A E]
--  A  |---|
--  E  |-------|
--
-- [starts^-1 A E]
--  A  |-------|
--  E  |---|
--
create or replace
function has_starts(a timeperiod , b timeperiod )
returns boolean language SQL IMMUTABLE 
as $$
  select fst(a) = fst(b) and snd(a) <> snd(b);
$$
SET search_path = 'temporal_relationships';
--
-- [finishes] [finishes^-1]
--
-- [finishes A E]
--  A  |-------|
--  E      |---|
--
-- [finishes^-1 A E]
--  A      |---|
--  E  |-------|
--
create or replace
function has_finishes(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select snd(a) = snd(b) and fst(a) <> fst(b);
$$
SET search_path = 'temporal_relationships';
--
-- [equals]
--
-- [equals A E]
--  A  |----|
--  E  |----|
--
create or replace
function equals(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  -- doubtful = operator exists for timeperiod
 select fst(a) = fst(b) and snd(a) = snd(b) ;
$$
SET search_path = 'temporal_relationships';
--
-- [during]
--
-- [during A E]
--  A    |---|
--  E  |-------|
--
create or replace
function is_during(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select (fst(a) > fst(b)) and (snd(a) < snd(b));
$$
SET search_path = 'temporal_relationships';
--
-- [during^-1] contained
--
-- [during^-1 A E]
--  A  |-------|
--  E    |---|
--
create or replace
function is_contained_in(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select is_during(b, a);
$$
SET search_path = 'temporal_relationships';

--
-- [during] or [during^-1] 
--
create or replace
function has_during(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select is_during(a, b) or is_during(b,a);
$$
SET search_path = 'temporal_relationships';
--
-- [overlaps]
--
-- [overlaps A E]
--  A  |-----|
--  E     |-----|
--
-- [overlaps^-1 A E]
--  A     |-----|
--  E  |-----|
--
create or replace
function is_overlaps(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 

as $$
  select  fst(a) < fst(b) and snd(a) > fst(b) and snd(a) < snd(b);
$$
SET search_path = 'temporal_relationships';

--
-- either overlaps the other [overlaps] [overlaps^-1]
--
create or replace
function has_overlaps(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select  is_overlaps(a , b ) or is_overlaps(b , a ) ;
$$
SET search_path = 'temporal_relationships';
--
-- [before]
--
-- [before A E]
--  A  |-----|
--  E           |-----|
--
create or replace
function is_before(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select  snd(a) < fst(b);
$$
SET search_path = 'temporal_relationships';
--
-- [before^-1]
--
-- [before^-1 A E]
--  A           |-----|
--  E   |-----|
--
create or replace
function is_after(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
    -- is_before(b, a)
   select snd(b) < fst(a);
$$
SET search_path = 'temporal_relationships';

-- 
-- either [before] [before^-1]
-- 
create or replace
function has_before(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select  snd(a) < fst(b) or snd(b) < fst(a);
$$
SET search_path = 'temporal_relationships';
--
-- [meets] [meets^-1]
--
-- no shared time tick.
--
-- [meets A E]
--  A   |-----|
--  E         |-----|
--
-- [meets^-1 A E]
--  A         |-----|
--  E   |-----|
--
create or replace
function is_meets(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
 select  snd(a) = fst(b) ;
$$
SET search_path = 'temporal_relationships';

create or replace
function has_meets(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select snd(a) = fst(b) or snd(b) = fst(a);
$$
SET search_path = 'temporal_relationships';
-- 
-- Partition of Allen Relationships
--

-- 
-- [Includes] 
--     [Contains] or [Overlaps]
create or replace
function has_includes(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select  fst(a) = fst(b) or snd(a) = snd(b) or 
      (snd(a) <= snd(b) and (fst(a) >= fst(b) or fst(b) < snd(a))) or 
        (snd(a) >= snd(b) and (fst(a) < snd(b) or fst(a) <= fst(b)));
$$
SET search_path = 'temporal_relationships';

--
-- [Contains]
--    [Encloses] or [Equals]

create or replace
function has_contains(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 

as $$
 select fst(a) = fst(b) or snd(a) = snd(b) or 
     (snd(a) < snd(b) and fst(a) > fst(b)) or 
       (snd(b) < snd(a) and fst(b) > fst(a));
$$
SET search_path = 'temporal_relationships';

--
-- [Aligns With]
--   [Starts] or [Finishes]
--
create or replace
function has_aligns_with(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
   select   xor( fst(a) = fst(b) , snd(a) = snd(b) );
$$
SET search_path = 'temporal_relationships';

--
-- [Encloses]
--   [Aligns With] or [During]
-- 

create or replace
function has_encloses(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select has_during(a,b) or has_aligns_with(a,b);
$$
SET search_path = 'temporal_relationships';


--
-- [Excludes]
--   [Before] or [Meets]
--
create or replace
function has_excludes(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
   select fst(a) >= snd(b) or fst(b) >= snd(a) ;
$$
SET search_path = 'temporal_relationships';
commit;

-- vim: set filetype=pgsql expandtab tabstop=2 shiftwidth=2:
