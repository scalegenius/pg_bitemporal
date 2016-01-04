begin;
create schema if not exists temporal_relationships;
set local search_path to temporal_relationships, public;
-- create a domain if not exists 
DO $d$
DECLARE
  domain_name text default 'timeperiod';
  --domain_type text default 'daterange';
  domain_type text default 'tstzrange';
BEGIN
PERFORM n.nspname as "Schema",
        t.typname as "Name",
        pg_catalog.format_type(t.typbasetype, t.typtypmod) as "Type"
FROM pg_catalog.pg_type t
      LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
WHERE t.typtype = 'd'
       AND n.nspname <> 'pg_catalog'
       AND n.nspname <> 'information_schema'
       AND pg_catalog.pg_type_is_visible(t.oid)
   AND t.typname = domain_name;
   if FOUND then
     raise NOTICE 'Domain % already exists', domain_name;
   else 
     execute format('create domain %I as %I', domain_name, domain_type);
   end if;
END;
$d$;
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
create or replace
function has_starts(a timeperiod , b timeperiod )
returns boolean language SQL IMMUTABLE 
as $$
  select fst(a) = fst(b) and snd(a) <> snd(b);
$$;
-- 
-- [finishes] [finishes^-1]
-- 
create or replace
function has_finishes(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select snd(a) = snd(b) and fst(a) <> fst(b);
$$;
-- 
-- [equals]
-- 
create or replace
function equals(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  -- doubtful = operator exists for timeperiod
 select fst(a) = fst(b) and snd(a) = snd(b) ;
$$;
-- 
-- [during] 
-- 
create or replace
function is_during(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select (fst(a) > fst(b)) and (snd(a) < snd(b));
$$;
-- 
-- [during^-1] contained
-- 
create or replace
function is_contained_in(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select is_during(b, a);
$$;

--
-- [during] or [during^-1] 
--
create or replace
function has_during(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select is_during(a, b) or is_during(b,a);
$$;
-- 
-- [overlaps] 
-- 
create or replace
function is_overlaps(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 

as $$
  select  fst(a) < fst(b) and snd(a) > fst(b) and snd(a) < snd(b);
$$;

--
-- either overlaps the other [overlaps] [overlaps^-1]
--
create or replace
function has_overlaps(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select  is_overlaps(a , b ) or is_overlaps(b , a ) ;
$$;
-- 
-- [before] 
-- 
create or replace
function is_before(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select  snd(a) < fst(b);
$$;
-- 
-- [before^-1]
-- 
create or replace
function is_after(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
    -- is_before(b, a)
   select snd(b) < fst(a);
$$;

-- 
-- either [before] [before^-1]
-- 
create or replace
function has_before(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select  snd(a) < fst(b) or snd(b) < fst(a);
$$;
-- 
-- [meets] [meets^-1]
-- 
-- no shared time tick.
-- 
create or replace
function is_meets(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
 select  snd(a) = fst(b) ;
$$;

create or replace
function has_meets(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select snd(a) = fst(b) or snd(b) = fst(a);
$$;
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
$$;

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
$$;

--
-- [Aligns With]
--   [Starts] or [Finishes]
--
create or replace
function has_aligns_with(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
   select   xor( fst(a) = fst(b) , snd(a) = snd(b) );
$$;

--
-- [Encloses]
--   [Aligns With] or [During]
-- 

create or replace
function has_encloses(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
  select has_during(a,b) or has_aligns_with(a,b);
$$;


--
-- [Excludes]
--   [Before] or [Meets]
--
create or replace
function has_excludes(a timeperiod, b timeperiod)
returns boolean language SQL IMMUTABLE 
as $$
   select fst(a) >= snd(b) or fst(b) >= snd(a) ;
$$ ;
commit;

-- vim: set filetype=pgsql expandtab tabstop=2 shiftwidth=2:
