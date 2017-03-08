
# Time Period Relationships


There are 13 Allen Relationships. There are 6 basic relationships and 6
inverses of those basic relationships and the equality relationship. The
Allen relationship definitions assume the relationships are not communitive. 

This library assumes that if the relationship and its inverse are communitive
then there is one function defined and it is left to the user to swap the
arguments if necessary. 

The 13 relationships are partitioned into 5 groups that encompasses
two or more basic relationships. These partitions allow for a larger range
of expression between two time periods. 


## User Functions


The user functions are defined below. They are group in a way that makes
explaination easier. 
    
```Sql
<< high level functions >>=
<< simple functions >>
<< during functions >>
<< overlaps functions >>
<< before functions >>
<< meets functions >>
<< partition functions >>
```
The function names are prefix with _is_ if the function implements a single
relationship such as *is\_after* implementing the *[Before^-1]* relationship. 
The prefix _has_ is used to denote a function that implements both a
relationship and its inverse, such as *has\_starts* implementing *[Starts]* and
*[Starts^-1*.


### Starts, Finishes and Equals

The has\_starts is [starts] and [starts^-1].
The has\_finishes is [finishes] [finishes^-1].
The equals is [equals]. As it is a single relationship that is its own inverse
neither is or has prefix was used.

The order of the arguments is unimportant to these relationship.


```Sql
<< simple functions >>=
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
```
### During

is\_during implements [during] and is\_contained\_in implements [during^-1].
The function names are meant to match how the relationship reads in english.

The function has\_during implments the alternative of either argument is during
the other. This is effective [during] or [during^-1].
It is unclear if this function would be needed.

```Sql
<< during functions >>=
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
```
### Overlaps

Implementing [Overlaps] and [Overlaps^-1] is confusing. 
is\_overlaps implments the relationships based on the order of the arguments.
But as an english description there is no real inverse as a can overlap b or b
overlaps a. has\_overlaps implements both cases of overlap.


```Sql
<< overlaps functions >>=
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
```
### Before

There are three [before] and [before^-1] functions. is\_before implements
[before] and is\_after implements [before^-1] and has\_before implements
both cases. The word after reads better as the inverse of before.
has\_before handles the case of determining if either argument is before the
other without regard to which.

```Sql
<< before functions >>=
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
```
### Meets

Meets is implemeted similarlly to overlaps. There is no english words to
handle the inverse case. is\_meets can be used for [meets] or [meets^-1]
depending on the order of arguments. has\_meets implements the order agnostic
case of whether two time periods meet in either way. 

```Sql
<< meets functions >>=
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
```
### Partitioning of Relationships


According to Johnston 2014 and Johnston, Weis 2010, the Allen Relationships
can be binary partitioned into groups. Theses groupings allow for an extended
set of terms to describe time period relationships. 

* Excludes 
* Includes
* Contains
* Encloses
* AlignsWith

Excludes is [Before], [Before^-1], [Meets], [Meets^-1].
Includes is [Overlaps], [Overlaps^-1] and the Contains group.
Contains is [Equals] and Encloses group.
Encloses is [During], [During^-1] and the AlignsWith group.
AlignsWith is [Starts],[Starts^-1], [Finishes], and [Finishes^-1].

All the functions uses the _has_ prefix to denote they are agnostic to the
order of arguements.


```Sql
<< partition functions >>=
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
```
### Schema

```Sql
<< schema >>=
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
```
 ### Types


```Sql
<< domain type functions >>=
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
```
### Support Functions

The accessor functions in Postgresql for ranges uses lower/upper but
I found that naming to be a bit arch. fst & snd are named after the haskell
methods for accessing elements of a two-tuple. 

The xor function was to make the logic of the partition functions more
readable and tie into the function definitions. The expressions were
simplified and the xor arose during simplification.

```Sql
<< internal functions >>=
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
```
## References

1.  James F. Allen. 1983. "Maintaining knowledge about temporal intervals." 
    Commun.  ACM 26, 11 (November 1983), 832-843. [DOI](http://dx.doi.org/10.1145/182.358434)
2.  Tom Johnston. 2014. "Bitemporal Data: Theory and Practice (1st ed.)." 
    Morgan Kaufmann Publishers Inc., San Francisco, CA, USA.
3.  Tom Johnston & Randall Weis. 2010. "Managing Time in Relational Databases: 
    How to Design, Update and Query Temporal Data." Morgan Kaufmann Publishers Inc., San Francisco, CA, USA.



## Appendix: Main File structure

The file structure is as follows

```Sql
<< * >>=
begin;
<< schema >>
<< domain type functions >>
<< internal functions >>
<< high level functions >>
commit;

-- vim: set filetype=pgsql expandtab tabstop=2 shiftwidth=2:
```
