-- 
--
--  triggers, not null, exclusions and check 
-- all work exactly the same given the bitemporal constraints
--
-- 3 constraints do not. primary key, foreign key and unique constraints.
--

-- create the three types of constraints.
--   need strings to include in a create table
--   need commands to modify existing table

-- find the a particular set of constraints given a table
-- 

create or replace 
function bitemporal_internal.conname_prefix() returns text
language sql IMMUTABLE as $f$ 
    select 'bitemporal'::text;
$f$;

create or replace 
function bitemporal_internal.mk_conname(con_type text, src_column text, fk_table text, fk_column text )
returns text
language sql IMMUTABLE
as $f$ 
select substring(format('%s %s %s%s%s', conname_prefix()
                   , con_type
                   , src_column
                   , fk_table, fk_column)
          from 0 for 64 );
$f$
SET search_path = 'bitemporal_internal';


create or replace 
function bitemporal_internal.mk_constraint(con_type text, con_name text, con_src text)
returns text
language sql IMMUTABLE
as $ff$ 
  select format($$CONSTRAINT %I check(true or '%s' <> '@%s@') $$
        , con_name 
        , con_type
        , con_src)::text;
$ff$;

create or replace 
function bitemporal_internal.pk_constraint(src_column text)
returns text
language sql IMMUTABLE as $f$
  select mk_constraint('pk', mk_conname('pk', src_column, '', '') , src_column);
$f$
SET search_path = 'bitemporal_internal';

create or replace
function bitemporal_internal.fk_constraint(src_column text, fk_table text, fk_column text, connname text)
returns text
language sql IMMUTABLE
as $ff$
  select mk_constraint('fk'
             , connname
             , format('%s -> %s(%s)', src_column, fk_table, fk_column) );
$ff$
SET search_path = 'bitemporal_internal';

create or replace 
function bitemporal_internal.fk_constraint(src_column text, fk_table text, fk_column text)
returns text
language sql IMMUTABLE
as $ff$ 
  select fk_constraint(src_column , fk_table , fk_column,
          mk_conname('fk', src_column, fk_table, fk_column));
$ff$
SET search_path = 'bitemporal_internal';

create or replace 
function bitemporal_internal.unique_constraint(src_column text)
returns setof text
language sql IMMUTABLE
as $f$ 
  values ( mk_constraint('u'
             , mk_conname('u', src_column, '','')
             , format('%s', src_column) )),
       (format('CONSTRAINT %I EXCLUDE USING gist (%I WITH =, asserted WITH &&, effective WITH &&)'
             , mk_conname('unique', src_column, '', '')
             , src_column)::text)

    ;
--   CONSTRAINT devices_device_id_asserted_effective_excl EXCLUDE 
--  USING gist (device_id WITH =, asserted WITH &&, effective WITH &&)
$f$
SET search_path = 'bitemporal_internal';

create or replace 
function bitemporal_internal.add_constraint(table_name text, _con text)
returns text
language sql IMMUTABLE
as $f$ 
  select format('alter table %s add %s', table_name, _con)::text; 
$f$;

create or replace 
function bitemporal_internal.select_constraint_value(src text)
returns  text
language plpgsql IMMUTABLE
as $f$ 
DECLARE 
  at int;
  s   text;
BEGIN
-- select inside @ @
  at := strpos(src, '@');
  s  := substr(src, at + 1 );
  at := strpos(s, '@');
  return substring(s from 0::int for at );
END;
$f$;

create
type bitemporal_internal.bitemporal_pg_constraint
as
(
oid	oid
,conname	name
,connamespace	oid
,contype	"char"
,condeferrable	bool
,condeferred	bool
,convalidated	bool
,conrelid	oid
,contypid	oid
,conindid	oid
-- ,conparentid	oid
,confrelid	oid
,confupdtype	"char"
,confdeltype	"char"
,confmatchtype	"char"
,conislocal	bool
,coninhcount	int4
,connoinherit	bool
,conkey	int2[]
,confkey	int2[]
,conpfeqop	oid[]
,conppeqop	oid[]
,conffeqop	oid[]
,conexclop	oid[]
,conbin	pg_node_tree
, consrc	text
);

create or replace
function bitemporal_internal.find_constraints(table_name text, _criteria text )
returns setof  bitemporal_internal.bitemporal_pg_constraint
language sql IMMUTABLE
as $f$
    select oid, conname, connamespace, contype,
      condeferrable, condeferred,convalidated,
  conrelid, contypid, conindid, /* conparentid,*/ confrelid,
  confupdtype, confdeltype, confmatchtype, conislocal,
  coninhcount	, connoinherit, conkey, confkey,
  conpfeqop	, conppeqop	, conffeqop	, conexclop	, conbin
       , pg_get_expr(conbin, conrelid) as consrc -- .pg_get_constraintdef()
       from pg_constraint
       where conrelid = cast(table_name as regclass)
       and conname like format('%s %s %%', bitemporal_internal.conname_prefix(), _criteria )
       ;
$f$;

create or replace 
function bitemporal_internal.find_pk(table_name text)
returns text
language plpgsql IMMUTABLE
as $f$ 
DECLARE
    r  record;
BEGIN
    select * into r from bitemporal_internal.find_constraints(table_name, 'pk');
    RETURN bitemporal_internal.select_constraint_value(r.consrc);
END;
$f$;

create table if not exists bitemporal_internal.fk_constraint_type (
   conname name
  , src_column  name
  , fk_table text
  , fk_column name
);

create or replace 
function bitemporal_internal.split_out_fk(consrc text)
returns bitemporal_internal.fk_constraint_type
language plpgsql IMMUTABLE
as $f$ 
DECLARE
    src text;
    ref text;
    rc  fk_constraint_type%ROWTYPE;
    rp int;
    lp int;
BEGIN
    -- format('%s -> %s(%s)', src_column, fk_table, fk_column) 
    src := select_constraint_value(consrc) ;
    rc.src_column :=  split_part(src, ' ', 1);
    ref := split_part(src, ' ', 3);
    rp := strpos(ref, '(');
    lp := strpos(ref, ')');
    if (lp < 1 or rp < 1 ) then
      raise notice 'split_out_bitemporal_fk: invaild format "%"', consrc ;
      return NULL;
    end if;
    rc.fk_table := substring(ref from 0 for rp );
    rc.fk_column :=  substring(ref from rp +1 for (lp - rp -1) );
    RETURN rc;
END;
$f$
SET search_path = 'bitemporal_internal';

create or replace
function bitemporal_internal.find_fk(table_name text)
returns setof bitemporal_internal.fk_constraint_type
language plpgsql
as $f$ 
DECLARE
    rc  bitemporal_internal.fk_constraint_type%ROWTYPE;
    r record;
BEGIN
    
    for r in select * from bitemporal_internal.find_constraints(table_name, 'fk')
    loop
        rc := bitemporal_internal.split_out_fk(r.consrc);
        rc.conname := r.conname;
        return next  rc;
    end loop;
    RETURN ;
END;
$f$;



/*
       conname       | contype | conrelid |
consrc                                          
---------------------+---------+----------+-----------------------------------------------------------------------------------------
 bitemporal fk 1     | c       |  1625561 | (true OR ('fk'::text <> '@node_id -> sg.networks network_id@'::text))
 bitemporal fk 2     | c       |  1625561 | (true OR ('fk'::text = ANY (ARRAY['node_id'::text, 'cnu.networks'::text, 'id'::text])))
 bitemporal unique 3 | c       |  1625561 | (true OR ('col'::text = 'name'::text))

*/

-- vim: set filetype=pgsql expandtab tabstop=2 shiftwidth=2:
