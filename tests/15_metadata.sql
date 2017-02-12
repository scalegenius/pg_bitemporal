BEGIN;
SELECT plan( 14 );

select  unialike( current_setting('search_path'), '%bitemporal_internal%'
  ,'bitemporal_internal should NOT be on search_path for these tests' );

select lives_ok($$
create table t9 (
id serial primary key
, name text
, mark int
, node_id int
, unique (name)
, unique ( mark )
, constraint "bitemporal fk 1" check ( true or 'fk' <> '@node_id -> sg.networks(network_id)@')
, constraint "bitemporal fk 2" check ( true or 'fk' <>'@node_id -> networks(id)@' )
, constraint "bitemporal unique 3" check ( true or 'u' <> 'name' )
, constraint "bitemporal pk 1" check ( true or 'pk' <> '@id@')
) 
$$);

--   consrc
-- ---------------------+---------+----------+-----------------------------------------------------------------------------------------
--  bitemporal fk 1     | c       |  1625561 | (true OR ('fk'::text <> '@node_id -> sg.networks network_id@'::text))
--  bitemporal fk 2     | c       |  1625561 | (true OR ('fk'::text = ANY (ARRAY['node_id'::text, 'cnu.networks'::text, 'id'::text])))
--  bitemporal unique 3 | c       |  1625561 | (true OR ('col'::text = 'name'::text))
--

select lives_ok($$
   select bitemporal_internal.conname_prefix() ;
$$, 'bitemporal_conname_prefix' );

select is(bitemporal_internal.mk_conname('a','b','c','d')
, format('%s a bcd', bitemporal_internal.conname_prefix() )
, 'mk_conname');


select is(bitemporal_internal.mk_constraint('type', 'name', 'source')
, $$CONSTRAINT name check(true or 'type' <> '@source@') $$
, 'mk_constraint');

select is( bitemporal_internal.pk_constraint('src_column')
, $$CONSTRAINT "bitemporal pk src_column" check(true or 'pk' <> '@src_column@') $$
, 'pk_constraint');


--  format('% -> %(%)', src_column, fk_table, fk_column)
select is( bitemporal_internal.fk_constraint('a', 'b', 'c')
, $$CONSTRAINT "bitemporal fk abc" check(true or 'fk' <> '@a -> b(c)@') $$
, 'fk_constraint');

select results_eq(
$q$
  select string_agg(a, ', ') from bitemporal_internal.unique_constraint('a') as s(a)
$q$::text,
$$ values
('CONSTRAINT "bitemporal u a" check(true or ''u'' <> ''@a@'')' || ' , ' ||
  'CONSTRAINT "bitemporal unique a" EXCLUDE USING gist '||
    '(a WITH =, asserted WITH &&, effective WITH &&)')
$$
, 'unique_constraint' );

select is( bitemporal_internal.select_constraint_value($$asdfasdfasdf '@XXX@' $$)
, 'XXX'
, 'select_constraint_value' );

select is(bitemporal_internal.add_constraint('t9', 'XXX')
, $$alter table t9 add XXX$$
, 'add_constraint');

select is( bitemporal_internal.find_pk('t9') , $$id$$ , 'find_bitemporal_pk');


select bag_eq( $$
  select * from bitemporal_internal.find_fk('t9')
$$,
$$
  values
 ('bitemporal fk 1', 'node_id','sg.networks','network_id')
, ('bitemporal fk 2','node_id','networks','id' )
$$
, 'find_bitemporal_fk');

select results_eq($$
select count(*)::int from bitemporal_internal.find_constraints('t9', '%' )
$$
, $$
values ( 4::int )
$$, 'find_constraints');

select has_relation('bitemporal_internal', 'fk_constraint_type', 'table bitemporal_internal.fk_constraint_type exists');

SELECT * FROM finish();
ROLLBACK;

-- vim: set filetype=pgsql expandtab tabstop=2 shiftwidth=2:
