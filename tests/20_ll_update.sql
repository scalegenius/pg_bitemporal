
BEGIN;
set client_min_messages to warning;
set local search_path = 'bi_temp_tables','bitemporal_internal','public';
set local TimeZone  = 'UTC';

SELECT plan(23);

select  unialike( current_setting('search_path'), '%temporal_relationships%'
  ,'temporal_relationships should NOT be on search_path for these tests' );



select lives_ok($$ 
    create schema bi_temp_tables 
$$, 'create schema');

select lives_ok($$
  create table bi_temp_tables.devices_manual (
      device_id_key serial NOT NULL
    , device_id integer
    , effective tstzrange
    , asserted tstzrange
    , device_descr text
    , row_created_at timestamptz NOT NULL DEFAULT now()
    , CONSTRAINT devices_device_id_asserted_effective_excl EXCLUDE 
      USING gist (device_id WITH =, asserted WITH &&, effective WITH &&)
  ) 
$$, 'create devices manual');

select lives_ok($$select * from bitemporal_internal.ll_create_bitemporal_table('bi_temp_tables','devices', 
'device_id integer, device_descr text', 'device_id') 
$$, 'create devices');

select lives_ok($$
insert into  bi_temp_tables.devices( device_id , effective, asserted, device_descr ) 
values 
 (1, '[01-01-2015, infinity)', '[01-01-2015, infinity)','descr2')
,(5, '[2015-12-01 00:00:00-06,2015-12-31 00:00:00+00)', '[2015-12-01 00:00:00-06,2015-12-31 00:00:00+00)', 'test_5')
$$, 'insert data into devices');

---non-temp for test

select lives_ok($$
  create table bi_temp_tables.devices_non_temp(
    device_id_key serial,
    device_id int4
  );
  create table bi_temp_tables.devices_temp(
   device_id_key serial,
   device_id int4,
   effective tsrange,
   EXCLUDE USING gist (device_id WITH =,  effective WITH &&)
  )
$$, 'double create');

select is( ll_is_bitemporal_table('bi_temp_tables.devices_temp'), false
      , 'is bitemporal table? devices_temp');

select is( ll_is_bitemporal_table('bi_temp_tables.devices')
    , true  , 'is bitemporal table? devices');

select is( ll_is_bitemporal_table('bi_temp_tables.devices_manual')
    , true  , 'is bitemporal table? devices_manual');

select is( ll_is_bitemporal_table('bi_temp_tables.devices_non_temp')
    , false , 'is bitemporal table? devices_non_temp');

select is( ll_is_bitemporal_table('bi_temp_tables.dev')
    , false , 'is bitemporal table? dev');

----test insert:
 select results_eq ($q$
  select bitemporal_internal.ll_bitemporal_insert('bi_temp_tables.devices',
  'device_id , device_descr', $$'11', 'new_descr'$$, '[01-01-2016, infinity)', '[01-02-2016, infinity)' )
$q$, 
$v$ values(1) $v$
,'bitemporal insert'
);

-- select * from bi_temp_tables.devices ;

select results_eq($q$ select device_id, device_descr, effective, asserted
from bi_temp_tables.devices where device_id =11 $q$
, $v$
values 
( 11
,'new_descr'::text
  ,'["2016-01-01 00:00:00+00",infinity)'::temporal_relationships.timeperiod
  ,'["2016-01-02 00:00:00+00",infinity)'::temporal_relationships.timeperiod
)
$v$ 
,'bitemporal insert, returns select '
);


select results_eq($q$ 
    select * from 
  bitemporal_internal.ll_bitemporal_list_of_fields('bi_temp_tables.devices')
$q$
, $v$
values 
( 
ARRAY['device_id','device_descr']
)
$v$ 
,'list of fields'
);

---test correction


select results_eq($q$select * from bitemporal_internal.ll_bitemporal_correction('bi_temp_tables.devices',
'device_descr',
$$'updated_descr_11'$$,
'device_id' , 
'11',
'[01-01-2016, infinity)' )$q$, 
$v$ values(1) $v$
,'bitemporal correction'
);

select results_eq($q$ 
  select device_descr
        from bi_temp_tables.devices where device_id = 11 and upper(asserted)='infinity' and effective='[01-01-2016, infinity)'
$q$
, $v$
values 
('updated_descr_11'::text)
$v$ 
,'select after bitemporal correction - old'
);

select results_eq($q$ 
  select device_descr
        from bi_temp_tables.devices where device_id = 11 and lower(asserted)='2016-01-02'
         and effective='[01-01-2016, infinity)'
$q$
, $v$
values 
('new_descr'::text)
$v$ 
,'select after bitemporal correction - new'
);

select results_eq ($q$
select bitemporal_internal.ll_bitemporal_insert('bi_temp_tables.devices',
  'device_id , device_descr', $$'10', 'descr_crean_insert'$$, '[01-01-2017, infinity)', '[2017-07-09 21:59:58.993815-05, infinity)' )
$q$, 
$v$ values(1) $v$
,'bitemporal insert for correction'
);
select * from bitemporal_internal.ll_bitemporal_correction('bi_temp_tables.devices',
'device_descr',
$$'descr_10_corr_on_place'$$,
'device_id' , 
'10',
'[01-01-2017, infinity)' , '2017-07-09 21:59:58.993815-05'
)

select * from bitemporal_internal.ll_bitemporal_correction('bi_temp_tables.devices',
'device_descr',
$$'descr_10_corr_with_new-record'$$,
'device_id' , 
'10',
'[01-01-2017, infinity)' , '2017-07-09 22:10:58.993815-05'
)       


---test update with error:
/*

select * from bitemporal_internal.ll_bitemporal_update('bi_temp_tables.devices'
,'device_descr'
,$$'descr starting from jan 1'$$ 
,'device_id'  
,$$8$$  
,'[2016-01-01, infinity)'
, '[2016-01-02, infinity)') 
;

---output:

ERROR:  Asserted interval starts in the past or has a finite end: ["2016-01-02 00:00:00-06",infinity)

Exactly the same test should be performed for inactivate
*/

/* should probably include the test of bitemporal_internal.ll_check_bitemporal_update_conditions - if it returns zero,
then 

ERROR:  Nothing to update, use INSERT or check effective: ["2015-01-02 00:00:00-06","2015-02-02")

Exactly the same test should be performed for inactivate


*/


---correct test:

select results_eq($q$ 
select *  from bitemporal_internal.ll_bitemporal_update('bi_temp_tables.devices'
,'device_descr'
,$$'descr starting from jan 1'$$ 
,'device_id'  
,$$1$$  
,'[2018-01-01, infinity)'
, '[3016-03-01, infinity)') $q$, 
$v$ values(1) $v$
,'bitemporal update - correct'
);

----inactivate

select results_eq($q$select * from bitemporal_internal.ll_bitemporal_inactivate('bi_temp_tables.devices'
,'device_id'  
,$$11$$  
,'[3016-02-02, infinity)'
, '[3016-02-02, infinity)')  $q$, 
$v$ values(1) $v$
,'bitemporal inactivate - correct'
);


select results_eq($q$select count(*)::integer from bi_temp_tables.devices 
where device_id=11 
and  '[3016-03-16,  3016-03-16]'<@ effective 
and '[3016-02-04, 3016-02-04]' <@ asserted $q$, 
$v$ values(0::integer) $v$,'bitemporal inactivate no active rows');


---delete:

select results_eq($q$select * from bitemporal_internal.ll_bitemporal_delete('bi_temp_tables.devices'
,'device_id'  
,$$1$$  
, '[3016-04-04 21:30, infinity)')  $q$, 
$v$ values(2) $v$
,'bitemporal delete');

select results_eq($q$select count(*)::integer from bi_temp_tables.devices 
where device_id=1 
and '[3016-04-05, 3016-04-05]' <@ asserted $q$, 
$v$ values(0::integer) $v$
,'bitemporal delete no active rows');

SELECT * FROM finish();
ROLLBACK;
-- vim: set filetype=pgsql expandtab tabstop=2 shiftwidth=2:
