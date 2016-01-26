
BEGIN;
set client_min_messages to warning;
set local search_path = 'bi_temp_tables','bitemporal_internal','temporal_relationships','public';
set local TimeZone  = 'UTC';

SELECT plan( 14 );

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

select lives_ok($$select * from bitemporal_internal.ll_create_bitemporal_table('bi_temp_tables.devices', 
'device_id_key serial,device_id integer, device_descr text', 'device_id') 
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
select lives_ok($q$
  select bitemporal_internal.ll_bitemporal_insert('bi_temp_tables.devices',
  'device_id , device_descr', $$'11', 'new_descr'$$, '[01-01-2016, infinity)', '[01-02-2016, infinity)' )
$q$
,'bitemporal insert'
);

-- select * from bi_temp_tables.devices ;

select results_eq($q$ 
  select device_id,effective::text,asserted::text,device_descr
        from bi_temp_tables.devices where device_id = 11
$q$
, $v$
values 
( 11
  ,'["2016-01-01 00:00:00+00",infinity)'
  ,'["2016-01-02 00:00:00+00",infinity)'
  ,'new_descr'::text)
$v$ 
,'select after bitemporal insert'
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


select lives_ok($q$select * from bitemporal_internal.ll_bitemporal_correction('bi_temp_tables.devices',
'device_descr',
$$'updated_descr_11'$$,
'device_id' , 
'11',
'[01-01-2016, infinity)' )$q$
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

/* next test to write

---test update:

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

---correct test:

select * from bitemporal_internal.ll_bitemporal_update('bi_temp_tables.devices'
,'device_descr'
,$$'descr starting from jan 1'$$ 
,'device_id'  
,$$8$$  
,'[2016-01-01, infinity)'
, '[2016-01-03, infinity)') 
;
select * from bi_temp_tables.devices order by 2,1

---output

11;8;"["2015-12-01 00:00:00-06","2016-01-01 00:00:00-06")";"["2015-12-15 00:00:00-06","2016-01-03 00:00:00-06")";"descr8"
52;8;"["2015-12-01 00:00:00-06","2016-01-01 00:00:00-06")";"["2016-01-03 00:00:00-06",infinity)";"descr8"
53;8;"["2016-01-01 00:00:00-06",infinity)";"["2016-01-03 00:00:00-06",infinity)";"'descr starting from jan 1'"

*/

/* 
----inactivate

select * from bitemporal_internal.ll_bitemporal_inactivate('bi_temp_tables.devices'
,'device_id'  
,$$8$$  
,'[2016-01-31, infinity)'
, '[2016-01-03 21:30, infinity)') 
;select * from bi_temp_tables.devices order by 2,1



---output


11;8;"["2015-12-01 00:00:00-06","2016-01-01 00:00:00-06")";"["2015-12-15 00:00:00-06","2016-01-03 00:00:00-06")";"descr8"
49;8;"["2015-12-01 00:00:00-06","2016-01-01 00:00:00-06")";"["2016-01-03 00:00:00-06",infinity)";"descr8"
50;8;"["2016-01-01 00:00:00-06",infinity)";"["2016-01-03 00:00:00-06","2016-01-03 21:30:00-06")";"'descr starting from jan 1'"
51;8;"["2016-01-01 00:00:00-06","2016-01-31 00:00:00-06")";"["2016-01-03 21:30:00-06",infinity)";"'descr starting from jan 1'"



---delete:

select * from bitemporal_internal.ll_bitemporal_delete('bi_temp_tables.devices'
,'device_id'  
,$$8$$  
, '[2016-01-04 21:30, infinity)') 
;

---output:

11;8;"["2015-12-01 00:00:00-06","2016-01-01 00:00:00-06")";"["2015-12-15 00:00:00-06","2016-01-03 00:00:00-06")";"descr8"
57;8;"["2015-12-01 00:00:00-06","2016-01-01 00:00:00-06")";"["2016-01-03 00:00:00-06","2016-01-04 21:30:00-06")";"descr8"
58;8;"["2016-01-01 00:00:00-06",infinity)";"["2016-01-03 00:00:00-06","2016-01-03 21:30:00-06")";"'descr starting from jan 1'"
59;8;"["2016-01-01 00:00:00-06","2016-01-31 00:00:00-06")";"["2016-01-03 21:30:00-06","2016-01-04 21:30:00-06")";"'descr starting from jan 1'"

*/

SELECT * FROM finish();
ROLLBACK;
-- vim: set filetype=pgsql expandtab tabstop=2 shiftwidth=2:
