BEGIN;
set client_min_messages to warning;
SELECT plan(1);
CREATE SCHEMA bi_temp_tables;

--create test table
--------------------------
drop table if exists bi_temp_tables.devices;
create table bi_temp_tables.devices (device_id_key serial NOT NULL,
  device_id integer,
  effective tstzrange,
  asserted tstzrange,
  device_descr text,
  CONSTRAINT devices_device_id_asserted_effective_excl EXCLUDE 
  USING gist (device_id WITH =, asserted WITH &&, effective WITH &&)
)


insert into  bi_temp_tables.devices(
device_id ,
effective,
asserted,
device_descr ) values (1, '[01-01-2015, infinity)', '[01-01-2015, infinity)','descr_1')


---non-temp for test
drop table if exists bi_temp_tables.devices_non_temp;

create table bi_temp_tables.devices_non_temp(
device_id_key serial,
device_id int4
);

drop table if exists bi_temp_tables.devices_temp;
create table bi_temp_tables.devices_temp(
device_id_key serial,
device_id int4,
effective tsrange,
EXCLUDE USING gist (device_id WITH =,  effective WITH &&)
);


-- 
-- Test each relationship function against the sample data
--

select results_eq(
$q$ select ll_is_bitemporal_table('bi_temp_tables.devices')
 $q$::text,
'true', 'bitemp_table'
);


SELECT * FROM finish();
ROLLBACK;
