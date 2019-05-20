##pg_bitemporal functions

#ll_generate_bitemp_for_schema - generates DDL to make a db schema bitemporal

bitemporal_internal.ll_generate_bitemp_for_schema(p_schema_name text)returns text

input:

p_schema - the name of the schema for which we want to generate it's bitemporal version

output:

DDL for new schema creation (does not provide data conversion

#ll_create_bitemporal_table - creates bitemporal table

bitemporal_internal.ll_create_bitemporal_table(
    p_schema text,
    p_table text,
    p_table_definition text,
    p_business_key text)
  RETURNS boolean 
  
 input:
  
    p_schema - the name of the schema where we want to create a bitemporal table
    p_table - the name of the table
    p_table_definition - a text field which should contain an actual DDL for all non-bitemporal fields
    p_business_key - the name of the column, which whill be userd as a natural key
    
Output: true or false depending on whether the table was successfully created.
    
Example:

select * from bitemporal_internal.ll_create_bitemporal_table(
'bi_temp_tables',
'devices', 
'device_id integer, device_descr text', 
'device_id') ;

The following table will be created:

bi_temp_tables.devices_manual (
      device_id_key serial not null
    , device_id integer
    , effective tstzrange
    , asserted tstzrange
    , device_descr text
    , row_created_at timestamptz NOT NULL DEFAULT now()
    , CONSTRAINT devices_device_id_asserted_effective_excl EXCLUDE 
      USING gist (device_id WITH =, asserted WITH &&, effective WITH &&)
  ) 
  
  
  
