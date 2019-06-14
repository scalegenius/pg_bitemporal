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
'bitemp_tables',
'devices', 
'device_id integer, device_descr text', 
'device_id') ;

The following table will be created:

bitemp_tables.devices (
      device_id_key serial not null
    , device_id integer   
    , device_descr text
    , row_created_at timestamptz NOT NULL DEFAULT now()
    , effective tstzrange
    , asserted tstzrange
    , CONSTRAINT devices_device_id_asserted_effective_excl EXCLUDE 
      USING gist (device_id WITH =, asserted WITH &&, effective WITH &&)
  ) ;
  
  
 #ll_is_bitemporal_table - checks whether a table is bitemporal 
 
 select * from bitemporal_internal.ll_is_bitemporal_table(p_table text) 
 RETURNS boolean 
 
 Input: 
 p_table - the name of the table (icluding the schema name)
 
 Output:
 
 true or false
 
 
 #bl_bitemporal_insert - inserts a row into bitemporal table
 
 bitemporal_internal.ll_bitemporal_insert(p_table text
,p_list_of_fields text
,p_list_of_values TEXT
,p_effective temporal_relationships.timeperiod 
,p_asserted temporal_relationships.timeperiod )
RETURNS INTEGER

Input:

p_table - bitemporal table name (including the schema name)
p_list_of_fields  - a character string, containing the list of columns for 
which the values will be passed, separated by commas
p_list_of_values  -  a list of values to be inserted, separated by commas. 
note that you should pass the text values in quotes (see example)
p_effective - an effective interval of the inserted records, should be of type 
timeperiod, as defined when pg_bitemporal was installed (tstzrange by default)
p_asserted - an asserted  interval of the inserted records, should be of type 
timeperiod, as defined when pg_bitemporal was installed (tstzrange by default)

Output: number of records inserted (1 if success)

Example:


select * from bitemporal_internal.ll_bitemporal_insert(
'bitemp_tables.devices',
'device_id , device_descr', 
$$1, 'description_1'$$, 
'[now(), infinity)'::tstzrange, 
'[now(), infinity)' ::tstzrange);

#ll_bitemporal_insert_select - inserts a set of records into a bitemporal table

bitemporal_internal.ll_bitemporal_insert_select(p_table text
,p_list_of_fields text
,p_select TEXT
,p_effective temporal_relationships.timeperiod 
,p_asserted temporal_relationships.timeperiod ) 
RETURNS INTEGER

Input:

p_table - bitemporal table name (including the schema name)
p_list_of_fields  - a character string, containing the list of columns for 
which the values will be passed, separated by commas
p_select  -  s text field which contains a select statement which will be used as 
a source of data to be inserted, may contain a set of lists of values
p_effective - an effective interval of the inserted records, should be of type 
timeperiod, as defined when pg_bitemporal was installed (tstzrange by default)
p_asserted - an asserted  interval of the inserted records, should be of type 
timeperiod, as defined when pg_bitemporal was installed (tstzrange by default)

Output: 

number of records inserted


Example 1:

select * from bitemporal_internal.ll_bitemporal_insert_select                                                                                                         ('bitemp_tables.devices',
'device_id , device_descr', 
$$(2, 'description_2'),
  (3, 'decription_3')$$, 
'[now(), infinity)'::tstzrange, 
'[now(), infinity)' ::tstzrange);

Example 2:

select * from bitemporal_internal.ll_bitemporal_insert_select                                                                                                         ('bitemp_tables.devices',
'device_id , device_descr', 
$$select device_id, device_description
     from nontemporal.devices
       where device_id between 4 and 10$$, 
'[now(), infinity)'::tstzrange, 
'[now(), infinity)' ::tstzrange);

#ll_bitemporal_update - performs bitemporal update operation. 
This function has two signatures:

bitemporal_internal.ll_bitemporal_update(p_table text
,p_list_of_fields text -- fields to update
,p_list_of_values TEXT  -- values to update with
,p_search_fields TEXT  -- search fields
,p_search_values TEXT  --  search values
,p_effective temporal_relationships.timeperiod  
,p_asserted temporal_relationships.timeperiod  
) 
RETURNS INTEGER

bitemporal_internal.ll_bitemporal_update(
p_schema_name text
p_table text
,p_list_of_fields text -- fields to update
,p_list_of_values TEXT  -- values to update with
,p_search_fields TEXT  -- search fields
,p_search_values TEXT  --  search values
,p_effective temporal_relationships.timeperiod  
,p_asserted temporal_relationships.timeperiod  
) 
RETURNS INTEGER

We strongly recommend to use the latter one, the first signature is 
non-compatiable with PG 10 and up, and is retained solely for the backward 
compatiability, will be eventually retired. In addition, the performance of 
the second version is much better. 

bitemporal update has some significant limitations on it's usage. The most 
restrictive is that the search criteria are limited to the EQUAL to some 
constants. Please use update_select for more complex search criteria

Input:

p_schema_name - the name of the schema 
p_table_name  - bitemporal table name 
p_table  - the table name, including the schema name (for older version only)
p_list_of_fields  - a character string, containing the list of columns which will 
be updated, separated by commas
p_list_of_values  -  a list of updated values, separated by commas. 
p_search_fields - the list of fields used in the WHERE clause
p_search_values TEXT - the list of values for the search fields
p_effective - effective range of the update
p_asserted - assertion for the update


Example:

Update future effective period asserted immediately.

select * from  bitemporal_internal.ll_bitemporal_update('bitemp_tables'
,'devices',
,'device_descr'
,$$'descr starting from jan 1'$$ 
,'device_id'  
,$$1$$  
,'[2020-01-01, infinity)'
, [now(), infinity)') 

Update future effective period future assertion

select * from  bitemporal_internal.ll_bitemporal_update('bitemp_tables'
,'devices',
,'device_descr'
,$$'updated description'$$ 
,'device_id'  
,$$2$  
,'[2019-10-31, infinity)'
, [2019-10-30, infinity)') 


