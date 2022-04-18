# pg_bitemporal functions

## ll_generate_bitemp_for_schema

Generates DDL to make a db schema bitemporal

```
bitemporal_internal.ll_generate_bitemp_for_schema(
    p_schema_name TEXT
);
```

Returns: TEXT

Input:

- p_schema - the name of the schema for which we want to generate it's bitemporal version

Output:

- DDL for new schema creation (does not provide data conversion)

## ll_create_bitemporal_table

Creates a bitemporal table

```
bitemporal_internal.ll_create_bitemporal_table(
    p_schema TEXT,
    p_table TEXT,
    p_table_definition TEXT,
    p_business_key TEXT)
```

Returns: BOOLEAN

Input:

- p_schema - the name of the schema where we want to create a bitemporal table
- p_table - the name of the table
- p_table_definition - a text field which should contain an actual DDL for all non-bitemporal fields
- p_business_key - the name of the column, which whill be userd as a natural key

Output:

- true or false depending on whether the table was successfully created.

Example:

```
select \* from bitemporal_internal.ll_create_bitemporal_table(
    'bitemp_tables',
    'devices',
    'device_id integer, device_descr text',
    'device_id'
);
```

The following table will be created:

```
bitemp_tables.devices (
    device_id_key serial not null,
    device_id integer,
    device_descr text,
    row_created_at timestamptz NOT NULL DEFAULT now(),  effective tstzrange,
    asserted tstzrange,
    CONSTRAINT devices_device_id_asserted_effective_excl EXCLUDE
    USING gist (device_id WITH =, asserted WITH &&, effective WITH &&)
);

```

## ll_is_bitemporal_table

Checks whether a table is bitemporal

```
select \* from bitemporal_internal.ll_is_bitemporal_table(
    p_table text
);
```

Returns: BOOLEAN

Input:

- p_table - the name of the table (including the schema name)

Output:

- true or false

## ll_bitemporal_insert

Inserts a row into a bitemporal table

```
bitemporal_internal.ll_bitemporal_insert(
    p_table TEXT,
    p_list_of_fields TEXT,
    p_list_of_values TEXT,
    p_effective temporal_relationships.timeperiod,
    p_asserted temporal_relationships.timeperiod
);
```

Returns: INTEGER

Input:

- p_table - bitemporal table name (including the schema name)
- p_list_of_fields - a character string, containing the list of columns for
  which the values will be passed, separated by commas
- p_list_of_values - a list of values to be inserted, separated by commas.
  note that you should pass the text values in quotes (see example)
- p_effective - an effective interval of the inserted records, should be of type
  timeperiod, as defined when pg_bitemporal was installed (tstzrange by default)
- p_asserted - an asserted interval of the inserted records, should be of type
  timeperiod, as defined when pg_bitemporal was installed (tstzrange by default)

Output:

- number of records inserted (1 if success)

Example:

```
select \* from bitemporal_internal.ll_bitemporal_insert(
    'bitemp_tables.devices',
    'device_id ,
    device_descr',
    $$1,
    'description_1'$$,
    '[now(), infinity)'::tstzrange,
    '[now(), infinity)'::tstzrange
);

```

## ll_bitemporal_insert_select

Inserts a set of records into a bitemporal table

```
bitemporal_internal.ll_bitemporal_insert_select(
    p_table TEXT,
    p_list_of_fields TEXT,
    p_select TEXT,
    p_effective temporal_relationships.timeperiod,
    p_asserted temporal_relationships.timeperiod
);
```

Returns: integer

Input:

- p_table - bitemporal table name (including the schema name)
- p_list_of_fields - a character string, containing the list of columns for
  which the values will be passed, separated by commas
- p_select - s text field which contains a select statement which will be used as
  a source of data to be inserted, may contain a set of lists of values
- p_effective - an effective interval of the inserted records, should be of type
  timeperiod, as defined when pg_bitemporal was installed (tstzrange by default)
- p_asserted - an asserted interval of the inserted records, should be of type
  timeperiod, as defined when pg_bitemporal was installed (tstzrange by default)

Output:

- number of records inserted

Example 1:

```
select \* from bitemporal_internal.ll_bitemporal_insert_select (
    'bitemp_tables.devices',
    'device_id , device_descr',
    $$(3, 'decription_3')$$,
    '[now(), infinity)'::tstzrange,
    '[now(), infinity)' ::tstzrange
);
```

Example 2:

```
select * from bitemporal_internal.ll_bitemporal_insert_select(
    'bitemp_tables.devices',
    'device_id , device_descr',
    $$from nontemporal.devices where device_id between 4 and 10$$,
    '[now(), infinity)'::tstzrange,
    '[now(), infinity)'::tstzrange
);
```

## ll_bitemporal_update

Performs bitemporal update operation.


```
bitemporal_internal.ll_bitemporal_update(
    p_schema_name TEXT,
    p_table text,
    p_list_of_fields TEXT,
    p_list_of_values TEXT,
    p_search_fields TEXT,
    p_effective temporal_relationships.timeperiod,
    p_asserted temporal_relationships.timeperiod
);
```

Returns: INTEGER


_bitemporal update has some significant limitations on it's usage. The most
restrictive is that the search criteria are limited to the EQUAL to some
constants. Please use update_select for more complex search criteria_

Input:

- p_schema_name - the name of the schema
- p_table_name - bitemporal table name
- p_table - the table name, including the schema name (for older version only)
- p_list_of_fields - a character string, containing the list of columns which will
  be updated, separated by commas
- p_list_of_values - a list of updated values, separated by commas.
- p_search_fields - the list of fields used in the WHERE clause
- p_search_values - the list of values for the search fields
- p_effective - effective range of the update
- p_asserted - assertion for the update

Example 1:

Update future effective period asserted immediately.

```
select \* from bitemporal_internal.ll_bitemporal_update(
    'bitemp_tables',
    'devices',
    'device_descr',
    $$'descr starting from jan 1'$$,
    'device_id',
    $$1$$,
    '[2020-01-01, infinity)',
    [now(), infinity)'
);
```

Example 2:

```
Update future effective period future assertion

select \* from bitemporal_internal.ll_bitemporal_update(
    'bitemp_tables',
    'devices',
    'device_descr',
    $$'updated description'$$,
    'device_id',
    \$$2$,
    '[2019-10-31, infinity)',
    [2019-10-30, infinity)'
);

```

## ll_bitemporal_update_select

Performs bitemporal update, where both updated values and records to be updated are pre-selected, rather than explicitly set up. It is an equivalent of a regular UPDATE, when you use it in the following form:

```
UPDATE T1 set (a1, a2) = (SELECT v1, v2 FROM T2 WHERE ) WHERE T1.a3 IN (SELECT )
```


```
bitemporal_internal.ll_bitemporal_update_select(
    p_schema_name TEXT,
    p_table_name TEXT,
    p_list_of_fields TEXT, -- fields to update
    p_values_selected_update TEXT, -- values to update with
    p_search_fields TEXT, -- search fields
    p_values_selected_search TEXT, -- search values selected
    p_effective temporal_relationships.timeperiod, -- effective range of the update
    p_asserted temporal_relationships.timeperiod -- assertion for the update
);
```

Returns: INTEGER

Input:

- p_schema_name - the name of the schema
- p_table_name - bitemporal table name
- p_table - the table name, including the schema name (for older version only)
- p_list_of_fields - a character string, containing the list of columns which will
  be updated, separated by commas
- p_values_selected_update - the text of select statement, which is used for update
- p_search_fields -- the list of fields used in the WHERE clause, separated by commas,p_values_selected_search -- mas
- p_values_selected_search - the text of select statement, which is used to obtain updated values
- p_effective -- effective range of the update
- p_asserted - assertion for the update

Example 1:

```
select \* from bitemporal_internal.ll_bitemporal_update_select(
    'bitemp_tables',
    'devices',
    'device_descr',
    $$select device_descr from regular_tables.new_devices d
        where device_id=t.device_id$$,
    'device_id',
    $$select device_id from regular_tables.new_devices$$,
    temporal_relationships.timeperiod(now(), infinity),
    temporal_relationships.timeperiod(now(), infinity)
```

## Bitemporal Correction

There are four different bitemporal correction functions, which correctpond to the four bitemporal update functions described earlier, but instead of executing bitemporal update, they execute bitemporal correction. Please refer to the Bitemporal presentation in the same directory for more explanation how these two are different. Most of the time application developers need only bitemporal update.

Bitemporal correction functions:

```
bitemporal_internal.ll_bitemporal_correction(
    p_table TEXT,
    p_list_of_fields TEXT,
    p_list_of_values TEXT,
    p_search_fields TEXT,
    p_search_values TEXT,
    p_effective temporal_relationships.timeperiod,
    p_now temporal_relationships.time_endpoint
);
```

Returns: INTEGER

```
bitemporal_internal.ll_bitemporal_correction(
    p_schema_name TEXT,
    p_table_name TEXT,
    p_list_of_fields TEXT,
    p_list_of_values TEXT,
    p_search_fields TEXT,
    p_search_values TEXT,
    p_effective temporal_relationships.timeperiod,
    p_now temporal_relationships.time_endpoint
);
```

Returns: INTEGER

```
bitemporal_internal.ll_bitemporal_correction_select(
    p_table TEXT,
    p_list_of_fields TEXT,
    p_values_selected_update TEXT,
    p_where TEXT,
    p_effective_at time_endpoint ,
    p_now time_endpoint
);
```

Returns: INTEGER

```
bitemporal_internal.ll_bitemporal_correction_select(
    p_schema_name TEXT,
    p_table_name TEXT,
    p_list_of_fields TEXT,
    p_values_selected_update TEXT,
    p_where TEXT,
    p_effective_at time_endpoint ,
    p_now time_endpoint
);
```

Returns: INTEGER

Input:

- p_schema_name - the name of the schema
- p_table_name - bitemporal table name
- p_table - the table name, including the schema name (for older version only)
- p_list_of_fields - a character string, containing the list of columns which will
  be updated, separated by commas
- p_list_of_values - a list of updated values, separated by commas.
- p_values_selected_update - the text of select statement, which is used for correction
- p_search_fields -- the list of fields used in the WHERE clause, separated by commas
- p_search_values - the list of values for the search fields
- p_where parameter is used to pass the whole WHERE clause, combining together p_search_fields and \* p_values_selected_search from the ll_bitemporal_update_select.
- p_effective -- effective range which is being corrected
- p_now assertion start time for correction

Output:

- number of corrected records

Example:

```
select \* from bitemporal_internal.ll_bitemporal_correction(
    'bi_temp_tables',
    'devices',
    'device_descr',
    $$'updated_descr_11'$$,
    'device_id' ,
    '11',
    '[01-01-2016, infinity)'::temporal_relationships.timeperiod
);
```

## ll_bitemporal_inactivate

Performs bitemporal inactivate operation. The selected record(s) will be modified so that there won't be any currently effective record(s), but this status will be still currently asserted. In other words, we can "see" the record(s) as being currently not effective

```
bitemporal_internal.ll_bitemporal_inactivate(
    p_table TEXT,
    p_search_fields TEXT,
    p_search_values TEXT,
    p_effective temporal_relationships.timeperiod, -- inactive starting
    p_asserted temporal_relationships.timeperiod -- will be asserted
);
```

Returns: INTEGER

Input:

- p_table_name - bitemporal table name (including schema)
- p_search_fields -- the list of fields used in the WHERE clause, separated by commas
- p_search_values - the list of values for the search fields
- p_effective temporal_relationships.timeperiod -- inactive starting (only lower(effective) is used)
- p_asserted temporal_relationships.timeperiod -- inactiveness will be asserted

Output:

- number of inactivated records

_NOTE: this function has only ONE signature (we might add more for consistency later)_

## ll_bitemporal_delete

Performs bitemporal delete (ends assertion interval for selected records, making them 'invisible')

```
bitemporal_internal.ll_bitemporal_delete(
    p_table,
    p_search_fields TEXT,
    p_search_values TEXT,
    p_asserted temporal_relationships.timeperiod
);
```

Returns: INTEGER

Input:

- p_table_name - bitemporal table name (including schema)
- p_search_fields -- the list of fields used in the WHERE clause, separated by commas
- p_search_values - the list of values for the search fields
- p_asserted temporal_relationships.timeperiod - lower(asserted) will be the end of assertion

Output:

- number of deleted records

_NOTE: this function does not have a PG 10 compatiable version yet, please use delete_select, while we are working on the new version_

Example:

```
select \* from bitemporal_internal.ll_bitemporal_delete(
    'bi_temp_tables.devices',
    'device_id',
    $$1$$,
    '[3016-04-04 21:30, infinity)' -- deleteion with future assertion
);
```

## ll_bitemporal_delete_select

Bitemporarily delete (end assertion) for the records, which are selected using p_values_selected_search

```
bitemporal_internal.ll_bitemporal_delete_select(
    p_table TEXT,
    p_search_fields TEXT,
    p_values_selected_search TEXT,
    p_asserted temporal_relationships.timeperiod
);
```

Returns: INTEGER

Input:

- p_table_name - bitemporal table name (including schema)
- p_search_fields -- the list of fields used in the WHERE clause, separated by commas
- p_values_selected_search - the text of select statement, which is used to identify records to be deleted
- p_asserted temporal_relationships.timeperiod - lower(asserted) will be the end of assertion

Output:

- number of records deleted
