create or replace function bitemporal_internal.ll_lookup_validation_function(
p_schema_name text,
p_table_name text,
p_column_name text)
returns boolean immutable as
 $$select count(*)=1 from 
        pg_proc p 
     join pg_namespace n ON n.oid = p.pronamespace 
     and proname ='validate_bt_'||p_table_name||'_'||p_column_name 
     and n.nspname=p_schema_name
     ;$$
language sql;