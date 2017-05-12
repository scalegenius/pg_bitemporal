create or replace function  temporal_relationships.get_column_type(
	p_schema_name text,
	p_table_name text,                                            
	p_column_name text)
RETURNS text
    LANGUAGE 'sql'
    COST 100.0
AS $function$
select t.typname  :: text
   from pg_class c 
     JOIN pg_attribute a ON c.oid = a.attrelid and 
        c.relname=p_table_name and attname=p_column_name
        JOIN pg_namespace n ON n.oid = c.relnamespace and n.nspname=p_schema_name
             join pg_type t on atttypid =t.oid;
      $function$;
