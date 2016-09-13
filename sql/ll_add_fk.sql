create or replace function bitemporal_internal.ll_add_fk(
p_schema_name text,
p_table_name text,
p_column_name text,
p_source_schema_name text,
p_source_table_name text,
p_source_column_name text)
returns text 
as
$BODY$
declare
v_fk_constraint_name text;
v_function_name text;
begin
if bitemporal_internal.validate_bitemporal_pk_uq(
    p_source_schema_name,
    p_source_table_name,
    p_source_column_name) IS  NULL
  then
   return  'no primary key or unique constraint';
   exit;
  end if;
    
   v_fk_constraint_name:= bitemporal_internal.fk_constraint(
    p_column_name,
    p_source_table_name,
    p_source_column_name);
execute format ($a$ALTER TABLE %s.%s ADD %s $a$
                ,p_schema_name
                ,p_table_name
                ,v_fk_constraint_name) ;  
                
if not bitemporal_internal.ll_lookup_validation_function(
p_source_schema_name,
p_source_table_name,
p_source_column_name ) then
  select * into v_function_name from bitemporal_internal.ll_generate_fk_validate(
p_source_schema_name); 
end if;                         
  /*do not need to check whether this constraint already exists,
   it will error, if exists */
/*create trigger on insert/update and a trigger function */
return  v_fk_constraint_name;

END;    
$BODY$ 
LANGUAGE plpgsql;

   
