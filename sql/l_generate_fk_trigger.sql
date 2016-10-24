CREATE OR REPLACE FUNCTION bitemporal_internal.ll_generate_fk_trigger(
p_schema_name text,
p_table_name text,
p_column_name text) returns text
as $BODY_AUTO$
declare t text;
v_trigger_function_name text;
v_trigger_name text;
v_return_type text;
BEGIN
--raise notice 'entered';
v_trigger_function_name:=p_schema_name||'.RI_BT_FKey_check_ins_'||p_table_name||'_'||p_column_name;
v_return_type :=temporal_relationships.get_column_type(
	p_schema_name ,
	p_table_name ,                                            
	p_column_name );

--EXECUTE 
t:=format($execute$create or replace function %s () returns trigger as
$TR_BODY$
/*  TG_ARGV[0] is a schema name
    TG_ARGV[1] is a table name
    TG_ARGV[2] is a column name
 */  
 DECLARE v_value integer;
         v_effective temporal_relationships.timeperiod;
         v_asserted temporal_relationships.timeperiod;
BEGIN
    v_value:=NEW.device_id;
    v_effective:= NEW.effective;
    v_asserted:=NEW.asserted;
    if (select * from bi_temp_tables.validate_bitemporal_devices_device_id(v_value,
     v_effective, v_asserted) ) is false then 
      RAISE EXCEPTION 'device_id % foreign key constraint violated', NEW.device_id;
       /*execute format($ef$ select * from %s.validate_bitemporal_%s_%s(NEW.%s,NEW.effective, NEW.asserted)$ef$         ,
       TG_ARGV[0],
       TG_ARGV[1],
       TG_ARGV[2],
       TG_ARGV[2]
       );
       */
       end if;
       return new;
    END;
    $TR_BODY$
    LANGUAGE plpgsql;
$execute$
       , v_trigger_function_name
       
       , v_return_type
       , p_schema_name
       , p_table_name
       , p_column_name
       , p_schema_name
       , p_table_name
       , p_column_name);
  raise notice 'code:%',t;     
  return v_function_name;      
END;    
$BODY_AUTO$
  LANGUAGE plpgsql VOLATILE
 ;
