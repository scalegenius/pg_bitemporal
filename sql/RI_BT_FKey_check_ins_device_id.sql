
CREATE or replace FUNCTION bitemporal_internal.ri_bt_fkey_check_ins_device_id()
    RETURNS trigger
    LANGUAGE 'plpgsql'
    COST 100.0
AS $BODY$

/*  TG_ARGV[0] is a schema name
    TG_ARGV[1] is a table name
    TG_ARGV[2] is a column name
 */  
 DECLARE   v_value integer;
           v_result boolean;
BEGIN
    v_value:=NEW.device_id;
    execute format($ef$ select * from %s.validate_bitemporal_%s_%s(%s,
       $1, $2)$ef$  ,
       TG_ARGV[0],
       TG_ARGV[1],
       TG_ARGV[2],
       v_value
       ) into v_result
       using NEW.effective, NEW.asserted;
       
      if v_result is false then 
      RAISE EXCEPTION '% % foreign key constraint violated', TG_ARGV[2], NEW.device_id; 
       end if;
       return new;
    END;
    
$BODY$;
