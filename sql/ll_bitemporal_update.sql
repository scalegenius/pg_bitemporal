CREATE OR REPLACE FUNCTION ll_bitemporal_update(p_table text
,p_list_of_fields text -- fields to update
,p_list_of_values TEXT  -- values to update with
,p_search_fields TEXT  -- search fields
,p_search_values TEXT  --  search values
,p_effective tstzrange  -- effective range of the update
,p_asserted tstzrange  -- assertion for the update
) 
RETURNS void
AS
$BODY$
DECLARE 
v_list_of_fields text:=' ';
v_list_of_values text :=' '; 
v_list_of_fields_to_insert text:=' ';
v_list_of_fields_to_insert_excl_effective text:=' ';
v_table_attr text[];
v_table_attr_excl_effective text[];
v_records_found integer;
v_now timestamptz:=now();-- so that we can reference this time
i integer :=1;
v_sql_debug text;
BEGIN 
 IF lower(p_asserted)<v_now::date --should we allow this precision?...
    OR upper(p_asserted)< 'infinity'
 THEN RAISE EXCEPTION'Asserted interval starts in the past or has a finite end: %', p_asserted
  USING ERRCODE = 'BT_Asserted_001'; 
  RETURN;
 END IF;
 IF (ll_check_bitemporal_update_conditions(p_table 
                                                       ,p_search_fields 
                                                       ,p_search_values
                                                       ,p_effective)  =0 )
 THEN RAISE EXCEPTION'Nothing to update, use INSERT or check effective: %', p_effective; 
  RETURN;
 END IF;   

v_table_attr := ll_bitemporal_list_of_fields(p_table);
v_table_attr_excl_effective := ll_bitemporal_list_of_fields_excl_effective(p_table);

  -- assert array_length(v_list_of_fields_to_insert,1) > 1 
v_list_of_fields_to_insert:= array_to_string(v_table_attr, ',','');
v_list_of_fields_to_insert_excl_effective:= array_to_string(v_table_attr_excl_effective, ',','');

--end assertion period for the old record(s)

EXECUTE format($u$ UPDATE %s SET asserted = tstzrange(lower(asserted), lower(%L::tstzrange), '[)')
                    WHERE ( %s )=( %s ) AND lower(%L::tstzrange) > lower(effective) 
                                        AND lower(%L::tstzrange) <= upper(effective)
                                        AND now()<@ asserted  $u$  
          , p_table
          , p_asserted
          , p_search_fields
          , p_search_values
          , p_effective
          , p_effective);
 --insert new assertion rage with old values and effective-ended
 
EXECUTE format($i$INSERT INTO %s ( %s, effective, asserted )
                SELECT %s ,tstzrange(lower(effective), lower(%L::tstzrange),'[)') ,%L
                  FROM %s WHERE ( %s )=( %s ) AND lower(%L::tstzrange) > lower(effective) 
                                              AND lower(%L::tstzrange) <= upper(effective)
                                              AND upper(asserted)=lower(%L::tstzrange) $i$  
          , p_table
          , v_list_of_fields_to_insert_excl_effective
          , v_list_of_fields_to_insert_excl_effective
          , p_effective
          , p_asserted
          , p_table
          , p_search_fields
          , p_search_values
          , p_effective
          , p_effective
          , p_asserted
);


---insert new assertion rage with old values and new effective range
 
EXECUTE format($i$INSERT INTO %s ( %s, effective, asserted )
                SELECT %s ,%L, %L
                  FROM %s WHERE ( %s )=( %s ) AND lower(%L::tstzrange) > lower(effective) 
                                              AND lower(%L::tstzrange) <= upper(effective)
                                              AND upper(asserted)=lower(%L::tstzrange) $i$  
          , p_table
          , v_list_of_fields_to_insert_excl_effective
          , v_list_of_fields_to_insert_excl_effective
          , p_effective
          , p_asserted
          , p_table
          , p_search_fields
          , p_search_values
          , p_effective
          , p_effective
          , p_asserted
);

--update new record(s) in new assertion rage with new values                                  
                                  
EXECUTE format($u$ UPDATE %s SET (%s) = (%L) 
                    WHERE ( %s )=( %s ) AND effective=%L
                                        AND asserted=%L $u$  
          , p_table
          , p_list_of_fields
          , p_list_of_values
          , p_search_fields
          , p_search_values
          , p_effective
          , p_asserted);
                                                                                               

END;    
$BODY$ LANGUAGE plpgsql;

