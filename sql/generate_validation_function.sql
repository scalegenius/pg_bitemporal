CREATE OR REPLACE FUNCTION bitemporal_internal.generate_validation_function(
p_schema_name text
,p_table_name text
,p_field_name text --PK or UK field
) 
RETURNS text
AS
$BODY$
DECLARE 
v_key_type text;
v_function_source text;
BEGIN
SELECT bitemporal_internal.validate_bitemporal_pk_uq(p_schema_name, p_table_name, p_field_name) into v_key_type;
IF v_key_type IS NULL THEN return NULL; END IF;
v_function_source :=format($source$
CREATE OR REPLACE FUNCTION %s.validate_bitemporal_%s_%s(
p_value %s
,p_effective temporal_relationships.timeperiod
,p_asserted temporal_relationships.timeperiod)
RETURNS boolean
AS $FUNCTION_BODY$
DECLARE
v_record record;
i integer:=0;
v_max_upper_effective temporal_relationships.time_endpoint;
v_max_upper_asserted temporal_relationships.time_endpoint;
v_min_lower_asserted temporal_relationships.time_endpoint;
BEGIN
SELECT max (upper(asserted)), min(lower(asserted)) INTO 
        v_max_upper_asserted,
        v_min_lower_asserted
  FROM %s.%s 
    WHERE %s=p_value
;
IF v_max_upper_asserted < 'infinity' OR v_min_lower_asserted > lower(p_asserted) 
   THEN RETURN false;
END IF;   
---start cycle, if there are future assertion 
WHILE true 
 LOOP --cycle through present and future asserted   
FOR v_record IN SELECT effective, upper(asserted) AS upper_asserted  FROM  %s.%s  
    WHERE %s=p_value
    AND temporal_relationships.has_includes(effective,p_effective) 
    AND lower(asserted) =v_min_lower_asserted
ORDER BY lower(effective), upper(effective)
LOOP
IF i=0 THEN 
	IF lower(p_effective)<lower(v_record.effective) 
	THEN
	    RETURN false;
	  --  raise notice 'false - effective start too late!';
	ELSE 
		   v_max_upper_effective:=upper(v_record.effective);
	END IF;  
END IF; --i=0
i:=i+1;
IF lower(v_record.effective) > v_max_upper_effective  --- there is a hole!
   THEN 
   RETURN false;
    -- raise notice 'false- gap in effective!';
   ELSE 
     IF upper(v_record.effective) > v_max_upper_effective ---sanity check
        THEN v_max_upper_effective:=  upper(v_record.effective) ; 
     END IF;
  END IF;         
END LOOP;
IF i=0 THEN
   return false;
END IF;
IF v_max_upper_effective< upper(p_effective)
   THEN 
   RETURN false;
END IF; 
IF v_record.upper_asserted = 'infinity' THEN
  RETURN true;
  ELSE v_min_lower_asserted:=v_record.upper_asserted;
END IF;  
END LOOP; 
END;
$FUNCTION_BODY$ LANGUAGE plpgsql;  
$source$
,p_schema_name 
,p_table_name 
,p_field_name
,v_key_type
,p_schema_name 
,p_table_name 
,p_field_name
,p_schema_name 
,p_table_name 
,p_field_name);
RETURN v_function_source; 
END;    
$BODY$ LANGUAGE plpgsql;

