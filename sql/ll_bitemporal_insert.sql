CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_insert(p_table text
,p_list_of_fields text
,p_list_of_values TEXT
,p_effective temporal_relationships.timeperiod 
,p_asserted temporal_relationships.timeperiod ) 
RETURNS RECORD
AS
 $BODY$
DECLARE
v_record RECORD;
BEGIN
 EXECUTE format ($i$INSERT INTO %s (%s, effective, asserted )  
                 VALUES (%s,%L,%L) RETURNING * $i$
                ,p_table
                ,p_list_of_fields
                ,p_list_of_values
                ,p_effective
                ,p_asserted) INTO v_record;
      RETURN v_record;          
     END;    
$BODY$ LANGUAGE plpgsql;
