CREATE OR REPLACE FUNCTION bitemporal_internal.ll_check_bitemporal_update_conditions(p_table text
,p_search_fields TEXT  -- search fields
,p_search_values TEXT  --  search values
,p_effective temporal_relationships.timeperiod  -- effective range of the update
) 
RETURNS integer
AS
$BODY$
DECLARE 
v_records_found integer;
---currently do not support checks for future assertion after existing future assertion
BEGIN 
EXECUTE format($s$ SELECT count(*) 
    FROM %s WHERE ( %s )=( %s ) AND  (temporal_relationships.is_overlaps(effective::temporal_relationships.timeperiod, %L::temporal_relationships.timeperiod)
                                       OR 
                                       temporal_relationships.is_meets(effective::temporal_relationships.timeperiod, %L::temporal_relationships.timeperiod)
                                       OR 
                                       temporal_relationships.has_finishes(effective::temporal_relationships.timeperiod, %L::temporal_relationships.timeperiod))
                                       AND now()<@ asserted  $s$ 
          , p_table
          , p_search_fields
          , p_search_values
          , p_effective
          , p_effective
          , p_effective) INTO v_records_found;
RETURN v_records_found;          
END;    
$BODY$ LANGUAGE plpgsql;

