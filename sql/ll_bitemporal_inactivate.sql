CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_inactivate(p_table text
, p_search_fields TEXT  -- search fields
, p_search_values TEXT  --  search values
, p_effective temporal_relationships.timeperiod -- inactive starting
, p_asserted temporal_relationships.timeperiod -- will be asserted
)
RETURNS void
AS
$BODY$
DECLARE
v_list_of_fields_to_insert text:=' ';
v_list_of_fields_to_insert_excl_effective text;
v_table_attr text[];
v_now timestamptz:=now();-- so that we can reference this time
BEGIN 
 IF lower(p_asserted)<v_now::date --should we allow this precision?...
    OR upper(p_asserted)< 'infinity'
 THEN RAISE EXCEPTION'Asserted interval starts in the past or has a finite end: %', p_asserted
  ; 
  RETURN;
 END IF;
 IF (bitemporal_internal.ll_check_bitemporal_update_conditions(p_table 
                                                       ,p_search_fields 
                                                       ,p_search_values
                                                       ,p_effective)  =0 )
 THEN RAISE EXCEPTION'Nothing to inactivate: % = %, effective %', p_search_fields, p_search_values, p_effective; 
  RETURN;
 END IF;   

v_table_attr := bitemporal_internal.ll_bitemporal_list_of_fields(p_table);
IF  array_length(v_table_attr,1)=0
      THEN RAISE EXCEPTION 'Empty list of fields for a table: %', p_table; 
  RETURN;
 END IF;
v_list_of_fields_to_insert_excl_effective:= array_to_string(v_table_attr, ',','');
v_list_of_fields_to_insert:= v_list_of_fields_to_insert_excl_effective||',effective';
  

--end assertion period for the old record(s)

EXECUTE format($u$ UPDATE %s SET asserted = temporal_relationships.timeperiod_range(lower(asserted), lower(%L::timeperiod), '[)')
                    WHERE ( %s )=( %s ) AND (temporal_relationships.is_overlaps(effective, %L)
                                       OR 
                                       temporal_relationships.is_meets(effective, %L)
                                       OR 
                                       temporal_relationships.has_finishes(effective, %L))
                                       AND now()<@ asserted  $u$  
          , p_table
          , p_asserted
          , p_search_fields
          , p_search_values
          , p_effective
          , p_effective
          , p_effective);
          
 --insert new assertion rage with old values and effective-ended
 
 
EXECUTE format($i$INSERT INTO %s ( %s, effective, asserted )
                SELECT %s ,temporal_relationships.timeperiod_range(lower(effective), lower(%L::timeperiod),'[)') ,%L
                  FROM %s WHERE ( %s )=( %s ) AND (temporal_relationships.is_overlaps(effective, %L)
                                       OR 
                                       temporal_relationships.is_meets(effective, %L)
                                       OR 
                                       temporal_relationships.has_finishes(effective, %L))
                                       AND upper(asserted)=lower(%L::timeperiod) $i$  
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
          , p_effective
          , p_asserted
);

END;
$BODY$ LANGUAGE plpgsql;

