CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_correction(p_table text
, p_list_of_fields text -- fields to update
, p_list_of_values TEXT  -- values to update with
, p_search_fields TEXT  -- search fields
, p_search_values TEXT  --  search values
, p_effective temporal_relationships.timeperiod  -- effective range we are correcting
)
RETURNS INTEGER
--does not check whether this is a future assert, can be used to correct future asserted as well
AS
$BODY$
DECLARE
  v_record_count INTEGER;
  v_list_of_fields_to_insert text;
  v_table_attr text[];
  v_now timestamptz              :=now();-- so that we can reference this time as a constant
BEGIN
 v_table_attr := bitemporal_internal.ll_bitemporal_list_of_fields(p_table);
 IF  array_length(v_table_attr,1)=0
      THEN RAISE EXCEPTION 'Empty list of fields for a table: %', p_table; 
  RETURN 0;
 END IF;

 v_list_of_fields_to_insert:= array_to_string(v_table_attr, ',','');

 EXECUTE format($u$ UPDATE %s SET asserted = temporal_relationships.timeperiod_range(lower(asserted), %L, '[)')
                    WHERE ( %s )=( %s ) AND effective = %L
                          AND upper(asserted)='infinity' $u$  --end assertion period for the old record(s)
          , p_table
          , v_now
          , p_search_fields
          , p_search_values
          , p_effective);

 EXECUTE format($i$INSERT INTO %s ( %s, effective, asserted )
                SELECT %s ,effective, temporal_relationships.timeperiod_range(upper(asserted), 'infinity', '[)')
                  FROM %s WHERE ( %s )=( %s ) AND effective = %L
                          AND upper(asserted)= %L $i$  --insert new assertion rage with old values 
          , p_table
          , v_list_of_fields_to_insert
          , v_list_of_fields_to_insert
          , p_table
          , p_search_fields
          , p_search_values
          , p_effective
          , v_now
);

    EXECUTE format($uu$UPDATE %s SET ( %s ) = ( %s ) WHERE ( %s ) = ( %s )
                           AND effective = %L
                           AND upper(asserted)='infinity'
                           RETURNING * $uu$  --update new assertion rage with new values
          , p_table
          , p_list_of_fields
          , p_list_of_values
          , p_search_fields
          , p_search_values
          , p_effective
     ) 
 RETURN 1;        
END;
$BODY$ LANGUAGE plpgsql;