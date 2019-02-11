CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_correction_select(
    p_table text,
    p_list_of_fields text,
    p_values_selected_update text,
    p_where text,
  --  p_values_selected_search text,
    p_effective_at time_endpoint ,
    p_now time_endpoint)
  RETURNS integer AS
$BODY$
DECLARE
v_sql  text;
  v_rowcount INTEGER:=0;
  v_list_of_fields_to_insert text;
  v_table_attr text[];
  v_now temporal_relationships.time_endpoint:=p_now ;-- for compatiability with the previous version
  v_effective_at temporal_relationships.time_endpoint:=p_effective_at ;
BEGIN
 v_table_attr := bitemporal_internal.ll_bitemporal_list_of_fields(p_table);
 IF  array_length(v_table_attr,1)=0
      THEN RAISE EXCEPTION 'Empty list of fields for a table: %', p_table; 
  RETURN v_rowcount;
 END IF;

 v_list_of_fields_to_insert:= array_to_string(v_table_attr, ',','');

 EXECUTE 
 --v_sql:=
 format($u$ UPDATE %s t SET asserted = temporal_relationships.timeperiod_range(lower(asserted), %L, '[)')
                    WHERE  %s   AND %L::timestamptz <@ t.effective
                          AND upper(t.asserted)='infinity' 
                          AnD lower(t.asserted)<%L$u$  --end assertion period for the old record(s), if any
          , p_table
          , v_now
          , p_where
          ,v_effective_at
          , v_now);      
--      raise notice 'sql %', v_sql;  

 EXECUTE 
-- v_sql:=
 format($i$INSERT INTO %s ( %s, effective, asserted )
                SELECT %s ,effective, temporal_relationships.timeperiod_range(now(),
                 'infinity', '[)')
                  FROM %s WHERE  %s  AND %L::timestamptz <@ effective
                          AND upper(asserted)= %L 
                                 $i$  --insert new assertion rage with old values where applicable 
          , p_table
          , v_list_of_fields_to_insert
          , v_list_of_fields_to_insert
          , p_table
          , p_where
          , v_effective_at
          , v_now
);
--raise notice 'sql%', v_sql;  

 EXECUTE 
-- v_sql:=   
 format($uu$UPDATE %s t SET ( %s ) = ( %s ) WHERE  %s  
                           AND  %L::timestamptz <@ t.effective
                           AND upper(asserted)='infinity'
                           RETURNING * $uu$  --update new assertion rage with new values
          , p_table
          , p_list_of_fields
          , p_values_selected_update
          , p_where
          , v_effective_at
     ) ;
   --  raise notice 'sql%', v_sql;  
 GET DIAGNOSTICS v_rowcount:=ROW_COUNT; 
 RETURN v_rowcount;
END;
$BODY$
  LANGUAGE plpgsql ;