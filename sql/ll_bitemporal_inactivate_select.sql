 CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_inactivate_select(p_schema_name text,
  p_table_name text
,p_search_fields TEXT  -- search fields
,p_values_selected_search TEXT  --  search values selected
, p_effective temporal_relationships.timeperiod -- inactive starting
, p_asserted temporal_relationships.timeperiod -- will be asserted
)
RETURNS INTEGER
AS
$BODY$
DECLARE
v_sql text ;
v_rowcount INTEGER:=0;
v_list_of_fields_to_insert text:=' ';
v_list_of_fields_to_insert_excl_effective text;
v_table_attr text[];
v_now timestamptz:=now();-- so that we can reference this time
  v_keys int[];
  v_keys_old  int[];
  v_serial_key text:=p_table_name||'_key';
  v_table text:=p_schema_name||'.'||p_table_name;
 
BEGIN 
 IF lower(p_asserted)<v_now::date --should we allow this precision?...
    OR upper(p_asserted)< 'infinity'
 THEN RAISE EXCEPTION'Asserted interval starts in the past or has a finite end: %', p_asserted
  ; 
  RETURN v_rowcount;
 END IF;
v_table_attr := bitemporal_internal.ll_bitemporal_list_of_fields(v_table);
IF  array_length(v_table_attr,1)=0
      THEN RAISE EXCEPTION 'Empty list of fields for a table: %', v_table; 
  RETURN v_rowcount;
 END IF;
v_list_of_fields_to_insert_excl_effective:= array_to_string(v_table_attr, ',','');
v_list_of_fields_to_insert:= v_list_of_fields_to_insert_excl_effective||',effective';
  

--end assertion period for the old record(s)

EXECUTE
 format($u$ WITH updt AS (UPDATE %s SET asserted =
            temporal_relationships.timeperiod(lower(asserted), lower(%L::temporal_relationships.timeperiod))
                    WHERE ( %s )in( %s ) AND (temporal_relationships.is_overlaps(effective, %L)
                                                   
                                       OR 
                                       temporal_relationships.is_meets(effective, %L)
                                       OR 
                                       temporal_relationships.has_finishes(effective, %L))
                                      AND ---now()<@ asserted  
                                       (temporal_relationships.is_overlaps(asserted, %L) 
                                       OR 
                                       temporal_relationships.has_finishes(asserted, %L)) returning %s )
                                      SELECT array_agg(%s) FROM updt
                                      $u$  
          , v_table
          , p_asserted
          , p_search_fields
          , p_values_selected_search
          , p_effective
          , p_effective
          , p_asserted
          , p_asserted
          , v_serial_key
          , v_serial_key) into v_keys_old;
          
                    
                    
                    
                    
                    
                    
                 
 
 
 
EXECUTE format($i$INSERT INTO %s ( %s, effective, asserted )
                SELECT %s ,temporal_relationships.timeperiod(lower(effective), lower(%L::temporal_relationships.timeperiod)) ,%L
                  FROM %s WHERE ( %s )in ( %s )  $i$
          , v_table
          , v_list_of_fields_to_insert_excl_effective
          , v_list_of_fields_to_insert_excl_effective
          , p_effective
          , p_asserted
          , v_table
          , v_serial_key
          , coalesce(array_to_string(v_keys_old,','),'NULL')
);



GET DIAGNOSTICS v_rowcount:=ROW_COUNT; 
RETURN v_rowcount;
END;
$BODY$ LANGUAGE plpgsql;


