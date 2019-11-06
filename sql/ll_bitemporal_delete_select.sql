CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_delete_select(
	p_table text,
	p_search_fields text,
	p_values_selected_search text,
	p_asserted temporal_relationships.timeperiod)
    RETURNS integer
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE 
AS $BODY$

DECLARE
v_rowcount INTEGER:=0;
v_table_attr text[];
v_now timestamptz:=now();-- so that we can reference this time
BEGIN 
EXECUTE format($u$ UPDATE %s t    SET asserted =
            temporal_relationships.timeperiod(lower(asserted), lower(%L::temporal_relationships.timeperiod))
                    WHERE ( %s )in( %s ) AND  lower(%L::temporal_relationships.timeperiod)<@asserted   $u$  
          , p_table
          , p_asserted
          , p_search_fields
          , p_values_selected_search
          , p_asserted);
          
GET DIAGNOSTICS v_rowcount:=ROW_COUNT;  
RETURN v_rowcount;
END;    

$BODY$;

CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_delete_select(
	p_table text,
	p_selected_search text,
	p_asserted temporal_relationships.timeperiod)
    RETURNS integer
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE 
AS $BODY$

DECLARE
v_rowcount INTEGER:=0;
v_table_attr text[];
v_now timestamptz:=now();-- so that we can reference this time
BEGIN 
EXECUTE format($u$ UPDATE %s t    SET asserted =
            temporal_relationships.timeperiod(lower(asserted), lower(%L::temporal_relationships.timeperiod))
                    WHERE ( %s ) AND  lower(%L::temporal_relationships.timeperiod)<@asserted   $u$  
          , p_table
          , p_asserted
          , p_selected_search
          , p_asserted);
          
GET DIAGNOSTICS v_rowcount:=ROW_COUNT;  
RETURN v_rowcount;
END;    

$BODY$;

