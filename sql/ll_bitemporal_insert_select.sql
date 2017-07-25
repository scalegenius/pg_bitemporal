CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_insert_select(p_table text
,p_list_of_fields text
,p_select TEXT
,p_effective temporal_relationships.timeperiod 
,p_asserted temporal_relationships.timeperiod ) 
RETURNS INTEGER
AS
 $BODY$
DECLARE
v_rowcount INTEGER;
BEGIN
 EXECUTE format ($i$INSERT INTO %s (%s, effective, asserted )  
                 select a.*, %L,%L
                 from (%s) a RETURNING * $i$
                ,p_table
                ,p_list_of_fields
                ,p_effective
                ,p_asserted
                ,p_select) ;
     GET DIAGNOSTICS v_rowcount:=ROW_COUNT; 
     RETURN v_rowcount;         
     END;    
$BODY$ LANGUAGE plpgsql;

