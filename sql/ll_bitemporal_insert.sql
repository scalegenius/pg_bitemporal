CREATE OR REPLACE FUNCTION ll_bitemporal_insert(p_table text
,p_list_of_fields text
,p_list_of_values TEXT
,p_effective tsrange
,p_asserted tsrange) 
RETURNS void
AS
 $BODY$
DECLARE 
v_list_of_fields text:=' ';
v_list_of_values text :=' ';
v_sql_final text:='INSERT INTO ';
BEGIN  
  v_list_of_fields:=$$($$||p_list_of_fields||$$,effective, asserted)$$;
  v_list_of_values:=$$($$||p_list_of_values ||$$, '$$||p_effective||$$', '$$||p_asserted||$$') $$;
  v_sql_final:=v_sql_final||
                p_table ||   
                v_list_of_fields || 
                $$ VALUES $$ || 
                v_list_of_values;
     EXECUTE v_sql_final;           
     END;    
$BODY$ LANGUAGE plpgsql;
