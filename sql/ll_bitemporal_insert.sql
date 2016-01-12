CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_insert(p_table text
,p_list_of_fields text
,p_list_of_values TEXT
,p_effective tsrange
,p_asserted tsrange) 
RETURNS void
AS
 $BODY$
BEGIN
   EXECUTE format ($i$INSERT INTO %s (%s, effective, asserted )  
                 VALUES (%s,%L,%L) $i$
                ,p_table
                ,p_list_of_fields
                ,p_list_of_values
                ,p_effective
                ,p_asserted)
                ;
     END;    
$BODY$ LANGUAGE plpgsql;
