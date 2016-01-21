CREATE OR REPLACE FUNCTION bitemporal_internal.ll_create_bitemporal_table(p_table text, p_table_definition text, p_business_key text) 
RETURNS void 
AS
 $$
DECLARE 
v_business_key_name text;
v_business_key_gist text;
BEGIN
v_business_key_name :=translate(p_business_key, ', ','_')||'_asserted_effective_excl';
v_business_key_gist :=replace(p_business_key, ',',' WITH =,')||' WITH =, asserted WITH &&, effective WITH &&';
--raise notice 'gist %',v_business_key_gist;
EXECUTE format($create$
CREATE TABLE %s (
                 %s
                 ,effective temporal_relationships.timeperiod
                 ,asserted temporal_relationships.timeperiod
                 ,row_created_at timestamptz NOT NULL
                 ,CONSTRAINT %s EXCLUDE 
                   USING gist (%s))
                 $create$
                 ,p_table
                 ,p_table_definition
                 ,v_business_key_name
                 ,v_business_key_gist
                 ) ;
 return ;               
END;
$$ LANGUAGE plpgsql;
