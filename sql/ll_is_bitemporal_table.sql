CREATE OR REPLACE FUNCTION ll_is_bitemporal_table(p_table text) RETURNS boolean   immutable
AS
 $$
DECLARE 
--inp_param_array varchar(50) array;
v_schemaname text;
v_tablename text;
--v_sql_final text;
BEGIN  
--SELECT regexp_split_to_array(v_input_string, ' ') INTO inp_param_array;
IF  position('.' in p_table) =0 
   THEN v_schemaname:='public'; 
   			v_tablename:=p_table;
   ELSE 			
        SELECT coalesce(split_part(p_table, '.', 1), 'public') INTO v_schemaname;
        SELECT split_part(p_table, '.', 2) INTO v_tablename;
   END IF;
 RETURN 
 (SELECT 
         coalesce(max(CASE WHEN t.typname='tsrange' AND attname='asserted' THEN 1 ELSE 0 END),0) +
         coalesce(max(CASE WHEN t.typname='tsrange' AND attname='effective' THEN 1 ELSE 0 END),0)=2
         AND
         bool_and(indisexclusion) AND  bool_and(amname='gist')
  FROM pg_class c 
     JOIN pg_namespace n ON n.oid = c.relnamespace and relkind='i'
     join pg_am am ON am.oid=c.relam
     join pg_index x ON c.oid=x.indexrelid ---oid NOT NULL,
     JOIN pg_class cc ON cc.oid = x.indrelid
     join pg_attribute ON attrelid=c.oid
     join pg_type t ON atttypid=t.oid
 WHERE  n.nspname=v_schemaname AND  cc.relname=v_tablename);
 END;    
$$ LANGUAGE plpgsql;
