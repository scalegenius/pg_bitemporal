CREATE OR REPLACE FUNCTION bitemporal_internal.ll_is_bitemporal_table(p_table text) RETURNS boolean   immutable
AS
 $$
DECLARE 
v_schemaname text;
v_tablename text;
BEGIN  
        SELECT split_part(p_table, '.', 1) INTO v_schemaname;
        SELECT split_part(p_table, '.', 2) INTO v_tablename;
 RETURN 
 (  SELECT 
         coalesce(max(CASE WHEN a.attname='asserted' THEN 1 ELSE 0 END),0) +
         coalesce(max(CASE WHEN a.attname='effective' THEN 1 ELSE 0 END),0)=2
         AND exists (select 1 from pg_attribute ac
          JOIN pg_class cc
                     ON ac.attrelid=cc.oid
                     and  ac.attname='row_created_at'
                     JOIN pg_namespace n ON n.oid = cc.relnamespace
                     and n.nspname=v_schemaname AND  cc.relname=v_tablename)
   FROM pg_class c 
     JOIN pg_namespace n ON n.oid = c.relnamespace and relkind='i'
     join pg_am am ON am.oid=c.relam
     join pg_index x ON c.oid=x.indexrelid 
     and amname='gist'
     and indisexclusion='true'
     JOIN pg_class cc ON cc.oid = x.indrelid
     join pg_attribute a ON a.attrelid=c.oid
     join pg_type t ON a.atttypid=t.oid
  --   join pg_attribute ac ON ac.attrelid=cc.oid
 WHERE  n.nspname=v_schemaname AND  cc.relname=v_tablename);
  END;    
$$ LANGUAGE plpgsql;
