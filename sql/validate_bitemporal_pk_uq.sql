CREATE OR REPLACE FUNCTION bitemporal_internal.validate_bitemporal_pk_uq(p_schema text
,p_table text
,p_field text) 
RETURNS text IMMUTABLE
AS
$BODY$
DECLARE 
v_is_valid boolean;
v_type text;
BEGIN
SELECT
(coalesce(max(CASE WHEN a.attname='asserted' THEN 1 ELSE 0 END),0) +
         coalesce(max(CASE WHEN a.attname='effective' THEN 1 ELSE 0 END),0)+
         coalesce(max(CASE WHEN a.attname=p_field THEN 1 ELSE 0 END),0)=3
         --AND
        -- bool_and(indisexclusion) AND  bool_and(amname='gist') 
        ) as is_valid,
         max(CASE WHEN ac.attname=p_field THEN tc.typname ELSE null END) as check_type
         INTO v_is_valid, v_type      
  FROM pg_class c 
     JOIN pg_namespace n ON n.oid = c.relnamespace and relkind='i'
     join pg_am am ON am.oid=c.relam
     join pg_index x ON c.oid=x.indexrelid ---oid NOT NULL,
     JOIN pg_class cc ON cc.oid = x.indrelid
     join pg_attribute a ON a.attrelid=c.oid
     join pg_type t ON a.atttypid=t.oid
     join pg_attribute ac ON ac.attname=a.attname and ac.attrelid=cc.oid
     join pg_type tc ON ac.atttypid=tc.oid
 WHERE  n.nspname=p_schema AND  cc.relname=p_table
 and amname='gist' and indisexclusion;
 IF v_is_valid THEN return v_type;
 ELSE return NULL;
 END IF;
 END;    
$BODY$ LANGUAGE plpgsql;

