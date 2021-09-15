
CREATE OR REPLACE FUNCTION bitemporal_internal.ll_create_bitemporal_table(
    p_schema text,
    p_table text,
    p_table_definition text,
    p_business_key text)
  RETURNS boolean AS
$BODY$
DECLARE
  v_sql  text;
  v_error text;
BEGIN
  v_sql := bitemporal_internal.ll_create_bitemporal_table_sql(
    p_schema,
    p_table,
    p_table_definition,
    p_business_key );
  EXECUTE v_sql;
  RETURN ('true');
  EXCEPTION WHEN OTHERS THEN
    GET STACKED DIAGNOSTICS v_error = MESSAGE_TEXT;
    RAISE NOTICE '%', v_error;
    RETURN ('false');
END;
$BODY$
  LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION bitemporal_internal.ll_create_bitemporal_table_sql(
    p_schema text,
    p_table text,
    p_table_definition text,
    p_business_key text)
  RETURNS text AS
$BODY$
DECLARE
  v_business_key_name text;
  v_business_key_gist text;
  v_serial_key_name text;
  v_serial_key text;
  v_pk_constraint_name text;
  v_table_definition text;
  v_business_key_array text[];
  i int;
  v_sql text DEFAULT '';
BEGIN
  v_serial_key :=p_table||'_key';
  v_serial_key_name :=v_serial_key ||' serial';
  v_pk_constraint_name:= p_table||'_pk';
  v_business_key_name :=p_table||'_'||translate(p_business_key, ', ','_')||'_assert_eff_excl';
  v_business_key_gist :=replace(p_business_key, ',',' WITH =,')||' WITH =, asserted WITH &&, effective WITH &&';

  v_table_definition :=replace (p_table_definition, ' serial', ' integer');
  v_business_key_array :=string_to_array(p_business_key, ',');

  v_sql := format($create$
CREATE TABLE %s.%s (
                 %s
                 ,%s
                 ,effective temporal_relationships.timeperiod NOT NULL
                 ,asserted temporal_relationships.timeperiod  NOT NULL
                 ,row_created_at timestamptz NOT NULL DEFAULT now()
                 ,CONSTRAINT %s PRIMARY KEY (%s)
                 ,CONSTRAINT %s EXCLUDE
                   USING gist (%s)
                    )
                 $create$
                 ,p_schema
                 ,p_table
                 ,v_serial_key_name
                 ,v_table_definition
                  ,v_pk_constraint_name
                  ,v_serial_key
                 ,v_business_key_name
                 ,v_business_key_gist
                 ) ;
  i:=1;
  while v_business_key_array[i] is not null loop
    v_sql := v_sql || ';' ||
               format($alter$
                 ALTER TABLE %s.%s ALTER %s SET NOT NULL $alter$
                 ,p_schema
                 ,p_table
                 ,v_business_key_array[i]
                 ) ;
    i:=i+1;
  end loop;
  return v_sql;
END;
$BODY$
  LANGUAGE plpgsql;
