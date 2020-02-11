  CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_correction_hist(p_schema_name text,
    p_table_name text,
    p_list_of_fields text,
    p_list_of_values text,
    p_search_fields text,
    p_search_values text,
    p_effective temporal_relationships.timeperiod,
    p_now temporal_relationships.time_endpoint )
  RETURNS integer AS
$BODY$
DECLARE
v_sql  text;
  v_rowcount INTEGER:=0;
  v_cnt int:=0;
  v_list_of_fields_to_insert text;
  v_table_attr text[];
  v_now temporal_relationships.time_endpoint:=p_now ;-- for compatiability with the previous version
  v_effective_start temporal_relationships.time_endpoint;
  v_serial_key text:=p_table_name||'_key';
  v_table text:=p_schema_name||'.'||p_table_name;
  v_keys int[];
  v_keys_old  int[];
  v_effective temporal_relationships.timeperiod;
BEGIN
 v_table_attr := bitemporal_internal.ll_bitemporal_list_of_fields(v_table);
 IF  array_length(v_table_attr,1)=0
      THEN RAISE EXCEPTION 'Empty list of fields for a table: %', v_table; 
  RETURN v_rowcount;
 END IF;

v_list_of_fields_to_insert:= array_to_string(v_table_attr, ',','');

execute format($$select distinct (effective)from %s 
      WHERE ( %s )=( %s ) AND %L::timestamptz<@effective
                          AND upper(asserted)='infinity' 
                          AND lower(asserted)<%L $$
          , v_table
          , p_search_fields
          , p_search_values
          , lower(p_effective)
          , v_now
          )
      into v_effective ;
	--  raise notice 'effective:%',v_effective;
 IF lower(v_effective)<lower(p_effective)
   THEN  ---create new interval
  -- raise notice 'new interval:%', temporal_relationships.timeperiod(lower(p_effective), upper(v_effective));
  
  perform bitemporal_internal.ll_bitemporal_update(
 p_schema_name, 
 p_table_name, 
 p_search_fields,  /*split the record, do not change vaues)*/
 p_search_values,
 p_search_fields,
 p_search_values,
 temporal_relationships.timeperiod(lower(p_effective), upper(v_effective)),
 temporal_relationships.timeperiod(v_now, 'infinity'));
 
select  bitemporal_internal.ll_bitemporal_correction(
 p_schema_name, 
 p_table_name, 
 p_list_of_fields,
 p_list_of_values ,
p_search_fields,
 p_search_values,
 temporal_relationships.timeperiod(lower(p_effective), upper(v_effective)),
 v_now)  into v_cnt;

	v_effective_start:=upper(v_effective);	v_cnt:=1;
ELSE 
  v_effective_start:=lower(p_effective);
END IF;     
 
EXECUTE 
--v_sql:=
 format($u$ WITH updt AS (UPDATE %s SET asserted = temporal_relationships.timeperiod(lower(asserted), %L)
                    WHERE ( %s )=( %s ) AND lower(effective)>= %L::temporal_relationships.time_endpoint 
		                  and upper(effective) <=upper(%L::temporal_relationships.timeperiod)     ---is_included (effective )
                          AND upper(asserted)='infinity' 
                          AND lower(asserted)<%L returning %s )
                                      SELECT array_agg(%s) FROM updt
                                      $u$  --end assertion period for the old record(s), if any
          , v_table
          , v_now
          , p_search_fields
          , p_search_values
          , v_effective_start
          , p_effective
          , v_now
          , v_serial_key
          , v_serial_key
	   ) into v_keys_old;
--  raise notice 'sql%', v_sql;  

EXECUTE 
-- v_sql:=
 format($i$WITH inst AS (INSERT INTO %s ( %s, effective, asserted )
                SELECT %s ,effective, temporal_relationships.timeperiod_range(upper(asserted), 'infinity', '[)')
                  FROM %s WHERE ( %s )IN ( %s ) 
                                returning %s )
                                    SELECT array_agg(%s) FROM inst $i$  --insert new assertion rage with old values where applicable 
          , v_table
          , v_list_of_fields_to_insert
          , v_list_of_fields_to_insert
          , v_table
          , v_serial_key
          , coalesce(array_to_string(v_keys_old,','),'NULL')
          , v_serial_key
          , v_serial_key
)into v_keys;
--raise notice 'sql%', v_sql;  

--raise notice 'sql%', v_sql;  
if coalesce(array_to_string(v_keys_old,',')) IS NULL 
   then 
EXECUTE   format($uu$UPDATE %s SET ( %s ) = (SELECT %s ) WHERE ( %s ) = ( %s )
                           AND lower(effective)>= lower(%L::temporal_relationships.timeperiod) 
                           and upper(effective) <=upper(%L::temporal_relationships.timeperiod)     ---is_included (effective )
                         
                           AND upper(asserted)='infinity'
                            $uu$  --update new assertion rage with new values
          , v_table
          , p_list_of_fields
          , p_list_of_values
          , p_search_fields
          , p_search_values
          , p_effective
          ,p_effective
     ) 
	;

ELSE 
 EXECUTE 
-- v_sql:=   
 format($uu$UPDATE %s SET ( %s ) = ( SELECT %s ) WHERE ( %s ) IN ( %s )
                           $uu$  --update new assertion rage with new values
          , v_table
          , p_list_of_fields
          , p_list_of_values
          , v_serial_key
          ,coalesce(array_to_string(v_keys,','), 'NULL'));
          
   --  raise notice 'sql%', v_sql; 
 END IF;  
 GET DIAGNOSTICS v_rowcount:=ROW_COUNT; 
if coalesce(array_to_string(v_keys,',')) IS NULL 
then v_rowcount:=0;
end if;
 RETURN v_rowcount+v_cnt;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE; 


 
 CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_correction_hist(p_schema_name text,
    p_table_name text,
    p_list_of_fields text,
    p_list_of_values text,
    p_search_fields text,
    p_search_values text,
    p_effective temporal_relationships.timeperiod)
  RETURNS integer AS
  $BODY$
  declare v_rowcount int;
  begin
   select * into v_rowcount from  bitemporal_internal.ll_bitemporal_correction_hist(p_schema_name ,
    p_table_name ,
    p_list_of_fields ,
    p_list_of_values ,
    p_search_fields ,
    p_search_values,
    p_effective ,
    clock_timestamp() );
    return v_rowcount;
    END;
$BODY$
  LANGUAGE plpgsql VOLATILE;
 