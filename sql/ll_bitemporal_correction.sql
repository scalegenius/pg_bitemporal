CREATE OR REPLACE FUNCTION ll_bitemporal_correction(p_table text
,p_list_of_fields text -- fields to update
,p_list_of_values TEXT  -- values to update with
,p_search_fields TEXT  -- search fields
,p_search_values TEXT  --  search values
,p_effective tstzrange  -- effective range we are correcting
--,p_asserted tstzrange  -- does not look like we need it
) 
RETURNS void
AS
$BODY$
DECLARE 
v_list_of_fields text:=' ';
v_list_of_values text :=' '; 
v_list_of_fields_to_insert text:=' ';
v_table_attr text[];
v_now timestamptz:=now();-- so that we can reference this time
i integer :=1;
BEGIN  	            
  v_table_attr := array(SELECT attname FROM (SELECT * FROM pg_attribute WHERE 
                                                     attrelid=p_table::regclass AND attnum >0) pa
                                                  LEFT OUTER JOIN pg_attrdef pad ON adrelid='bi_temp_tables.devices'::regclass
                                                        AND adrelid=attrelid 
                                                        AND pa.attnum=pad.adnum
                                                   WHERE (adsrc NOT LIKE '%nextval%' OR adsrc IS NULL) 
                                                          AND attname !='asserted'
                                                   ORDER BY pa.attnum );
                    
  WHILE v_table_attr[i]IS NOT NULL
      LOOP 
        v_list_of_fields_to_insert:=v_list_of_fields_to_insert || v_table_attr[i] ||$$,$$;
        i:=i+1; 
      END LOOP;
  EXECUTE 
   $$UPDATE $$||
	     p_table||$$ SET asserted =tstzrange(lower(asserted),  $$||quote_literal(v_now)
	     	      ||$$, '[)') WHERE ($$
	            ||p_search_fields
	            ||$$)=($$
	            ||p_search_values
	            ||$$) AND effective = $$
	            ||quote_literal(p_effective)
	            ||$$ AND upper(asserted)='infinity'$$;  --end assertion period for the old record(s)

 EXECUTE $$INSERT INTO $$||p_table||$$($$|| v_list_of_fields_to_insert ||  $$asserted)  
                SELECT $$||v_list_of_fields_to_insert || $$ tstzrange(upper(asserted), 'infinity', '[)')
                  FROM $$||p_table||$$ WHERE ($$||
                              p_search_fields||
                                      $$)=($$||
                              p_search_values||
                       $$) AND effective = $$||
                                  quote_literal(p_effective)||
                                  $$ AND upper(asserted)=$$||quote_literal(v_now);  --insert new assertion rage with old values 
                                  
    EXECUTE $$UPDATE $$||p_table|| $$ SET ($$||p_list_of_fields||$$)=($$
                                             ||p_list_of_values 
                                             ||$$) WHERE ($$
                                             || p_search_fields||
                                      $$)=($$||
                              p_search_values||
                       $$) AND effective = $$||
                                 quote_literal(p_effective)||
                                  $$ AND upper(asserted)='infinity'$$;  --update new assertion rage with new values                                                                 

END;    
$BODY$ LANGUAGE plpgsql;

