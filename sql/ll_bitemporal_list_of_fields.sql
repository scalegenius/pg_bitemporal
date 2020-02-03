create or replace function  bitemporal_internal.generate_ll_bitemporal_list_of_fields()
returns boolean as
$GBODY$
DECLARE v_sql text;
begin
IF current_setting('server_version_num')::int <120000
THEN 
v_sql:=
$txt$ CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_list_of_fields(p_table text) RETURNS text[]
AS
$BODY$
BEGIN
RETURN ( array(SELECT attname
                          FROM (SELECT * FROM pg_attribute
                                  WHERE attrelid=p_table::regclass AND attnum >0) pa
                          LEFT OUTER JOIN pg_attrdef pad ON adrelid=p_table::regclass
                                                        AND adrelid=attrelid
                                                        AND pa.attnum=pad.adnum
                          WHERE (adsrc NOT LIKE 'nextval%' OR adsrc IS NULL)
                                AND attname !='asserted'
                                AND attname !='effective'
                                AND attname !='row_created_at'
                                and attname not like '%dropped%'
                        ORDER BY pa.attnum));
END;                        
$BODY$ LANGUAGE plpgsql
$txt$ ;
else  v_sql:=
$txt$
CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_list_of_fields(p_table text) RETURNS text[]
AS
$BODY$
BEGIN
RETURN ( array(SELECT attname
                          FROM (SELECT * FROM pg_attribute
                                  WHERE attrelid=p_table::regclass AND attnum >0) pa
                          LEFT OUTER JOIN pg_attrdef pad ON adrelid=p_table::regclass
                                                        AND adrelid=attrelid
                                                        AND pa.attnum=pad.adnum
                          WHERE (adbin IS NULL)
                                AND attname !='asserted'
                                AND attname !='effective'
                               -- AND attname !='row_created_at'
                                and attname not like '%dropped%'
                        ORDER BY pa.attnum));
END;                        
$BODY$ LANGUAGE plpgsql
$txt$;

end if;

execute (v_sql);
return  null;

end;
$GBODY$ LANGUAGE plpgsql;

select * from  bitemporal_internal.generate_ll_bitemporal_list_of_fields();
drop function bitemporal_internal.generate_ll_bitemporal_list_of_fields();
