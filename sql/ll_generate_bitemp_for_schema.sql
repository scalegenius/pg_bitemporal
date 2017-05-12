create or replace function bitemporal_internal.ll_generate_bitemp_for_schema(p_schema_name text)
returns text as
$BODY$
declare
v_rec record;
v_rec2 record;
v_table_definition text;
v_business_key text;
v_business_key_def int2[];
v_all_tables text:=' ';
begin
for v_rec in (select c.relname, lower(c.relname) as stg_name,
c.oid from pg_class c   
JOIN pg_namespace n ON n.oid = c.relnamespace and relkind='r' 
and n.nspname=p_schema_name
order by 1)
loop
v_table_definition:= format(
$text$select * from 
bitemporal_internal.ll_create_bitemporal_table ('%s_bitemporal',
%L,
'$text$, 
p_schema_name,
v_rec.stg_name);
select conkey into v_business_key_def from pg_constraint where conrelid=v_rec.oid
and contype='p';
--raise notice '%', v_business_key_def;
v_business_key:=NULL;
for v_rec2 in (select 
attnum,
attname, typname,
case typname when 'varchar' then '('||(atttypmod-4)::varchar||')' 
when 'numeric' then '(10,2)'
else ' '
end  as field_length
from pg_attribute ac 
join pg_type t ON ac.atttypid=t.oid
and ac.attrelid =v_rec.oid and attnum>0    
order by attnum )
loop
if v_rec2.attnum>1 then 
v_table_definition:=
v_table_definition||',';
end if;
v_table_definition:=
v_table_definition||format($text$%s %s %s
$text$,
v_rec2.attname, 
v_rec2.typname,
v_rec2.field_length); 
if v_rec2.attnum = any (v_business_key_def)
then if  v_business_key is null
then v_business_key:=v_rec2.attname ;else 
   v_business_key:=v_business_key||','||v_rec2.attname;
   end if;
 end if;  
 
end loop;
v_table_definition:= v_table_definition|| format(
$text$', 
%L
);

$text$, 
v_business_key);

v_all_tables:=v_all_tables|| v_table_definition;
end loop;
return v_all_tables;
end;
$BODY$
  LANGUAGE plpgsql;