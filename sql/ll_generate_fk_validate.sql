CREATE OR REPLACE FUNCTION bitemporal_internal.ll_generate_fk_validate(
p_schema_name text,
p_table_name text,
p_column_name text) returns text
as $BODY_AUTO$
declare t text;
v_function_name text;
--v_return_type text;
BEGIN
v_function_name:='validate_bt_'||p_table_name||'_'||p_column_name;
/*v_return_type :=temporal_relationships.get_column_type(
	p_schema_name ,
	p_table_name ,                                            
	p_column_name );*/

--EXECUTE 
t:=format($execute$
create or replace function %s.%s(    
    p_value anyelement,
    p_effective temporal_relationships.timeperiod,
    p_asserted temporal_relationships.timeperiod)
  RETURNS boolean AS
$BODY$
declare
v_record record;
i integer:=0;
v_min_low_effective temporal_relationships.time_endpoint;
v_max_upper_effective temporal_relationships.time_endpoint;
v_min_low_asserted temporal_relationships.time_endpoint;
v_max_upper_asserted temporal_relationships.time_endpoint;
begin
for v_record in select effective from
    %s.%s  where %s=p_value  
   and temporal_relationships.has_includes(effective, p_effective) 
   and temporal_relationships.has_includes(asserted ,p_asserted )
order by lower(effective), upper(effective)
loop
if i=0 then 
	if lower(p_effective)<lower(v_record.effective) 
	then
	    return false;
	else 
	   v_min_low_effective:= lower(v_record.effective);
	   v_max_upper_effective:=upper(v_record.effective);
	end if;  
     end if; 
i:=i+1;
if lower(v_record.effective) > v_max_upper_effective  
   then 
     raise notice 'false- gap in effective!';
     return  false;
   else 
     if upper(v_record.effective) > v_max_upper_effective ---sanity check
        then v_max_upper_effective:=  upper(v_record.effective) ; 
     end if;
  end if;         
end loop;
if i=0 then
 return  false;
end if;
if v_max_upper_effective< upper(p_effective) then
  return false;
   end if; 
i:=0;
for v_record in select asserted from
 %s.%s  where %s=p_value
 and temporal_relationships.has_includes(effective,p_effective) 
 and temporal_relationships.has_includes(asserted,p_asserted )
order by lower(asserted), upper(asserted)
loop
if i=0 then 
	if lower(p_asserted)<lower(v_record.asserted) 
	then
	    return false;
	else 
	   v_min_low_asserted:= lower(v_record.asserted);
	   v_max_upper_asserted:=upper(v_record.asserted);
	end if;  
     end if; 
i:=i+1;
if lower(v_record.asserted) > v_max_upper_asserted  
   then 
     return false;
   else 
     if upper(v_record.asserted) > v_max_upper_asserted ---sanity check
        then v_max_upper_asserted:=  upper(v_record.asserted) ; 
     end if;
  end if;         
end loop;
if i=0 then
return  false;
end if;
if v_max_upper_asserted< upper(p_asserted) then
     return false;
end if; 
 return true;
end;
$BODY$
LANGUAGE plpgsql
$execute$
       , p_schema_name
       , v_function_name
       , v_return_type
       , p_schema_name
       , p_table_name
       , p_column_name
       , p_schema_name
       , p_table_name
       , p_column_name);
  raise notice 'code:%',t;     
  return v_function_name;      
END;    
$BODY_AUTO$
  LANGUAGE plpgsql VOLATILE
 ;
