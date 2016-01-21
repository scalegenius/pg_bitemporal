CREATE OR REPLACE FUNCTION bitemporal_internal.ll_bitemporal_delete(p_table text
, p_search_fields TEXT  -- search fields
, p_search_values TEXT  --  search values
, p_asserted temporal_relationships.timeperiod -- will be asserted
)
RETURNS void
AS
$BODY$
BEGIN 
--end assertion period for the current records record(s)

EXECUTE format($u$ UPDATE %s SET asserted = temporal_relationships.timeperiod_range(lower(asserted), lower(%L::timeperiod), '[)')
                    WHERE ( %s )=( %s )AND lower(%L::timeperiod)<@ asserted  $u$  
          , p_table
          , p_asserted
          , p_search_fields   
          , p_search_values
          , p_asserted
          );
          

END;
$BODY$ LANGUAGE plpgsql;

