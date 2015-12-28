CREATE OR REPLACE FUNCTION ll_bitemporal_correction(p_table text
, p_list_of_fields text -- fields to update
, p_list_of_values TEXT  -- values to update with
, p_search_fields TEXT  -- search fields
, p_search_values TEXT  --  search values
, p_effective tstzrange  -- effective range we are correcting
)
RETURNS void
AS
$BODY$
DECLARE
  v_list_of_fields text          :=' ';
  v_list_of_values text          :=' ';
  v_list_of_fields_to_insert text:=' ';
  v_table_attr text[];
  v_now timestamptz              :=now();-- so that we can reference this time as a constant
  i integer                      := 1;
BEGIN
  v_table_attr := ll_bitemporal_list_of_fields(p_table);

  -- assert array_length(v_list_of_fields_to_insert,1) > 1 
  v_list_of_fields_to_insert:= array_to_string(v_table_attr, ',','');

 EXECUTE format($u$ UPDATE %I SET asserted = tstzrange(lower(asserted), %L, '[)')
                    WHERE ( %s )=( %s ) AND effective = %L
                          AND upper(asserted)='infinity' $u$  --end assertion period for the old record(s)
          , p_table
          , v_now
          , p_search_fields
          , p_search_values
          , p_effective);

 EXECUTE format($i$INSERT INTO %I ( %s, asserted )
                SELECT %s , tstzrange(upper(asserted), 'infinity', '[)')
                  FROM %I WHERE ( %s )=( %s ) AND effective = %L
                          AND upper(asserted)= %L $i$  --insert new assertion rage with old values 
          , p_table
          , v_list_of_fields_to_insert
          , v_list_of_fields_to_insert
          , p_table
          , p_search_fields
          , p_search_values
          , p_effective
          , v_now
);

    EXECUTE format($uu$UPDATE %I SET ( %s ) = ( %s ) WHERE ( %s ) = ( %s )
                           AND effective = %L
                           AND upper(asserted)='infinity'$uu$  --update new assertion rage with new values
          , p_table
          , p_list_of_fields
          , p_list_of_values
          , p_search_fields
          , p_search_values
          , p_effective
     );
END;
$BODY$ LANGUAGE plpgsql;