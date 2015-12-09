BEGIN;
set client_min_messages to warning;
set local search_path = 'temporal_relationships', 'public';

SELECT plan( 6 );

select is( xor(true, true), false) ; 
select is( xor(true, false), true) ; 
select is( xor(false, true), true) ; 
select is( xor(false, false), false) ; 

select is( fst(  daterange('1999-01-01'::date, '2010-12-31'::date)), '1999-01-01'::date );
select is( snd(  daterange('1999-01-01'::date, '2010-12-31'::date)), '2010-12-31'::date );





SELECT * FROM finish();
ROLLBACK;
