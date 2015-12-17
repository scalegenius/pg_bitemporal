\echo Start load all bitemporal code
\set ON_ERROR_STOP on
\pset pager off
-- toggle timing to get a better idea of what is going on
--\timing on
set client_min_messages to warning;


\ir relationships.sql

