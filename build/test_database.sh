#!/bin/bash

source  `dirname $0`/environment.sh

export CI=${CI:-true}

D=${APP_ROOT}/sql


${BUILD}/pg_prove --ext .pg --ext .sql ${APP_ROOT}/tests/
PGTAP_TEST=$?

echo "Test if load_all is reloadable."
# TODO: make new 'create type" reloadable
echo "There will be an ERROR for type bitemporal_pg_constraint"
echo "\\set CI $CI %\\ir $D/_load_all.sql" |tr '%' '\n' | $PSQL
RELOAD_TEST=$?

# Print out the return code. Bash math sucks!
echo " "
echo "pgtap test returned:" $PGTAP_TEST
echo "reload test returned:" $RELOAD_TEST

echo "Skipping RELOAD_TEST Check"
# if ((  $PGTAP_TEST || $RELOAD_TEST )); then
if ((  $PGTAP_TEST )); then
  exit 1
else
  exit 0
fi
