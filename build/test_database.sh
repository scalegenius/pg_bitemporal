#!/bin/bash
export CI=${CI:-true}
export CIRCLECI=${CIRCLECI:-true}
export CIRCLE_ARTIFACTS=${CIRCLE_ARTIFACTS:-./tmp}

BUILD=`dirname $0`
APP_ROOT=`dirname $BUILD`
D=${APP_ROOT}/sql

export PGOPTIONS="--client-min-messages=warning"
PSQL_CMD="psql -q -P pager=off "
PSQL="$PSQL_CMD -X -v VERBOSITY=terse -v ON_ERROR_STOP=on "

${BUILD}/pg_prove --ext .pg --ext .sql ${APP_ROOT}/tests/
PGTAP_TEST=$?

echo "Test if load_all is reloadable."
echo "\\set CI $CI %\\ir $D/_load_all.sql" |tr '%' '\n' | $PSQL
RELOAD_TEST=$?

# Print out the return code. Bash math sucks!
echo " "
echo "pgtap test returned:" $PGTAP_TEST
echo "reload test returned:" $RELOAD_TEST

if ((  $PGTAP_TEST || $RELOAD_TEST )); then
  exit 1
else
  exit 0
fi
