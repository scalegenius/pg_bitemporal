#!/bin/bash
#

BUILD=`dirname $0`
APP_ROOT=`dirname $BUILD`
export PGOPTIONS="--client-min-messages=warning"

PSQL_CMD="psql -q -P pager=off "
PSQL="$PSQL_CMD -v VERBOSITY=terse -v ON_ERROR_STOP=on"

D=${APP_ROOT}/sql

# infra setup
if [ "${CI:-nil}" != "nil" ] ; then
   echo "CI Infrastructure setup"
#  $PSQL -f ${APP_ROOT}/dummy_fdw/dummy_data--1.0.sql
fi
echo "_load_all.sql"
echo "\\set CI $CI %\\ir $D/_load_all.sql" |tr '%' '\n' | $PSQL
LOAD_ALL=$?

if (( $LOAD_ALL )); then
  exit 1
fi

# load pgtap framework
LOAD_PGTAP=0
if  [ "${CI:-nil}" != "nil" ] ; then
  echo "Load pgtap"
  echo "\\ir ${BUILD}/pgtap.sql %\\ir ${BUILD}/pgtap_ft.sql" |tr '%' '\n' | $PSQL
  LOAD_PGTAP=$?
fi

if ((  $LOAD_PGTAP || $LOAD_ALL )); then
  exit 1
else
  exit 0
fi
