#!/bin/bash
#
# To debug output the best is to change circle.yml to 
#  bash dependencies.sh >> ${CIRCLE_ARTIFACTS}/dependencies.txt 
#
# Add directories to circle.yml as follows
#  cache_directories:
#    - "dummy_fdw" 

# Verify pgTAP is installed correctly.
# If it is not tests get weird, hard to understand  errors.
perl -MTAP::Parser::SourceHandler::pgTAP -e 'print;'

#echo "Installing dummy_data FDW"
#[ ! -d dummy_fdw ] && git clone https://github.com/slaught/dummy_fdw.git
#(cd dummy_fdw; make clobber; make)
