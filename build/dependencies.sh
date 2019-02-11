#!/bin/bash
#
#
#

echo "Apt-get Update"
sudo apt-get update
echo "Update ca certs"
sudo update-ca-certificates
echo "Install packages"
sudo apt-get install -y --no-install-recommends parallel libgetopt-declare-perl
yes | sudo cpan TAP::Parser::SourceHandler::pgTAP

echo "Verify pgTAP is installed correctly."
# If it is not tests get weird, hard to understand  errors.
perl -MTAP::Parser::SourceHandler::pgTAP -e 'print;'

if (( $? )); then
  exit 1
fi


if [ "x$CIRCLECI" == "x" ]; then
   echo "Skipping CircleCI setup"
else
   `dirname $0`/circleci_setup.sh
fi
