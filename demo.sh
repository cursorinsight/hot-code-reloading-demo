#!/bin/bash

TOP_DIR=`pwd`/_demo

read -p "Cleaning up old PROD environment (press <enter> to continue)" -n1 -s && echo

echo "Stopping / removing old hcr-prod directory"
${TOP_DIR}/bin/hcr_demo stop
rm -rf ${TOP_DIR}

echo "Installing 0.1.0"
mkdir ${TOP_DIR}
cd ${TOP_DIR}
tar xf ${TOP_DIR}/../_build/prod/rel/hcr_demo/hcr_demo-0.1.0.tar.gz
cp ${TOP_DIR}/../_build/prod/rel/hcr_demo/hcr_demo-0.1.0.tar.gz ${TOP_DIR}/releases

echo "starting 0.1.0"
${TOP_DIR}/bin/hcr_demo-0.1.0 start

read -p "waiting for <enter>" -n1 -s && echo

echo "copying 0.2.0 (and 0.1.0)"
cp ${TOP_DIR}/../_build/prod/rel/hcr_demo/hcr_demo-0.2.0.tar.gz ${TOP_DIR}/releases

echo "upgrading 0.2.0"
${TOP_DIR}/bin/hcr_demo-0.1.0 upgrade "0.2.0"
