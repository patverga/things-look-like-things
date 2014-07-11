#!/bin/bash
#
#

echo $ARGS

cd /home/pat/things-look-like-things/

mvn compile && mvn exec:java -Dexec.mainClass=co.pemma.MainThings -Dexec.args="${ARGS}"
