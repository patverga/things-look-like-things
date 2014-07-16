#!/bin/bash
#
#

echo $ARGS

cd /home/pat/things-look-like-things/

mvn compile && mvn scala:run -DmainClass=co.pemma.MainThings -Dargs="${ARGS}"
