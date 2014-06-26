#!/bin/bash

LIBPATH=`pwd`

cd $LIBPATH/..

mvn install:install-file -Dfile=${LIBPATH}/galago.jar -DgroupId=org.lemurproject -DartifactId=galago -Dversion=3.6 -Dpackaging=jar

mvn install:install-file -Dfile=${LIBPATH}/core-3.6.jar -DgroupId=org.lemurproject -DartifactId=galago-core -Dversion=3.6 -Dpackaging=jar

mvn install:install-file -Dfile=${LIBPATH}/tupleflow-3.6.jar -DgroupId=org.lemurproject -DartifactId=galago-tupleflow -Dversion=3.6 -Dpackaging=jar

mvn install:install-file -Dfile=${LIBPATH}/tupleflow-typebuilder-3.6.jar -DgroupId=org.lemurproject -DartifactId=galago-tupleflow-builder -Dversion=3.6 -Dpackaging=jar
