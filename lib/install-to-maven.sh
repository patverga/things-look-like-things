#!/bin/bash

LIBPATH=`pwd`

cd $LIBPATH/..

mvn install:install-file -Dfile=${LIBPATH}/core-3.6.jar -DgroupId=org.lemurproject -DartifactId=galago-core -Dversion=3.6 -Dpackaging=jar

mvn install:install-file -Dfile=${LIBPATH}/tupleflow-3.6.jar -DgroupId=org.lemurproject -DartifactId=galago-tupleflow -Dversion=3.6 -Dpackaging=jar

mvn install:install-file -Dfile=${LIBPATH}/tupleflow-typebuilder-3.6.jar -DgroupId=org.lemurproject -DartifactId=galago-tupleflow-builder -Dversion=3.6 -Dpackaging=jar

mvn install:install-file -Dfile=${LIBPATH}/clauseie-1.0.jar -DgroupId=de.mpii -DartifactId=clauseie -Dversion=1.0 -Dpackaging=jar

mvn install:install-file -Dfile=${LIBPATH}/stanford-parser.jar -DgroupId=de.mpii -DartifactId=clauseie-stanford-parser -Dversion=1.0 -Dpackaging=jar

mvn install:install-file -Dfile=${LIBPATH}/stanford-parser-2.0.4-models.jar -DgroupId=de.mpii -DartifactId=clauseie-stanford-models -Dversion=1.0 -Dpackaging=jar
