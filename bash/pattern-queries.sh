#!/bin/bash

set -e
./dont-run-too-many-jobs.sh
./check-args.sh $#

# file where each line contains a thing we want to find things that look like
ROOT=/home/pat/things-look-like-things
PATTERN_FILE=$ROOT/src/main/resources/patterns
while read QUERY
do
   QUERY=`echo $QUERY | sed 's/?//g'`
   ARGUMENTS="--pattern \"$QUERY\" --data $1 --extractor $2 "
   echo $ARGUMENTS
   qsub -cwd -l mem_free=16G -v ARGS="$ARGUMENTS " qsub-job.sh
done < ${PATTERN_FILE}
