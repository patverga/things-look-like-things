#!/bin/bash

# run max 100 jobs at a time
running=`qstat | grep pat | wc -l`
while [ $running -gt 100 ] 
do
   # sleep for 10 seconds - waiting for jobs to finish
   sleep 10
   running=`qstat | grep pat | wc -l`
done

# file where each line contains a thing we want to find things that look like
ROOT=/home/pat/things-look-like-things
PATTERN_FILE=$ROOT/src/main/resources/patterns
while read QUERY
do
   QUERY=`echo $QUERY | sed 's/?//g'`
   echo $QUERY
   ARGUMENTS="--pattern \"$QUERY\" "
   echo $ARGUMENTS
   qsub -cwd -l mem_free=16G -v ARGS="$ARGUMENTS " qsub-job.sh
done < ${PATTERN_FILE}
