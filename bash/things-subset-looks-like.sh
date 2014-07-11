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
THING_FILE=$ROOT/src/main/resources/things-subset
while read THING
do
   echo $THING
   qsub -cwd -l mem_free=16G -v ARGS="--thing \"$THING\" " qsub-job.sh
done < ${THING_FILE}
