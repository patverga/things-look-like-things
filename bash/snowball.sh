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
LINE_FILE=$ROOT/src/main/resources/things-look-like-these
while read LINE
do
   echo $LINE
   qsub -cwd -l mem_free=16G -v ARGS="--snowball \"$LINE\" " qsub-job.sh
done < ${LINE_FILE}
