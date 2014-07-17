#!/bin/bash

set -e
./dont-run-too-many-jobs.sh
./check-args.sh $#

# file where each line contains a thing we want to find things that look like
ROOT=/home/pat/things-look-like-things
THING_FILE=$ROOT/src/main/resources/dogs
while read THING
do
   echo $THING
   qsub -cwd -l mem_free=32G -v ARGS="--syns=$THING|--data=$1|--extractor=$2" qsub-job.sh
done < ${THING_FILE}
