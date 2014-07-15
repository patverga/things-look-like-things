#!/bin/bash

# run max 100 jobs at a time
./dont-run-too-many-jobs.sh

for dat in "clueweb" "wikipedia"; do
   for extract in "reverb" "clauseie" "ollie"; do
       echo $dat $extract

      ./pattern-queries.sh $dat $extract
#      ./dogs-looks-like.sh $dat $extract
#      ./things-subset-looks-like.sh $dat $extract
   done
done

