#!/bin/bash

# run max 100 jobs at a time
./dont-run-too-many-jobs.sh

for dat in "clueweb" "wikipedia"; do
   ./pattern-queries.sh $dat ollie
   ./dogs-looks-like.sh $dat ollie
   ./things-subset-looks-like.sh $dat ollie
   ./all-relations.sh $dat ollie
done

