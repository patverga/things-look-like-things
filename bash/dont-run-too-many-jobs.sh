#!/bin/bash

# run max 100 jobs at a time 
running=`qstat | grep pat | wc -l` 
while [ $running -gt 100 ]  
do 
   # sleep for 10 seconds - waiting for jobs to finish 
   sleep 10 
   running=`qstat | grep pat | wc -l` 
done 
