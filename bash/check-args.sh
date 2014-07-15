#!/bin/bash

if [ $1 -ne 2 ]; then
    echo "Must input 2 args: data (clueweb or wikipedia), extractor (reverb, ollie, or clauseie)"
    exit 1
fi

