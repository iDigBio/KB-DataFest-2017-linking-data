#!/bin/bash

fn=$1

if [ "A${fn}" == "A" ] ; then
    echo "Usage: $0 <file name>"
    exit 64
fi

# The formatting of CSV file downloaded from the SPARL interface isn't great, there are spaces
# around the delimter, prune them manually to make importing easier.
perl -pi -e 's/^ "/"/' "${fn}"
perl -pi -e 's/" ,$/"/' "${fn}"
perl -pi -e 's/" , "/","/g' "${fn}"
