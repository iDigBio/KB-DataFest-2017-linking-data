#!/bin/bash

# The formatting of CSV file downloaded from the SPARL interface isn't great, there are spaces
# around the delimter, prune them manually to make importing easier.
perl -pi -e 's/^ "/"/' data/queryResults_phenoscape_specimens.csv
perl -pi -e 's/" ,$/"/' data/queryResults_phenoscape_specimens.csv
perl -pi -e 's/" , "/","/g' data/queryResults_phenoscape_specimens.csv
