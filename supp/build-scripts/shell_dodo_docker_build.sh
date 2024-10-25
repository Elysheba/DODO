#!/bin/sh

#--------------------------------------------------------------------
#--------------------------------------------------------------------
## Start new docker container
## This script will run the process script for the ontologies
## Then load the files into a neo4j and create a neo4j dump file

## Type of report and instance to generate (public / internal)
# export TYPE=public-dodo
export DODO_VERSION=`date +%d.%m.%Y`c

## Chose Neo4j version (Only versions 3 and 4 are supported)
export NJ_VERSION=5.15

## Ports
# export NJ_HTTP_PORT=7475
# export NJ_BOLT_PORT=7688

## Pathss
export NJ_DATA=/Local_Disk/Reference_Files/neo4j_data
export NJ_ONT=/Local_Disk/Reference_Files/DODO/final_disease_ontos/$TYPE
export NJ_PLUGIN=/Local_Disk/Reference_Files/neo4j_plugins
export NJ_BCK=/Local_Disk/Reference_Files/neo4j_backups/$TYPE

## Authorization
export NJ_AUTH=none # set to 'neo4j/1234' if you want to set the 'neo4j' user with the '1234' password.

echo "Running process script for the ontologies"
Rscript ./01_process_ontologies.R $TYPE

# echo "Importing into neo4j and creating dump"
# docker run --interactive --tty --rm `
#     --name $TYPE  `
#     --publish=7474:7474 --publish=7687:7687  `
#     --env NEO4J_AUTH=none  `
#     --env NEO4J_server_memory_heap_initial__size=8G `
#     --env NEO4J_server_memory_heap_max__size=20G  `
#     --env NEO4J_server_memory_pagecache_size=8G  `
#     --volume=$NJ_DATA:/data  `
#     --volume=$NJ_ONT:/import  `
#     neo4j:5.15  `
#     neo4j-admin database import full `
#     --nodes=/import/instance.nodes.csv --nodes=/import/disease.nodes.csv  `
#     --nodes=/import/phenotype.nodes.csv --nodes=/import/db.nodes.csv  `
#     --relationships=/import/xref.edges.csv --relationships=/import/parent.edges.csv  `
#     --relationships=/import/pheno.edges.csv --relationships=/import/altid.edges.csv  `
#     --relationships=/import/concept_db.edges.csv `
#     --overwrite-destination --multiline-fields=true --verbose
# 
# 
# docker run --interactive --tty --rm `
#     --volume=$NJ_DATA:/data  `
#     --volume=$NJ_BCK:/var/lib/neo4j/backups `
#     neo4j:5.15 `
#     neo4j-admin database dump neo4j --to-path=/var/lib/neo4j/backups --overwrite-destination
# 
