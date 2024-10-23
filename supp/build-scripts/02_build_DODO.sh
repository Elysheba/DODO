#!/bin/sh
# Initialise relevant variables
docker_name="dodo"
path_import=""
path_data=""
heap_initial="8G"
heap_max="20G"
pagecache="8G"
neo4j_version="5.15"
# Colours
red='\033[0;31m'
no_col='\033[0m'
docker run --interactive --tty --rm `
    --name dodo  `
    --publish=7474:7474 --publish=7687:7687  `
    --env NEO4J_AUTH=none  `
    --env NEO4J_server_memory_heap_initial__size=8G `
    --env NEO4J_server_memory_heap_max__size=20G  `
    --env NEO4J_server_memory_pagecache_size=8G  `
    --volume=C:\Users\U060109\Data_Analysis\Reference_Files\neo4j_data:/data  `
    --volume=C:\Users\U060109\Data_Analysis\Reference_Files\DODO\final_disease_ontos:/import  `
    neo4j:5.15  `
    neo4j-admin database import full `
    --nodes=/import/instance.nodes.csv --nodes=/import/disease.nodes.csv  `
    --nodes=/import/phenotype.nodes.csv --nodes=/import/db.nodes.csv  `
    --relationships=/import/xref.edges.csv --relationships=/import/parent.edges.csv  `
    --relationships=/import/pheno.edges.csv --relationships=/import/altid.edges.csv  `
    --relationships=/import/concept_db.edges.csv `
    --overwrite-destination --multiline-fields=true --verbose


docker run --interactive --tty --rm `
    --volume=C:\Users\U060109\Data_Analysis\Reference_Files\neo4j_data:/data  `
    --volume=C:\Users\U060109\Data_Analysis\Reference_Files\neo4j_backups:/var/lib/neo4j/backups `
    neo4j:5.15 `
    neo4j-admin database dump neo4j --to-path=/var/lib/neo4j/backups --overwrite-destination


docker run --interactive --tty --rm  `
    --name dodo  `
    --publish=7474:7474 --publish=7687:7687  `
    --env NEO4J_AUTH=none  `
    --env NEO4J_server_memory_heap_initial__size=8G   `
    --env NEO4J_server_memory_heap_max__size=20G  `
    --env NEO4J_server_memory_pagecache_size=8G  `
    --volume=C:\Users\U060109\Data_Analysis\Reference_Files\neo4j_backups:/backups `
    --volume=C:\Users\U060109\Data_Analysis\Reference_Files\neo4j_data:/data `
    neo4j:5.15 `
    neo4j-admin database load neo4j --from-path=/backups --overwrite-destination --verbose

docker run --interactive --tty --rm `
    --name dodo  `
    --publish=7474:7474 --publish=7687:7687  `
    --env NEO4J_AUTH=none  `
    --env NEO4J_server_memory_heap_initial__size=8G  `
    --env NEO4J_server_memory_heap_max__size=20G  `
    --env NEO4J_server_memory_pagecache_size=8G  `
    --volume=C:\Users\U060109\Data_Analysis\Reference_Files\neo4j_data:/data `
    neo4j:5.15