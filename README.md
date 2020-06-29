# Dictionary of Disease Ontologies (DODO)

## Aim

The aim of DODO is to allow an easier way to interact and explore disease ontologies and their identifiers. The database is build on Neo4j and incorporates different ontologies with an accompagnying R package that allows easy access, exploration, and definition of disease concepts of interest. It can work as the intermediate player to facilitate access and exhaustive extraction of information from other life science databases without the need to harmonize these up front. 

## Installation 

The data model is implemented using the Neo4j graph database which using the Cypher query language [@Neo4j2020]. One accompagnying R package *DODO* was developed to connect and query the resource. It provides higher level functions to query the Neo4j graph database based on the described data model (above) [@R2019].

```
devtools::install_github("patzaw/BED")
```

The minimal system requirements are: 

- R ≥ 3.6
- Operating system: Linux, macOS, Windows
- Memory ≥ 4GB RAM

The graph database has been implemented with Neo4j 3.4.9 [@Neo4j2020], the DODO R package depends on the following packages:

-  dplyr
-  tibble
-  neo2R
-  rlist
-  stringr
-  readr
-  visNetwork
-  shinythemes
-  DT
-  igraph
-  shiny

## Implementation and usage

Please refer to the (DODO paper)[https://github.com/Elysheba/DODO/tree/master/inst/documentation/DODO-F1000-publication/DODO-F1000-publication.Rmd] for more information.

## Constructing a new DODO Neo4j database

To construct a DODO instance, a set of script is available to load and feed a Neo4j instance. These are not exposed directly to the user instead, these scripts are available in the *build/scripts* folder. The feeding of DODO is based on the parsed files of the different ontologies, a workflow on downloading and parsing for each included ontology is available through GitHub.


|Resource | GitHub repository|
|----|-------|
|Monarch Disease Ontology (MonDO) | https://github.com/Elysheba/Monarch |
|Experimental Factor Ontology (EFO) | https://github.com/Elysheba/EFO |
|Orphanet | https://github.com/Elysheba/Orphanet |
|MedGen | https://github.com/Elysheba/MedGen |
|Medical Subject Headings (MeSH) | https://github.com/Elysheba/MeSH |
|Human Phenotype Ontology (HPO) | https://github.com/patzaw/HPO |
|ClinVar | https://github.com/patzaw/ClinVar |
|Disease Ontology (DO) | https://github.com/Elysheba/DO |
|International Classification of Diseases (ICD11) | https://github.com/Elysheba/ICD11 |

## Docker image

This docker image provides an image of the DODO (Dictionary of Disease Ontologies) graph database. It aims to provide a more complete mapping across the multitude of disease ontologies and a comprehensive way to explore and interact with disease ontologies. It includes the ontologies listed below: 

- Monarch Ontology
- EFO
- DO
- ICD11
- MedGen
- MeSH
- HPO
- ClinVar

The docker image has been made available on Docker Hub: Updates will be made available on https://hub.docker.com/repository/docker/elysheba/public-dodo


Run the image

```
docker run -d \
--name public-dodo\
--restart always \
--publish=7476:7474 \
--publish=7689:7687 \
--env=NEO4J_dbms_memory_heap_initial__size=4G \
--env=NEO4J_dbms_memory_heap_max__size=4G \
--env=NEO4J_dbms_memory_pagecache_size=2G \
--env=NEO4J_dbms_query__cache__size=0 \
--env=NEO4J_cypher_min__replan__interval=100000000ms \
--env=NEO4J_cypher_statistics__divergence__threshold=1 \
--env=NEO4J_dbms_security_procedures_unrestricted=apoc.\\\* \
--env=NEO4J_dbms_directories_import=import \
--env NEO4J_AUTH=none\
elysheba/public-dodo:20.04.2020
```



