#!/bin/sh

#--------------------------------------------------------------------
#--------------------------------------------------------------------
## Start new public docker container


#################################
## CONFIG according to your needs
#################################

## Type of report and instance to generate (public / internal)
export TYPE=public
export NAME=public-dodo
## version
export DODO_VERSION=`date +%d.%m.%Y`
# export CONTAINER=neoDODO

## Chose Neo4j version (Only versions 3 and 4 are supported)
# export NJ_VERSION=4.0.0
export NJ_VERSION=3.5.14

## Ports
export NJ_HTTP_PORT=7476
export NJ_BOLT_PORT=7689

## Change the location of the Neo4j directory
export NJ_HOME=/data/lfrancois/Development/neoDODO/build/public-working
export NJ_ROOT=/data/lfrancois/Development/neoDODO/build

## Authorization
NJ_AUTH=none # set to 'neo4j/1234' if you want to set the 'neo4j' user with the '1234' password.

## APOC download
export NJ_APOC_LOC=/data/lfrancois/Development/neoDODO/build/plugins
# export NJ_APOC_URL=https://github.com/neo4j-contrib/neo4j-apoc-procedures/releases/download/4.0.0.2/apoc-4.0.0.2-all.jar
# export NJ_APOC=apoc-4.0.0.2-all.jar
# export NJ_APOC_URL=https://github.com/neo4j-contrib/neo4j-apoc-procedures/releases/download/4.0.0.3/apoc-4.0.0.3-all.jar
# export NJ_APOC=apoc-4.0.0.3-all.jar


#################################
## RUN
#################################
./Create-New-DODO-Container.sh
