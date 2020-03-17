#!/bin/sh

#--------------------------------------------------------------------
#--------------------------------------------------------------------
## Start new internal docker container


#################################
## CONFIG according to your needs
#################################

## Type of report and instance to generate (public / internal)
export TYPE=internal
export NAME=dodo
## version
export DODO_VERSION=`date +%d.%m.%Y`
# export CONTAINER=neoDODO

## Chose Neo4j version (Only versions 3 and 4 are supported)
# export NJ_VERSION=4.0.0
export NJ_VERSION=3.5.14

## Ports
export NJ_HTTP_PORT=7475
export NJ_BOLT_PORT=7688

## Change the location of the Neo4j directory
export NJ_HOME=/data/lfrancois/Development/DODO/build/working
export NJ_ROOT=/data/lfrancois/Development/DODO/build

## Authorization
NJ_AUTH=none # set to 'neo4j/1234' if you want to set the 'neo4j' user with the '1234' password.

## APOC download
export NJ_APOC_LOC=/data/lfrancois/Development/DODO/build/plugins
# export NJ_APOC_URL=https://github.com/neo4j-contrib/neo4j-apoc-procedures/releases/download/4.0.0.2/apoc-4.0.0.2-all.jar
# export NJ_APOC=apoc-4.0.0.2-all.jar
# export NJ_APOC_URL=https://github.com/neo4j-contrib/neo4j-apoc-procedures/releases/download/4.0.0.3/apoc-4.0.0.3-all.jar
# export NJ_APOC=apoc-4.0.0.3-all.jar


#################################
## RUN
#################################
cd $NJ_ROOT/scripts
./Create-New-DODO-Container.sh

# docker stop $NAME
# docker container rm $NAME
# 
# mkdir -p $NJ_HOME
# 
# ## Import and data directory
# export NJ_IMPORT=$NJ_HOME/neo4jImport
# mkdir -p $NJ_IMPORT
# export NJ_DATA=$NJ_HOME/neo4jData
# if test -e $NJ_DATA; then
#    echo "$NJ_DATA directory exists ==> abort - Remove it before proceeding" >&2
#    exit
# fi
# mkdir -p $NJ_DATA
# ## Neo4j plugins: APOC
# export NJ_PLUGINS=$NJ_HOME/neo4jPlugins
# mkdir -p $NJ_PLUGINS
# cd $NJ_PLUGINS
# 
# if test -f "$NJ_APOC"
# then
#    echo "Using existing $NJ_APOC"
# else
#    echo "Downloading $NJ_APOC"
#    cp $NJ_APOC_LOC/apoc-3.5.0.8-all.jar $NJ_PLUGINS
#    # cp $NJ_APOC_LOC/apoc-4.0.0.3-all.jar $NJ_PLUGINS
#    # wget --no-check-certificate $NJ_APOC_URL -O $NJ_APOC
# fi
# cd -
# 
# docker run -d \
#    --name $NAME \
#    --publish=$NJ_HTTP_PORT:7474 \
#    --publish=$NJ_BOLT_PORT:7687 \
#    --env=NEO4J_dbms_memory_heap_initial__size=4G \
#    --env=NEO4J_dbms_memory_heap_max__size=4G \
#    --env=NEO4J_dbms_memory_pagecache_size=2G \
#    --env=NEO4J_dbms_query__cache__size=0 \
#    --env=NEO4J_cypher_min__replan__interval=100000000ms \
#    --env=NEO4J_cypher_statistics__divergence__threshold=1 \
#    --env=NEO4J_dbms_security_procedures_unrestricted=apoc.\\\* \
#    --env=NEO4J_dbms_directories_import=import \
#    --env NEO4J_AUTH=$NJ_AUTH \
#    --volume $NJ_IMPORT:/var/lib/neo4j/import \
#    --volume $NJ_DATA/data:/data \
#    --volume $NJ_PLUGINS:/plugins \
#     neo4j:$NJ_VERSION
# 
# #--------------------------------------------------------------------
# #--------------------------------------------------------------------
# ## Building DODO database + report
# cd $NJ_ROOT/scripts/
# Rscript compile-feed-DODO-instance.R $TYPE $NAME
# 
# ## Create backup 
# mkdir $NJ_ROOT/ARCHIVE/$DODO_VERSION 
# sudo cp $NJ_ROOT/data/$NAME-neo4j-input-files.rda $NJ_ROOT/ARCHIVE/$DODO_VERSION/
# # sudo cp NJ_ROOT/working/data/dodo.schema NJ_ROOT/ARCHIVE/$DODO_VERSION/
# # sudo cp -r NJ_ROOT/working/dgraph NJ_ROOT/ARCHIVE/$DODO_VERSION/
#   
# #--------------------------------------------------------------------
# #--------------------------------------------------------------------
# ## Create Docker image
# # Stop container
# docker stop $NAME
# 
# # Create and save the image
# echo "FROM neo4j:$NJ_VERSION" > $NJ_HOME/Dockerfile
# echo "COPY neo4jPlugins /plugins" >> $NJ_HOME/Dockerfile
# echo "COPY neo4jData/data /data" >> $NJ_HOME/Dockerfile
# 
# cd $NJ_HOME
# docker build -t $NAME:$DODO_VERSION .
# 
# cd $NJ_ROOT/
# mkdir -p DODO-imagesdoc
# docker save $NAME:$DODO_VERSION > DODO-images/docker-$NAME-$DODO_VERSION.tar
# 
# ## Copy image to ARCHIVE
# cp DODO-images/docker-$NAME-$DODO_VERSION.tar $NJ_ROOT/ARCHIVE/$DODO_VERSION/
# ## Copy ARCHIVE to data science folder
# # cp -r $NJ_ROOT/ARCHIVE/$DODO_VERSION /home/lfrancois/Shared/Data-Science/R-Sandbox/neoDODO/build/ARCHIVE/
#   
# #--------------------------------------------------------------------
# #--------------------------------------------------------------------
# ## Finalize and start the new DODO instance
# ## Stop and remove old instance
# cd $NJ_HOME
# docker stop $NAME
# docker container rm $NAME
# 
# ## Load latest image
# docker image load -i $NJ_ROOT/DODO-images/docker-$NAME-$DODO_VERSION.tar
# docker tag $NAME:$DODO_VERSION $NAME:latest
# 
# docker run -d \
#    --name $NAME \
#    --publish=$NJ_HTTP_PORT:7474 \
#    --publish=$NJ_BOLT_PORT:7687 \
#    --env=NEO4J_dbms_memory_heap_initial__size=4G \
#    --env=NEO4J_dbms_memory_heap_max__size=4G \
#    --env=NEO4J_dbms_memory_pagecache_size=2G \
#    --env=NEO4J_dbms_query__cache__size=0 \
#    --env=NEO4J_cypher_min__replan__interval=100000000ms \
#    --env=NEO4J_cypher_statistics__divergence__threshold=1 \
#    --env=NEO4J_dbms_security_procedures_unrestricted=apoc.\\\* \
#    --env=NEO4J_dbms_directories_import=import \
#    --env NEO4J_AUTH=$NJ_AUTH \
#     $NAME:latest
# 
# 
