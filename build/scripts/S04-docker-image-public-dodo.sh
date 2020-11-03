#################################
## CONFIG according to your needs
#################################

## Type of report and instance to generate (public / internal)
export TYPE=public
export NAME=public-dodo
export TMP=tmpdodo
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
export NJ_HOME=/data/lfrancois/Development/DODO/build/working
export NJ_ROOT=/data/lfrancois/Development/DODO/build

## Authorization
NJ_AUTH=none # set to 'neo4j/1234' if you want to set the 'neo4j' user with the '1234' password.

## APOC download
export NJ_APOC_LOC=/data/lfrancois/Development/DODO/build/plugins


#--------------------------------------------------------------------
#--------------------------------------------------------------------
## Create Docker image
# Stop container
docker stop $TMP

# Create and save the image
echo "FROM neo4j:$NJ_VERSION" > $NJ_HOME/Dockerfile
echo "COPY neo4jPlugins /plugins" >> $NJ_HOME/Dockerfile
echo "COPY neo4jData/data /data" >> $NJ_HOME/Dockerfile

cd $NJ_HOME
docker build -t elysheba/$NAME:$DODO_VERSION .

cd $NJ_ROOT/
mkdir -p DODO-images
docker save elysheba/$NAME:$DODO_VERSION > DODO-images/docker-$NAME-$DODO_VERSION.tar

## Copy image to ARCHIVE
cp DODO-images/docker-$NAME-$DODO_VERSION.tar $NJ_ROOT/ARCHIVE/$DODO_VERSION/
## Copy ARCHIVE to data science folder
# cp -r $NJ_ROOT/ARCHIVE/$DODO_VERSION /home/lfrancois/Shared/Data-Science/R-Sandbox/neoDODO/build/ARCHIVE/

#--------------------------------------------------------------------
#--------------------------------------------------------------------
## Finalize and start the new DODO instance
## Stop and remove old instance
cd $NJ_HOME
docker stop $NAME
docker container rm $NAME
docker container rm $TMP

## Load latest image
docker image load -i $NJ_ROOT/DODO-images/docker-$NAME-$DODO_VERSION.tar
docker tag elysheba/$NAME:$DODO_VERSION $NAME:latest

docker run -d \
   --name $NAME \
   --publish=$NJ_HTTP_PORT:7474 \
   --publish=$NJ_BOLT_PORT:7687 \
   --env=NEO4J_dbms_memory_heap_initial__size=4G \
   --env=NEO4J_dbms_memory_heap_max__size=4G \
   --env=NEO4J_dbms_memory_pagecache_size=2G \
   --env=NEO4J_dbms_query__cache__size=0 \
   --env=NEO4J_cypher_min__replan__interval=100000000ms \
   --env=NEO4J_cypher_statistics__divergence__threshold=1 \
   --env=NEO4J_dbms_security_procedures_unrestricted=apoc.\\\* \
   --env=NEO4J_dbms_directories_import=import \
   --env NEO4J_AUTH=$NJ_AUTH \
    $NAME:latest
    
docker tag $NAME:$DODO_VERSION elysheba/$NAME:$DODO_VERSION
