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
--volume $NJ_IMPORT:/var/lib/neo4j/import \
--volume $NJ_DATA/data:/data \
--volume $NJ_PLUGINS:/plugins \
neo4j:$NJ_VERSION

#--------------------------------------------------------------------
#--------------------------------------------------------------------
## Building DODO database + report
cd $NJ_ROOT/scripts/
  Rscript compile-feed-DODO-instance.R $TYPE $NAME

## Create backup 
mkdir $NJ_ROOT/ARCHIVE/$DODO_VERSION 
cp $NJ_ROOT/data/$NAME-neo4j-input-files.rda $NJ_ROOT/ARCHIVE/$DODO_VERSION/
  # sudo cp NJ_ROOT/working/data/dodo.schema NJ_ROOT/ARCHIVE/$DODO_VERSION/
  # sudo cp -r NJ_ROOT/working/dgraph NJ_ROOT/ARCHIVE/$DODO_VERSION/
  
  #--------------------------------------------------------------------
#--------------------------------------------------------------------
## Create Docker image
# Stop container
docker stop $NAME

# Create and save the image
echo "FROM neo4j:$NJ_VERSION" > $NJ_HOME/Dockerfile
echo "COPY neo4jPlugins /plugins" >> $NJ_HOME/Dockerfile
echo "COPY neo4jData/data /data" >> $NJ_HOME/Dockerfile

cd $NJ_HOME
docker build -t $NAME:$DODO_VERSION .

cd $NJ_ROOT/
  mkdir -p DODO-images
docker save $NAME:$DODO_VERSION > DODO-images/$NAME:$DODO_VERSION.tar

## Copy image to ARCHIVE
# cp DODO-images/$NAME:$DODO_VERSION.tar $NJ_ROOT/ARCHIVE/$DODO_VERSION/
## Copy ARCHIVE to data science folder
cp -r DODO-images/$NAME:$DODO_VERSION.tar /home/lfrancois/Shared/Data-Science/R-Packages/DODO/build/DODO-images/
  
  #--------------------------------------------------------------------
#--------------------------------------------------------------------
## Finalize and start the new DODO instance
## Stop and remove old instance
cd $NJ_HOME
docker stop $NAME
docker container rm $NAME

## Load latest image
docker image load -i $NJ_ROOT/DODO-images/docker-$NAME-$DODO_VERSION.tar
docker tag $NAME:$DODO_VERSION $NAME:latest

docker run -d \
--name $NAME \
--restart always \
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