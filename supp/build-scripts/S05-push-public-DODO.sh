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
export NJ_HOME=/data/lfrancois/Development/
export NJ_ROOT=/data/lfrancois/Development/DODO/supp

## Authorization
NJ_AUTH=none # set to 'neo4j/1234' if you want to set the 'neo4j' user with the '1234' password.

## APOC download
export NJ_APOC_LOC=/data/lfrancois/Development/DODO/supp/plugins

###############################
## Docker hub on windows (!!)
###############################
docker load --input docker-$NAME-$DODO_VERSION.tar
    
docker login 
docker push elysheba/$NAME:$DODO_VERSION