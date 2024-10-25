#!/bin/sh

#--------------------------------------------------------------------
#--------------------------------------------------------------------
## Start new docker container
## This script will load the neo4j dump into a new instance and start the neo4j docker

## Type of report and instance to generate (public / internal)

## 
# public-dodo
export TYPE=public-dodo

./shell_dodo_docker_build.sh
./shell_dodo_load_neo4j.sh

# internal-dodo
export TYPE=internal-dodo

./shell_dodo_docker_build.sh
./shell_dodo_load_neo4j.sh