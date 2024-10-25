# DODO build commands for powershell

# Define the path to the user info file
$userInfoFile = "C:\Users\Public\user_info.txt"

# Read user information from the file and convert it into a hashtable
$userInfo = @{}
Get-Content $userInfoFile | ForEach-Object {
    $keyValue = $_ -split '='
    if ($keyValue.Length -eq 2) {
        $userInfo[$keyValue[0].Trim()] = $keyValue[1].Trim()
    }
}

# Retrieve the user variable from the hashtable
$user = $userInfo["user"]

# First run the 01_process_ontologies with the type of interest (internal-dodo or public-dodo)
# After this has finished, run the code below in powershell.

# Or run it directly here
# Set the path to your Rscript executable and your R script
$RscriptPath = "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe"
$RscriptFile = "C:\Users\$user\Git_Repos\DODO\supp\build-scripts\01_process_ontologies.R"

# Initialise relevant variables
$containerName = "internal-dodo"

# Execute the R script
# & $RscriptPath $RscriptFile $containerName 2>&1


$nj_data = "C:\Users\$user\Data_Analysis\Reference_Files\neo4j_data"
$nj_plugins = "C:\Users\$user\Data_Analysis\Reference_Files\neo4j_plugins"
$nj_backups = "C:\Users\$user\Data_Analysis\Reference_Files\neo4j_backups\$containerName"
$nj_ont = "C:\Users\$user\Data_Analysis\Reference_Files\DODO\final_disease_ontos\$containerName"

# Run docker commands
docker run --interactive --tty --rm `
    --name $containerName  `
    --publish=7474:7474 --publish=7687:7687  `
    --env NEO4J_AUTH=none  `
    --env NEO4J_server_memory_heap_initial__size=8G `
    --env NEO4J_server_memory_heap_max__size=20G  `
    --env NEO4J_server_memory_pagecache_size=8G  `
    --volume=${nj_data}:/data  `
    --volume=${nj_ont}:/import  `
    neo4j:5.15  `
    neo4j-admin database import full `
    --nodes=/import/instance.nodes.csv --nodes=/import/disease.nodes.csv  `
    --nodes=/import/phenotype.nodes.csv --nodes=/import/db.nodes.csv  `
    --relationships=/import/xref.edges.csv --relationships=/import/parent.edges.csv  `
    --relationships=/import/pheno.edges.csv --relationships=/import/altid.edges.csv  `
    --relationships=/import/concept_db.edges.csv `
    --overwrite-destination --multiline-fields=true --verbose


docker run --interactive --tty --rm `
    --volume=${nj_data}:/data  `
    --volume=${nj_backups}:/var/lib/neo4j/backups `
    neo4j:5.15 `
    neo4j-admin database dump neo4j --to-path=/var/lib/neo4j/backups --overwrite-destination


docker run --interactive --tty --rm  `
    --name $containerName  `
    --publish=7474:7474 --publish=7687:7687  `
    --env NEO4J_AUTH=none  `
    --env NEO4J_server_memory_heap_initial__size=8G   `
    --env NEO4J_server_memory_heap_max__size=20G  `
    --env NEO4J_server_memory_pagecache_size=8G  `
    --volume=${nj_backups}:/backups `
    --volume=${nj_data}:/data `
    neo4j:5.15 `
    neo4j-admin database load neo4j --from-path=/backups --overwrite-destination --verbose

## getting apoc for 5.15
# wget https://github.com/neo4j/apoc/releases/download/5.15.1/apoc-5.15.1-core.jar
##
docker run --interactive --tty --rm `
    --name $containerName  `
    --publish=7474:7474 --publish=7687:7687  `
    --env NEO4J_AUTH=none  `
    --env NEO4J_server_memory_heap_initial__size=8G  `
    --env NEO4J_server_memory_heap_max__size=20G  `
    --env NEO4J_server_memory_pagecache_size=8G  `
    --env NEO4J_apoc_export_file_enabled=true `
    --env NEO4J_apoc_import_file_enabled=true `
    --env NEO4J_apoc_import_file_use__neo4j__config=true `
    --env NEO4J_dbms_security_procedures_unrestricted=apoc.* `
    --env NEO4J_dbms_security_procedures_allowlist=apoc.* `
    --volume=${nj_data}:/data `
    --volume=${nj_plugins}:/plugins `
    neo4j:5.15