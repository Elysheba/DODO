####################################################################@
## Methodology DOD search
## based on used case: Parkinson

####################
## 1. Identify top parents (and their xrefs) within the seed set and evaluate them
## 2. With each step go one level down to obtain the children and the children's xrefs 
##    Use clustering to idenfity sets within this expansion (remove all ids from the higher level). 
##    Also consider the singletons
##    In the end, the entire tree will be navigated
## 3. As not al ids have parents, cluster the remaining ids falling outside any tree 
##    (and consider singletons) and evaluate them

rm(list = ls())
gc()

library(DOD)

#################################################################################@
## Establish seeds
#################################################################################@
seed <- buildDisNetByTerm(searchTerm = "parkinson", fields = c("synonym"))   
lapply(seed,dim)

## Extend disNet
eseed <- extendDisNet(seed,relations = c("xref","child"))
lapply(eseed,dim)

## Exploration
plotDisNet(eseed,hover = "label", pNodes = "all", igraph = T)
exploreDisNet(eseed,show = "label",terms = "parkinson")

## Cluster disNet
ceseed <- clusterDisNet(eseed,clusterOn = "xref")
plotSetDisNet(setDisNet = ceseed, set = 5, pNodes = "all", igraph = T)
exploreSetDisNet(ceseed,show = "label",terms = "parkinson")


#################################################################################@
## STEP 1: seed
#################################################################################@
##=====================================
## Interation 1: top parents
## Every ID that is never a child 
step1 <- reviewParents(seed,terms = c("parkinson"))
lapply(step1$disNet,dim)
## Keep / Remove clusters
clusters <- step1$review$clusters
range(clusters$ind)
k <- c(1)
r <- setdiff(1:max(clusters$ind),k)
toKeepSeed <- c(step1$review$toKeep,clusters$values[clusters$ind %in% k])
toRemoveSeed <- c(step1$review$toRemove, clusters$values[clusters$ind %in% r])
length(toKeepSeed) 
length(toRemoveSeed)
dim(clusters)

# ## SINGLETONS
# s <- checkSingletons(step1,
#                      toKeep = toKeepSeed,
#                      toRemove = toRemoveSeed,
#                      terms = "parkinson")
# 
# k <- c()
# r <- setdiff(1:length(s$singleton),k)
# toKeepSeed <- c(s$review$toKeep, s$review$singleton[k])
# toRemoveSeed <- c(s$review$toRemoveSeed,s$review$singleton[r])

toShow <- buildDisNet(ids = toKeepSeed, seed = toKeepSeed)
plotDisNet(toShow)

## 
seedDisNet = seed
toKeep = toKeepSeed
toRemove = toRemoveSeed
terms = c("parkinson")
step2 <- focusNet(seedDisNet, diseaseID = toKeep, relationship = "child")
step2 <- filtDisNetByID(step2,diseaseID = step2$nodes$id[!step2$nodes$id %in% toRemove])
step2 <- focusNet(seedDisNet, diseaseID = step2$nodes$id, 
                  relationship = "xref", steps = 100)
step1 <- buildDisNet(ids = c(toKeep, toRemove), seed = c(toKeep, 
                                                         toRemove))
fstep2 <- setdiffDisNet(step2, step1)
review <- list(clusters = NULL, singleton = NULL, toKeep = toKeep, 
               toRemove = toRemove)
if (nrow(fstep2$nodes) > 0) {
  message(nrow(fstep2$nodes), " children (and their cross-references) were identified")
  clustDisNet <- try(clusterDisNet(fstep2, clusterOn = "xref"))
  if ("class" %in% names(attributes(clustDisNet))) {
    exploreDisNet(fstep2, show = "synonym", terms = terms)
  }
  else {
    message("There are ", length(clustDisNet), " clusters detected\n")
    cid <- unlist(lapply(clustDisNet, function(x) x$nodes$id))
    if (!all(fstep2$nodes$id %in% cid)) {
      message(nrow(fstep2$nodes) - length(cid), " node(s) don't belong to any cluster. \n", 
              "These are stored in the 'singleton'. \n", 
              "Please use checkSingletons() to verify these disease IDs. \n")
      review$singleton <- fstep2$nodes$id[!fstep2$nodes$id %in% 
                                            cid]
    }
    message("Cluster membership can be found in 'cluster'.")
    cm <- stack(lapply(clustDisNet, function(x) {
      x$nodes$id
    }))
    cm$ind <- as.numeric(cm$ind)
    review$clusters <- cm
    exploreSetDisNet(clustDisNet, show = "synonym", terms = terms)
  }
  return(list(disNet = step2, review = review))
}
else {
  message("All children have been found.")
  return(disNet = step2)
}

##=====================================
## Iteration 2
step2 <- reviewChildren(seedDisNet = seed, toKeep = toKeepSeed, toRemove = toRemoveSeed, terms = c("parkinson"))
lapply(step2$disNet,dim)
lapply(step2$review,length)

## Keep / Remove clusters
clusters <- step2$review$clusters
range(clusters$ind)
k <- c(1,3:14)
r <- setdiff(1:max(clusters$ind),k)
toKeepSeed <- c(step2$review$toKeep,clusters$values[clusters$ind %in% k])
toRemoveSeed <- c(step2$review$toRemove, clusters$values[clusters$ind %in% r])
length(toKeepSeed) + length(toRemoveSeed)
dim(step2$disNet$nodes)

# ## SINGLETONS
# s <- checkSingletons(step2,
#                      toKeep = toKeepSeed,
#                      toRemove = toRemoveSeed,
#                      terms = "parkinson")
# 
# k <- c()
# r <- setdiff(1:length(s$singleton),k)
# toKeepSeed <- c(s$review$toKeep, s$review$singleton[k])
# toRemoveSeed <- c(s$review$toRemoveSeed,s$review$singleton[r])

toShow <- buildDisNet(ids = toKeepSeed, seed = toKeepSeed)
plotDisNet(toShow)


##=====================================
## Iteration 3
step3 <- reviewChildren(seedDisNet = seed, toKeep = toKeepSeed, toRemove = toRemoveSeed, terms = c("parkinson"))
lapply(step3$disNet,dim)
## Keep / Remove clusters
clusters <- step3$review$clusters
range(clusters$ind)
k <- c(1)
r <- setdiff(1:max(clusters$ind),k)
toKeepSeed <- c(step3$review$toKeep,clusters$values[clusters$ind %in% k])
toRemoveSeed <- c(step3$review$toRemove, clusters$values[clusters$ind %in% r])
length(toKeepSeed)
length(toRemoveSeed)
dim(step3$disNet$nodes)

# ## SINGLETONS
# s <- checkSingletons(step2,
#                      toKeep = toKeepSeed,
#                      toRemove = toRemoveSeed,
#                      terms = "parkinson")
# 
# k <- c()
# r <- setdiff(1:length(s$singleton),k)
# toKeepSeed <- c(toKeepSeed, s$singleton[k])
# toRemoveSeed <- c(toRemoveSeed,s$singleton[r])

toShow <- buildDisNet(ids = toKeepSeed, seed = toKeepSeed)
plotDisNet(toShow)

##=====================================
## Iteration 4
step4 <- reviewChildren(seedDisNet = seed, toKeep = toKeepSeed, toRemove = toRemoveSeed, terms = c("parkinson"))
lapply(step4$disNet,dim)
## Keep / Remove clusters
clusters <- step4$review$clusters
range(clusters$ind)
k <- c()
r <- setdiff(1:max(clusters$ind),k)
toKeepSeed <- c(step4$review$toKeep,clusters$values[clusters$ind %in% k])
toRemoveSeed <- c(step4$review$toRemove, clusters$values[clusters$ind %in% r])
length(toKeepSeed) + length(toRemoveSeed)
dim(step4$disNet$nodes)
dim(seed$nodes)

# ## SINGLETONS
# s <- checkSingletons(step2,
#                      toKeep = toKeepSeed,
#                      toRemove = toRemoveSeed,
#                      terms = "parkinson")
# 
# k <- c()
# r <- setdiff(1:length(s$singleton),k)
# toKeepSeed <- c(toKeepSeed, s$singleton[k])
# toRemoveSeed <- c(toRemoveSeed,s$singleton[r])

toShow <- buildDisNet(ids = toKeepSeed, seed = toKeepSeed)
plotDisNet(toShow)

##=====================================
## Iteration 5
step5 <- reviewChildren(seedDisNet = seed, toKeep = toKeepSeed, toRemove = toRemoveSeed, terms = c("parkinson"))
lapply(step5,dim)

##=====================================
## IDs outside tree
otnodes <- outsideTree(seed,step5,toKeepSeed,toRemoveSeed)
names(otnodes)
lapply(otnodes$disNet,dim)
## 143 nodes outside tree

step6 <- reviewDisNet(otnodes,
                      terms = c("parkinson"))
lapply(step6$disNet,dim)
lapply(step6$review,length)

## Keep / Remove clusters
clusters <- step6$review$clusters
range(clusters$ind)
dim(clusters)
k <- c(1)
r <- setdiff(1:max(clusters$ind),k)
length(clusters$values[clusters$ind %in% k]) + length(clusters$values[clusters$ind %in% r])
toKeepSeed <- c(step6$review$toKeep,clusters$values[clusters$ind %in% k])
toRemoveSeed <- c(step6$review$toRemove, clusters$values[clusters$ind %in% r])
length(toKeepSeed) + length(toRemoveSeed)
dim(seed$nodes)

## SINGLETONS
singleton <- checkSingletons(step6,
                             toKeep = toKeepSeed,
                             toRemove = toRemoveSeed,
                             terms = "parkinson")

k <- c(11,13,17:24,26:28,35,36,39,44,46:48,53,58:84,86)
r <- setdiff(1:length(singleton$review$singleton),k)
toKeepSeed <- c(singleton$review$toKeep, singleton$review$singleton[k])
toRemoveSeed <- c(singleton$review$toRemove,singleton$review$singleton[r])
length(toKeepSeed) + length(toRemoveSeed)
nrow(seed$nodes)

toShow <- buildDisNet(ids = toKeepSeed, seed = toKeepSeed)
plotDisNet(toShow)

##==================================
## Check
table(c(toKeepSeed,toRemoveSeed) %in% seed$nodes$id)
## all seeds are check --> extend
table(seed$nodes$id %in% c(toKeepSeed,toRemoveSeed))

#################################################################################@
## STEP 2: extend
#################################################################################@
## Iteration 1: Go one level down --> get children and get all their xref
step7 <- reviewExtendSeed(toKeep = toKeepSeed, 
                          toRemove = toRemoveSeed, 
                          terms = c("parkinson"))
lapply(step7$disNet,dim)
length(step7$review$toKeep)
length(step7$review$toRemove)

## Keep / Remove clusters
# clusters <- step7$review$clusters
# range(clusters$ind)
# k <- c()
# r <- setdiff(1:max(clusters$ind),k)
# toKeep <- unique(c(step7$review$toKeep,clusters$values[clusters$ind %in% k]))
# toRemove <- unique(c(step7$review$toRemove, clusters$values[clusters$ind %in% r]))
# length(toKeep)
# length(toRemove)
# dim(step7$disNet$nodes)

# ## SINGLETONS
# s <- checkSingletons(disNet = step7,
#                      toKeep,
#                      toRemove,
#                      terms = "parkinson")
# toKeep <- s$toKeep
# toRemove <- s$toRemove
# k <- c(13,17,20:24,27:28,36,39,44,46:49,53,57,58:84,86)
# r <- setdiff(1:length(s$singleton),k)
# toKeep <- c(s$toKeep, s$singleton[k])
# toRemove <- c(s$toRemove,s$singleton[r])
# length(toKeep)
# length(toRemove)

# ## Building disease network
# fdisnet <- buildDisNet(ids = toKeep, seed = toKeep)
# lapply(fdisnet,dim)
