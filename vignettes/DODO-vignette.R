## ----setup, include = FALSE, warning=FALSE, echo=FALSE------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>"
# )
library(DODO)
connect_to_dodo(url = "http://localhost:7476")
library(BED)
library(TKCat)
library(here)
library(dplyr)
library(knitr)
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
library(ggplot2)
tkcon <- chTKCat()
# install.packages('webshot')
# webshot::install_phantomjs()

## Rerun calculations
rerun <- FALSE

## ----dataModel, include = TRUE, echo = FALSE, fig.cap='(ref:fig1)' , out.width='50%'----
knitr::include_graphics(here("inst/documentation/DODO-F1000-publication/fig/Figure1.jpg"))

## ----githubOntology, include = TRUE, echo = FALSE-----------------------------
do <- c("Monarch Disease Ontology (MonDO)", "Experimental Factor Ontology (EFO)", "Orphanet", "MedGen", "Medical Subject Headings (MeSH)", "Human Phenotype Ontology (HPO)", "ClinVar", "Disease Ontology (DO)", "International Classification of Diseases (ICD11)")
gh <- c("https://github.com/Elysheba/Monarch",
        "https://github.com/Elysheba/EFO",
        "https://github.com/Elysheba/Orphanet",
        "https://github.com/Elysheba/MedGen",
        "https://github.com/Elysheba/MeSH",
        "https://github.com/patzaw/HPO",
        "https://github.com/patzaw/ClinVar",
        "https://github.com/Elysheba/DO",
        "https://github.com/Elysheba/ICD11")
toShow <- tibble(`Disease ontology` = do,
                 GitHub = gh)
toShow %>%
  kable(caption = "Different disease ontologies included into DODO database and link to GitHub repository.",
        format = "latex", 
        booktabs = T) 

## ----listDB, out.width="50%", out.height="30%", fig.cap="Overview of the number of nodes present for each disease ontology in DODO. 30 ontologies have less than 100 entries in DODO are summarized as 'other'.", include = TRUE, echo = FALSE----
list_database() %>%
  mutate(database = case_when(count < 100 ~ "Other",
                              TRUE ~ database)) %>%
  group_by(database) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  mutate(database=factor(database, levels=database)) %>%   
  ggplot(aes(x = database, y = count)) +
  geom_point(color = "#66c2a5") +
  xlab(label = "Ontology") +
  ylab(label = "Number of nodes") +
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size=12)) + 
  scale_y_log10()

## ----nodeNb, include = TRUE, echo = FALSE-------------------------------------
type <- c("is_xref", "is_related", "has_pheno", "is_a", "is_alt")
nb <- c("280,691 (bidirectionally implemented)",
        "225,021 (bidirectionally implemented)", 
        "363,589",
        "221,360",
        "5,057")
toShow <- tibble(Relation = type,
                 "Number of edges" = nb)
toShow %>%
  kable(caption = "The number of edges for each relationship type present in DODO graph database",
        format = "latex", 
        booktabs = T) 

## ----dodobafa, echo = FALSE, include = TRUE, fig.cap="Example of transitivity mapping to infer an indirect relation with information on the ambiguity on the cross-reference edges.", , out.width='100%'----
knitr::include_graphics("fig/Figure3.png")

## ----exampleAmb, echo = FALSE, include = FALSE, eval = FALSE------------------
#  disNet <- build_disNet(id = "ORPHA:192")
#  disNet <- extend_disNet(disNet)
#  
#  col <- DODO:::color_database(disNet = disNet)
#  
#  disNet <- focus_disNet(disNet, diseaseID = "ORPHA:192", relationship = "xref", step = 2)
#  
#  edges <- disNet$xref %>%
#    dplyr::select(from,
#                  to,
#                  type,
#                  FA = forwardAmbiguity,
#                  BA = backwardAmbiguity) %>%
#    # filter(BA == 1 & FA == 1) %>%
#    bind_rows(disNet$xref %>%
#                filter(to == "ICD10:Q87.0") %>%
#                select(from,
#                  to,
#                  type,
#                  FA = forwardAmbiguity,
#                  BA = backwardAmbiguity)) %>%
#    dplyr::mutate(color = dplyr::case_when(grepl("is_related", type) ~ "#fc8d62",
#                                           TRUE ~ "#8da0cb"),
#                  title = paste(gsub("_nba", "", type), ", FA = ", FA, ", BA = ", BA),
#                  arrows = case_when(FA == 1 & BA == 1 ~ "to;from",
#                                     BA == 1 ~ "to",
#                                     FA == 1 ~ "from",
#                                     TRUE ~ "FALSE")) %>%
#    dplyr::distinct()
#  # edges <- disNet$children %>%
#  #   dplyr::select(from = parent,
#  #                 to = child,
#  #                 origin) %>%
#  #   dplyr::mutate(title = paste("is_a, origin = ", origin),
#  #                 arrows = "to",
#  #                 color = "#66c2a5") %>%
#  #   dplyr::bind_rows(edges)
#  
#  ## Nodes
#  nodes <- tibble(id = disNet$nodes$id,
#                  database = disNet$nodes$database,
#                  label = disNet$nodes$label) %>%
#    filter(id %in% c(edges$from, edges$to)) %>%
#    distinct() %>%
#    mutate(color = col[database],
#           shape =  "dot",
#           lbl = label,
#           label = case_when(is.na(lbl) ~ id,
#                        TRUE ~ paste(id, paste0(stringr::str_sub(lbl, start = 1, end = 21), "..."),
#                                     sep = "\n")),)
#  
#  ## Visnetwork
#  visNetwork::visNetwork(nodes = nodes,
#                         edges = edges) %>%
#    visOptions(highlightNearest = TRUE) %>%
#    visLayout(randomSeed = 123) %>%
#    visPhysics(solver = "barnesHut", barnesHut = list(avoidOverlap = 0.5))
#    # visNetwork::visIgraphLayout(smooth = TRUE)

## ----ICD10q87, echo = FALSE, out.width="100%", include = TRUE, fig.cap="(ref:fig4)"----
knitr::include_graphics("fig/Figure4.png")

## ---- include = FALSE, echo= FALSE--------------------------------------------
max_amb <- readr::read_csv(here("vignettes/ambiguity_between_ontologies.csv"))
oriAmb <- reshape2::acast(max_amb, formula = DB1 ~ DB2, value.var = "Max.")  %>%
  log10()

my_palette <- colorRampPalette(c("yellow", "darkgreen"))(n = 20)

jpeg(file = "fig/heatmap_ambiguity.jpg")
gplots::heatmap.2(x = oriAmb,
                  revC = T,
                  Rowv = TRUE,
                  Colv = TRUE,
                  trace = "none",
                  symbreaks = FALSE,
                  col = my_palette,
                  density.info = "none",
                  key.title = "Ambiguity",
                  key.xlab = "Ambiguity",
                  key.ylab = "Ambiguity")
dev.off()

## ----heatmapAmbiguity, echo = FALSE, out.width="80%", out.height="50%", include = TRUE, fig.cap="(ref:fig5)"----
# knitr::include_graphics("fig/Figure5.png")
gplots::heatmap.2(x = oriAmb,
                  revC = T,
                  Rowv = TRUE,
                  Colv = TRUE,
                  trace = "none",
                  symbreaks = FALSE,
                  col = my_palette,
                  density.info = "none",
                  key.title = "Ambiguity",
                  key.xlab = "Ambiguity",
                  key.ylab = "Ambiguity")

## ---- include = FALSE, echo = FALSE-------------------------------------------
load(here("tmp/All_benchmark_ambiguities.rda"))
monarch <- lapply(results_benchmark, function(x){x$Monarch})
names(monarch) <- 1:20
monarch <- plyr::compact(monarch)
lapply(monarch, function(x){x %>% dplyr::arrange(desc(n))})
toPlot <- lapply(monarch, function(x) x$n)

jpeg(file = "fig/boxplot_ambiguity_conversion_mondo.jpg")
boxplot(toPlot,
        log = "y",
        xlab = "Maximum ambiguity allowed to define is_xref",
        ylab = "Number of conversions for each identifier",
        main = "Ontology = Monarch" )
dev.off()

## ----boxplotAmbiguity, out.width="50%",out.height="20%", include = TRUE, echo = FALSE, fig.cap="(ref:fig6)"----
# knitr::include_graphics("fig/Figure6.jpg")
boxplot(toPlot,
        log = "y",
        xlab = "Maximum ambiguity allowed to define is_xref",
        ylab = "Number of conversions for each identifier",
        main = "Ontology = Monarch" )

## ---- include = FALSE, echo = FALSE-------------------------------------------
disNet <- build_disNet(id = "MONDO:0005027")
sdisNet <- extend_disNet(disNet = disNet)
                        
edisNet <- extend_disNet(disNet = disNet,
                        transitive.ambiguity = 1,
                        intransitive.ambiguity = NULL,
                        relations = "xref")
# col <- DODO:::color_database(disNet = disNet)

conv1 <- convert_concept(from = "MONDO:0005027",
                         to = "EFO",
                         from.concept = "Disease",
                         to.concept = "Disease",
                         step = 1,
                         intransitive_ambiguity = 1)
conv2 <- convert_concept(from = "MONDO:0005027",
                         to = "ORPHA",
                         from.concept = "Disease",
                         to.concept = "Disease",
                         step = NULL,
                         intransitive_ambiguity = 1)
conv3 <- convert_concept(from = "MONDO:0005027",
                         to = "ORPHA",
                         from.concept = "Disease",
                         to.concept = "Disease",
                         step = NULL,
                         intransitive_ambiguity = NULL) 
conv3
## Edges
edges <- edisNet$xref %>%
  dplyr::select(from, 
                to,
                type,
                FA = forwardAmbiguity,
                BA = backwardAmbiguity) %>%
  dplyr::mutate(color = dplyr::case_when(grepl("is_related", type) ~ "#fc8d62",
                                         TRUE ~ "#8da0cb"),
                title = paste(gsub("_nba", "", type), ", FA = ", FA, ", BA = ", BA),
                arrows = case_when(FA == 1 & BA == 1 ~ "to;from",
                                   BA == 1 ~ "to",
                                   FA == 1 ~ "from",
                                   TRUE ~ "FALSE"))

## Nodes
nodes <- edisNet$nodes %>%
  dplyr::select(id,
                database,
                label) %>%
  dplyr::mutate(color = case_when(#id %in% sdisNet$nodes$id ~ "#fb9a99",
    id == "MONDO:0005027" ~ "#feb24c",
    id %in% conv3$to ~ "#33a02c",
    id %in% conv2$to ~ "#1f78b4",
    id %in% conv1$to ~ "#e31a1c",
    TRUE ~ "#969696"),
    shape = case_when(id %in% edisNet$seed ~ "triangle",
                      TRUE ~ "dot"),
    lbl = label,
    label = case_when(is.na(lbl) ~ id,
                      TRUE ~ paste(id, paste0(stringr::str_sub(lbl, start = 1, end = 21), "..."), sep = "\n")),
    title = paste(id, lbl, sep = " | "))


## Visnetwork
p <- visNetwork::visNetwork(nodes = nodes,
                       edges = edges) %>%
  visNetwork::visIgraphLayout(smooth = TRUE)

## ----extension, include = TRUE, echo = FALSE, out.width="120%", fig.cap='(ref:fig7)'----
# knitr::include_graphics("fig/Figure7.png")
p

## ---- include = TRUE, echo = TRUE---------------------------------------------
## Use case 1
conv <- convert_concept(from = "MONDO:0005027",
                         to = "EFO",
                         from.concept = "Disease",
                         to.concept = "Disease",
                         step = 1,
                         intransitive_ambiguity = 1)
conv

## ---- include = TRUE, echo = TRUE---------------------------------------------
## Use case 2
conv <- convert_concept(from = "MONDO:0005027",
                         to = "ORPHA",
                         from.concept = "Disease",
                         to.concept = "Disease",
                         step = NULL,
                         intransitive_ambiguity = 1)
conv

## ---- include = TRUE, echo = TRUE---------------------------------------------
## Use case 3
conv <- convert_concept(from = "MONDO:0005027",
                        to = "ORPHA",
                        from.concept = "Disease",
                        to.concept = "Disease",
                        step = NULL,
                        intransitive_ambiguity = NULL) 
conv

## ---- include = TRUE, echo = TRUE---------------------------------------------
## Use case 4
## convert_concept()
conversion <- convert_concept(from = "ICD10:G40.9",
                              to = "DOID", 
                              from.concept = "Disease",
                              to.concept = "Disease")
conversion

## get_related()
related <- get_related(from = "ICD10:G40.9",
                       to = "DOID", 
                       from.concept = "Disease",
                       to.concept = "Disease")
related

## ---- include = TRUE, echo = FALSE--------------------------------------------
conversion <- convert_concept(from = "ICD10:G40.9",
                              # to = "DOID",
                              from.concept = "Disease",
                              to.concept = "Disease")
related <- get_related(from = "ICD10:G40.9",
                        from.concept = "Disease",
                        to.concept = "Disease")
conv <- get_related(from = "ICD10:G40.9",
                    to = "DOID",
                    from.concept = "Disease",
                    to.concept = "Disease")
disNet <- build_disNet(id = c(related$from, related$to))
## Edges
edges <- disNet$xref %>%
  dplyr::select(from, 
                to,
                type,
                FA = forwardAmbiguity,
                BA = backwardAmbiguity) %>%
  dplyr::mutate(color = dplyr::case_when(grepl("is_related", type) ~ "#fc8d62",
                                         TRUE ~ "#8da0cb"),
                title = paste(gsub("_nba", "", type), ", FA = ", FA, ", BA = ", BA),
                arrows = case_when(FA == 1 & BA == 1 ~ "to;from",
                                   BA == 1 ~ "to",
                                   FA == 1 ~ "from",
                                   TRUE ~ "FALSE"))

## Nodes
nodes <- tibble(id = unique(c(related$from, related$to))) %>%
  distinct() %>%
  mutate(database = gsub(":.*", "", id),
         color = case_when(id %in% unique(c(conversion$to, conversion$from)) ~ "#99d594",
                           id %in% conv$to ~ "#fc8d59",
                           TRUE ~ "#969696"),
         shape = case_when(id %in% related$from ~ "triangle",
                           TRUE ~ "dot"),
         lbl = disNet$nodes$label[match(id, disNet$nodes$id)],
         label = case_when(is.na(lbl) ~ id,
                      TRUE ~ paste(id, paste0(stringr::str_sub(lbl, start = 1, end = 21), "..."), 
                                   sep = "\n")))
## Visnetwork
p <- visNetwork::visNetwork(nodes = nodes,
                       edges = edges) %>%
  visNetwork::visIgraphLayout(smooth = TRUE)

## ----getRelated, include = TRUE, out.width="120%", echo = FALSE, fig.cap="(ref:fig8)"----
p
# knitr::include_graphics("fig/Figure8.png")

## ---- include = TRUE, echo = TRUE---------------------------------------------
## From disease to phenotype
toPhenotype <- convert_concept(from = "MONDO:0012391",
                               to = "HP",
                               from.concept = "Disease",
                               to.concept = "Phenotype") 
toPhenotype <- toPhenotype %>%
  mutate(diseaseLabel = describe_concept(from)$label,
         phenotypeLabel = describe_concept(to)$label)
toPhenotype

## From phenotype to disease
toDisease <- convert_concept(from = "HP:0002384",
                               from.concept = "Phenotype",
                               to.concept = "Disease") 
toDisease <- toDisease %>%
  mutate(phenotypeLabel = describe_concept(from)$label,
         diseaseLabel = describe_concept(to)$label)
toDisease

## ---- include = TRUE, echo = TRUE---------------------------------------------
deprecated <- convert_concept(from = "HP:0009638",
                               deprecated = TRUE,
                               from.concept = "Phenotype",
                               to.concept = "Phenotype") 
deprecated

## ---- include = FALSE, echo = FALSE-------------------------------------------
mondo <- get_ontology("MONDO")

## option 1
conv1 <- convert_concept(from = mondo$nodes$id,
                         to = "EFO",
                         from.concept = "Disease",
                         to.concept = "Disease",
                         step = 1)
summary(conv1 %>% count(from) %>% pull(n))

conv2 <- convert_concept(from = mondo$nodes$id,
                         to = "EFO",
                         from.concept = "Disease",
                         to.concept = "Disease",
                         step = NULL, 
                         intransitive_ambiguity = 1)
summary(conv2 %>% count(from) %>% pull(n))


conv3 <- convert_concept(from = mondo$nodes$id,
                         to = "EFO",
                         from.concept = "Disease",
                         to.concept = "Disease",
                         step = NULL, 
                         intransitive_ambiguity = NULL)
summary(conv3 %>% count(from) %>% pull(n))

##########################################
a <- build_disNet("MONDO:0019587")
a <- extend_disNet(a)
b <- focus_disNet(a, diseaseID = "MONDO:0019587", steps = 1)
col <- DODO:::color_database(disNet = a)

edges <- b$xref %>%
    dplyr::select(from, 
                  to,
                  type,
                  FA = forwardAmbiguity,
                  BA = backwardAmbiguity) %>%
    dplyr::mutate(color = dplyr::case_when(grepl("is_related", type) ~ "#fc8d62",
                                           TRUE ~ "#8da0cb"),
                  title = paste(gsub("_nba", "", type), ", FA = ", FA, ", BA = ", BA),
                  arrows = case_when(FA == 1 & BA == 1 ~ "to;from",
                                     BA == 1 ~ "to",
                                     FA == 1 ~ "from",
                                     TRUE ~ "FALSE"))
  
## Nodes
nodes <- tibble(id = b$nodes$id,
                database = b$nodes$database,
                label = b$nodes$label) %>%
  distinct() %>%
  mutate(color = col[database],
         shape = case_when(id %in% b$seed ~ "triangle",
                          TRUE ~ "dot"),         
         lbl = label,
         label = case_when(is.na(lbl) ~ id,
                      TRUE ~ paste(id, paste0(stringr::str_sub(lbl, start = 1, end = 40), "..."), 
                                   sep = "\n")))
## Visnetwork
p <- visNetwork::visNetwork(nodes = nodes,
                       edges = edges) %>%
  visNetwork::visIgraphLayout(smooth = TRUE)

## ----effConversion, include = TRUE, out.width="100%", echo = FALSE, fig.cap="Direct cross-reference of MonDO identifier for 'autosomal dominant non-syndromic deafness' (MONDO:0019587)"----
# visNetwork::visNetwork(nodes = nodes,
#                        edges = edges) %>%
#   visNetwork::visIgraphLayout(smooth = TRUE) %>% 
#    visPhysics(solver = "forceAtlas2Based",
#                forceAtlas2Based = list(gravitationalConstant = -3000, avoidOverlap = 1,
#                                        springlength = 100))
knitr::include_graphics("fig/Figure9.png")

## ----benchmark, include = TRUE, out.width = "100%", warning=FALSE, echo = FALSE, message=FALSE----
# Comparison of different conversion strategies using the MonDO ontology
toShow <- readxl::read_xlsx(here("inst/documentation/DODO-F1000-publication/data/benchmark_summary.xlsx"), sheet = "Sheet2") 
# toShow <- toShow %>%
#   slice(-2, -6, -7, -11)

toShow$...1[c(1,4,7)] <- paste0("Direct conversion", 
                                      footnote_marker_number(5))
toShow$...1[c(2,5,8)] <- paste0("Indirect strict conversion", 
                                      footnote_marker_number(6))
toShow$...1[c(3,6,9)] <- paste0("Indirect extended conversion", 
                                      footnote_marker_number(7))
# LaTeX Table
kable(toShow, 
      col.names = c(" ", " ", " ", "Median", "Mean", "Max", ""),
      format = "latex", 
      booktabs = T,
      escape = FALSE,
      caption = "Comparison of different conversion strategies using the MonDO ontology") %>%
   kable_styling(latex_options = c("striped", "scale_down")) %>%
   add_header_above(c(" ", "(ref:c1)", "(ref:c2)", "(ref:c3)" = 3, "(ref:c4)")) %>%
   pack_rows("EFO", 1, 3) %>%
   pack_rows("MeSH", 4, 6) %>%
   pack_rows("DO", 7, 9) %>%
   add_footnote(c("Number of unique MonDO identifiers with a conversion",
                  "Number of unique converted identifiers in the targeted ontology",
                  "Distribution of the number of conversions returned per MonDO identifier.",
                  "Number of MonDO identifiers with ambiguous conversions",
                  "Conversion as performed by use case 1: direct conversion (see above), the parameter is step = 1", 
                  "Conversion as performed by use case 2: strict indirect conversion (see above), the parameters are step = NULL and intransitive_ambiguity = 1", 
                  "Conversion as performed by use case 3: extended indirect conversion (see above), the parameters are step = NULL and intransitive_ambiguity = NULL)"),
                notation="number") 

## ---- echo  = TRUE, eval = FALSE----------------------------------------------
#  mondo <- get_ontology("MONDO")
#  
#  ## option 1
#  conv1 <- convert_concept(from = mondo$nodes$id,
#                           to = "EFO",
#                           from.concept = "Disease",
#                           to.concept = "Disease",
#                           step = 1)
#  summary(conv1 %>% count(from) %>% pull(n))
#  
#  conv2 <- convert_concept(from = mondo$nodes$id,
#                           to = "EFO",
#                           from.concept = "Disease",
#                           to.concept = "Disease",
#                           step = NULL,
#                           intransitive_ambiguity = 1)
#  summary(conv2 %>% count(from) %>% pull(n))
#  
#  
#  conv3 <- convert_concept(from = mondo$nodes$id,
#                           to = "EFO",
#                           from.concept = "Disease",
#                           to.concept = "Disease",
#                           step = NULL,
#                           intransitive_ambiguity = NULL)
#  summary(conv3 %>% count(from) %>% pull(n))

## ---- include = TRUE, echo = TRUE---------------------------------------------
disNet <- build_disNet(term = "amyotrophic lateral sclerosis", 
                       fields = c("label", "synonym"))
disNet

## ---- include = TRUE, echo = TRUE---------------------------------------------
disNet <- build_disNet(term = "amyotrophic lateral sclerosis",
                       fields = c("label", "synonym"))
extendedDisNet <- extend_disNet(disNet,
                                relations = c("xref", "child"),
                                intransitive.ambiguity = 1)
extendedDisNet

## ---- include = FALSE, echo = FALSE-------------------------------------------
##############################@
## Compare disNet and extension
## Edges
edges <- extendedDisNet$xref %>%
  dplyr::select(from, 
                to,
                type,
                FA = forwardAmbiguity,
                BA = backwardAmbiguity) %>%
  dplyr::mutate(color = dplyr::case_when(grepl("is_related", type) ~ "#fc8d62",
                                         TRUE ~ "#8da0cb"),
                title = paste(gsub("_nba", "", type), ", FA = ", FA, ", BA = ", BA),
                arrows = case_when(FA == 1 & BA == 1 ~ "to;from",
                                   BA == 1 ~ "to",
                                   FA == 1 ~ "from",
                                   TRUE ~ "FALSE"))

## Nodes
nodes <- tibble(id = extendedDisNet$nodes$id,
                database = extendedDisNet$nodes$database) %>%
  distinct() %>%
  mutate(color = case_when(id %in% disNet$nodes$id ~ "#99d594",
                           TRUE ~ "#fc8d59"),
         shape =  "dot",
         label = id)

## Visnetwork
p <- visNetwork::visNetwork(nodes = nodes,
                       edges = edges) %>%
  visNetwork::visIgraphLayout(smooth = TRUE)

## ----alsplotdisNet, include = TRUE, out.width = "110%", echo = FALSE, fig.cap="(ref:fig10)"----
# knitr::include_graphics("fig/Figure10.png")
p

## ---- include = TRUE, echo = TRUE---------------------------------------------
disNet <- build_disNet(id = c("HP:0003394", "HP:0002180", "HP:0002878"))
disNet <- extend_disNet(disNet = disNet, relations = "disease")
disNet

## ----plotexample, include = TRUE, plotly = TRUE, warnings = FALSE, message = FALSE, out.width="100%", echo = FALSE, fig.cap="(ref:fig11)"----
disNet <- build_disNet(id = "MONDO:0004976")
disNet <- extend_disNet(disNet, step = 1)
plot(disNet)
# knitr::include_graphics("fig/Figure11.png")

## ---- include = FALSE, echo = TRUE, eval = FALSE------------------------------
#  disNet <- build_disNet(id = "MONDO:0004976")
#  disNet <- extend_disNet(disNet, step = 1)
#  # plot(disNet)

## ---- include = TRUE, echo = TRUE---------------------------------------------
disNet <- build_disNet(term = "amyotrophic lateral sclerosis", 
                       fields = c("label", "synonym"))
clDisNet <- cluster_disNet(disNet = disNet,
                          clusterOn = "xref")
clDisNet


explore_disNet(clDisNet)

## ----cldisNet, echo = FALSE, include = TRUE-----------------------------------
## Explore clusters
counter <- 1
toShow <- do.call(rbind,
                  lapply(clDisNet,
                         function(x){
                           toRet <- x$nodes %>%
                             dplyr::arrange(level, label) %>%
                             dplyr::mutate(clusterSize = length(id),
                                           level = as.character(level)) %>%
                             dplyr::slice(1) %>%
                             dplyr::mutate(label,
                                           cluster = counter,
                                           level = case_when(is.na(level) ~ "",
                                                             TRUE ~ level)) %>%
                             dplyr::select(cluster, clusterSize, id, label) 
                           counter <<- counter + 1
                           return(toRet)
                         })
)
kable(toShow,
      row.names = FALSE, 
      caption = "Annotation of the different cross-reference clusters of nodes identified for a disNet around 'amyotrophic lateral sclerosis'. ",
        format = "latex", 
        booktabs = T)   %>%
  kable_styling(latex_options = c("striped", "scale_down"))

## ---- include = FALSE, echo = TRUE--------------------------------------------
disNet <- build_disNet(term = "amyotrophic lateral sclerosis", 
                       fields = c("label", "synonym"))
extendedDisNet <- extend_disNet(disNet,
                                relations = c("xref", "child"),
                                intransitive.ambiguity = 1)

## ---- include = FALSE, echo = FALSE, message = FALSE, warning=FALSE-----------
# if(!rerun){
#   load(here("inst/documentation/DODO-F1000-publication/data/als_disNet_to_externalResources.rda"))
#   load(here("inst/documentation/DODO-F1000-publication/data/als_direct_chembl_clinvar_results.rda"))
# }else{
  ## Obtain all drugs for psoriasis using CHEMBL version 25

## Load resources
## CHEMBL 25
## Clinvar 27/03/2020
ClinVar_rcvaVariant <- read_tsv(
  here("inst/documentation/DODO-F1000-publication/data/clinvar_2020-03-27/ClinVar_rcvaVariant.txt"))
ClinVar_variants <- read_tsv(
  here("inst/documentation/DODO-F1000-publication/data/clinvar_2020-03-27/ClinVar_variants.txt"))
ClinVar_varEntrez <- read_tsv(
  here("inst/documentation/DODO-F1000-publication/data/clinvar_2020-03-27/ClinVar_varEntrez.txt"))
ClinVar_rcvaTraits <- read_tsv(
  here("inst/documentation/DODO-F1000-publication/data/clinvar_2020-03-27/ClinVar_rcvaTraits.txt"))
ClinVar_traitNames <- read_tsv(
  here("inst/documentation/DODO-F1000-publication/data/clinvar_2020-03-27/ClinVar_traitNames.txt"))
CHEMBL_drug_indication <- read_tsv(
  here("inst/documentation/DODO-F1000-publication/data/chembl_25/CHEMBL_drug_indication.txt"))
clinvarVariants <- ClinVar_rcvaVariant %>%
    inner_join(ClinVar_variants,
               by = c("varId" = "id")) %>%
    inner_join(ClinVar_varEntrez,
               by = c("varId")) %>%
    select(varId,
           rcvaId,
           variantType = type.x,
           entrez)

#############################@
## disNet
chembl_disnet <- CHEMBL_drug_indication %>%
  mutate(dbid = paste(DB, id, sep = ":")) %>%
  filter(dbid %in% extendedDisNet$nodes$id)
## Number of identified compounds
length(unique(chembl_disnet$molregno))

clinvar_disnet <- ClinVar_rcvaTraits %>%
  mutate(dbid = paste("ClinVar", t.id, sep = ":")) %>%
  filter(dbid %in% extendedDisNet$nodes$id) %>%
  inner_join(clinvarVariants,
             by = c("rcvaId"))
## Number of identified genes carrying a disease variant
length(unique(clinvar_disnet$entrez))

##########################@
## Query resource directly
## CHEMBL
chembl_als <- CHEMBL_drug_indication %>%
  filter(grepl("amyotrophic lateral sclerosis", name, ignore.case = T)) %>%
  mutate(dbid = paste(DB, id, sep = ":"))
## Number of identified compounds
length(unique(chembl_als$molregno))
## Compounds identified through use of disNet or CHEMBl directly
table(unique(chembl_disnet$molregno) %in% unique(chembl_als$molregno))

## ClinVar
clinvar_als <- ClinVar_traitNames %>%
  filter(grepl("amyotrophic lateral sclerosis", name, ignore.case = TRUE)) %>%
  inner_join(ClinVar_rcvaTraits,
             by = "t.id") %>%
  inner_join(clinvarVariants,
             by = "rcvaId") %>%
  mutate(dbid = paste("ClinVar", t.id, sep = ":"))
## Number of identified genes carrying a disease variant
length(unique(clinvar_als$entrez))
## Genes identified through use of disNet or ClinVar directly
table(unique(clinvar_als$entrez) %in% unique(clinvar_disnet$entrez))

  # save(chembl_als, clinvar_als,
  #      file = here("inst/documentation/DODO-F1000-publication/data/02042020_als_direct_chembl_clinvar_results.rda"))
# }

## ----chemblDisNetind, include = TRUE, echo = FALSE----------------------------
toShow <- chembl_disnet %>% 
  select(dbid, name, molregno) %>%
  distinct() %>%
  group_by(dbid) %>%
  count(dbid) %>%
  distinct()

kable(toShow,
      col.names = c("Disease identifier", "Number of compounds"),
      row.names = FALSE,
      caption = "Using the disNet to connect to CHEMBL results identifies compounds available for different disease identifiers listed here.") 
    # kable_styling(latex_options = c("striped", "scale_down"))


## ---- include = TRUE, echo = FALSE--------------------------------------------
toShow <- build_disNet(id = unique(chembl_disnet$dbid))
col <- DODO:::color_database(disNet = toShow)
## Edges
edges <- toShow$xref %>%
  dplyr::select(from, 
                to,
                type,
                FA = forwardAmbiguity,
                BA = backwardAmbiguity) %>%
  dplyr::mutate(color = dplyr::case_when(grepl("is_related", type) ~ "#fc8d62",
                                         TRUE ~ "#8da0cb"),
                title = paste(gsub("_nba", "", type), ", FA = ", FA, ", BA = ", BA),
                arrows = case_when(FA == 1 & BA == 1 ~ "to;from",
                                   BA == 1 ~ "to",
                                   FA == 1 ~ "from",
                                   TRUE ~ "FALSE"))
edges <- toShow$children %>%
  dplyr::select(from = parent, 
                to = child,
                origin) %>%
  dplyr::mutate(title = paste("is_a, origin = ", origin),
                arrows = "to",
                color = "#66c2a5") %>%
  dplyr::bind_rows(edges)

## Nodes
nodes <- tibble(id = toShow$nodes$id,
                database = toShow$nodes$database,
                label = toShow$nodes$label) %>%
  distinct() %>%
  mutate(color = col[database],
         shape =  "dot",
         lbl = label,
         label = case_when(is.na(lbl) ~ id,
                      TRUE ~ paste(id, paste0(stringr::str_sub(lbl, 
                                                               start = 1, 
                                                               end = 21), "..."), 
                                   sep = "\n")),)

## Visnetwork
p <- visNetwork::visNetwork(nodes = nodes,
                       edges = edges) %>%
  visNetwork::visIgraphLayout(smooth = TRUE) %>%
  visHierarchicalLayout(nodeSpacing = 200)

## ----disnetALSchembl, include = TRUE, echo = FALSE, out.width="100%", fig.cap="(ref:fig12)"----
# include_graphics("fig/Figure12.png")
p

## ---- include = FALSE, echo = FALSE-------------------------------------------
clDisNet <- cluster_disNet(disNet = extendedDisNet,
                           clusterOn = "xref")
explore_disNet(clDisNet)
clDisNet <- clDisNet[c(1:2, 4:6, 8:9, 13:20, 22, 24:28)]
fedisNet <- merge_disNet(list = clDisNet)

## ClinVar
clinvar_disnet <- ClinVar_rcvaTraits %>%
  mutate(dbid = paste("ClinVar", t.id, sep = ":")) %>%
  filter(dbid %in% fedisNet$nodes$id) %>%
  inner_join(clinvarVariants,
             by = c("rcvaId"))
## Number of identified genes carrying a disease variant
length(unique(clinvar_disnet$entrez))
table(unique(clinvar_als$entrez) %in% unique(clinvar_disnet$entrez))

## CHEMBL
chembl_disnet <- CHEMBL_drug_indication %>%
  mutate(dbid = paste(DB, id, sep = ":")) %>%
  filter(dbid %in% fedisNet$nodes$id)
## Number of identified compounds
length(unique(chembl_disnet$molregno))
## Compounds identified through use of disNet or CHEMBl directly
table(unique(chembl_disnet$molregno) %in% unique(chembl_als$molregno))

## ----disnetALSclinvar, include = TRUE, echo = FALSE, out.width="120%", fig.cap="(ref:fig13)"----
toPlot <- focus_disNet(extendedDisNet,
                  diseaseID = "ClinVar:18286",
                  relationship = "xref",
                  step = 2)

edges <- toPlot$xref %>%
  dplyr::select(from, 
                to,
                type,
                FA = forwardAmbiguity,
                BA = backwardAmbiguity) %>%
  dplyr::mutate(color = dplyr::case_when(grepl("is_related", type) ~ "#fc8d62",
                                         TRUE ~ "#8da0cb"),
                title = paste(gsub("_nba", "", type), ", FA = ", FA, ", BA = ", BA),
                arrows = case_when(FA == 1 & BA == 1 ~ "to;from",
                                   BA == 1 ~ "to",
                                   FA == 1 ~ "from",
                                   TRUE ~ "FALSE"))
edges <- toPlot$children %>%
  dplyr::select(from = parent, 
                to = child,
                origin) %>%
  dplyr::mutate(title = paste("is_a, origin = ", origin),
                arrows = "to",
                color = "#66c2a5") %>%
  dplyr::bind_rows(edges)

## Nodes
nodes <- tibble(id = toPlot$nodes$id,
                database = toPlot$nodes$database,
                label = toPlot$nodes$label) %>%
  distinct() %>%
  mutate(color = col[database],
         shape =  case_when(id == "ORPHA:52430" ~ "triangle",
                            TRUE ~ "dot"),
         lbl = label,
         label = case_when(is.na(lbl) ~ id,
                      TRUE ~ paste(id, paste0(stringr::str_sub(lbl, 
                                                               start = 1, 
                                                               end = 21), "..."), 
                                   sep = "\n")),)

## Visnetwork
visNetwork::visNetwork(nodes = nodes,
                       edges = edges) %>%
  visNetwork::visIgraphLayout(smooth = TRUE) %>%
  visPhysics(solver = "barnesHut",barnesHut = list(avoidOverlap = 1, 
                                                   centralGravity = 0.1,
                                                   gravitationalConstant = -55000))

# knitr::include_graphics("fig/Figure13.png")

## ---- echo = TRUE, include = FALSE, eval = FALSE------------------------------
#  disNet <- build_disNet(term = "amyotrophic lateral sclerosis",
#                         fields = c("label", "synonym"))
#  extendedDisNet <- extend_disNet(disNet,
#                                  relations = c("xref", "child"),
#                                  intransitive.ambiguity = 1)

## ---- include = TRUE, echo = TRUE---------------------------------------------
##########################@
## reviewing disNet
clDisNet <- cluster_disNet(disNet = extendedDisNet,
                           clusterOn = "xref")
explore_disNet(clDisNet)
clDisNet <- clDisNet[c(1:2, 4:6, 8:9, 13:20, 22, 24:28)]
fedisNet <- merge_disNet(list = clDisNet)

## ---- include = TRUE, echo = FALSE--------------------------------------------
comp_disNet <- build_disNet(id = c(unique(clinvar_disnet$dbid),
                                      unique(chembl_disnet$dbid)),
                            ambiguity = NULL)
comp_disNet <- extend_disNet(comp_disNet,
                            step = 1,
                            relations = "xref")
nodes <- comp_disNet$nodes %>%
  mutate(title = paste(id, label, sep = "|"),
         color = case_when(id %in% clinvar_disnet$dbid ~ "#67a9cf",
                           id %in% chembl_disnet$dbid ~ "#ef8a62",
                           TRUE ~ "#999999"),
         label = case_when(color == "#67a9cf" ~ "ClinVar",
                           color == "#ef8a62" ~ "ChEMBL",
                           TRUE ~ ""))
## Edges
edges <- comp_disNet$xref %>%
    dplyr::select(from, 
                  to,
                  type,
                  FA = forwardAmbiguity,
                  BA = backwardAmbiguity) %>%
    dplyr::mutate(color = dplyr::case_when(grepl("is_related", type) ~ "#fc8d62",
                                           TRUE ~ "#8da0cb"),
                  title = paste(gsub("_nba", "", type), ", FA = ", FA, ", BA = ", BA),
                  arrows = case_when(FA == 1 & BA == 1 ~ "to;from",
                                     BA == 1 ~ "to",
                                     FA == 1 ~ "from",
                                     TRUE ~ "FALSE"))

p <- visNetwork::visNetwork(nodes = nodes,
                       edges = edges) %>%
  visNetwork::visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
                         collapse = list(enabled = TRUE)) %>%
  visNetwork::visIgraphLayout()

## ----traceConn, include = TRUE, echo = FALSE, out.width = "120%", fig.cap = "(ref:fig14)"----
p
# knitr::include_graphics("fig/Figure14.png")

## ----functions, include = TRUE, echo = FALSE----------------------------------
ftie <- c("build_disNet", 
          "extend_disNet", 
          "filter_by_id", 
          "filter_by_database", 
          "focus_disNet", 
          "cluster_disNet", 
          "setdiff_disNet", 
          "split_disNet", 
          "explore_disNet",
          "show_relations", 
          "plot.disNet", 
          "convert_concept",
          "get_related",
          "check_dodo_connection", 
          "connect_to_dodo", 
          "forget_dodo_connection",
          "list_dodo_connections",
          "call_dodo", 
          "show_dodo_model", 
          "get_version", 
          "get_concept_url", 
          "list_database", 
          "list_node_type", 
          "get_ontology", 
          "describe_concept")
descr <- c("Building a network of disease identifiers",
           "Extending a disNet by different edges",
           "Filtering a disNet by id",
           "Filtering a disNet by database", 
           "Focus on identifiers of interest and its neighbors",
           "Clustering a disNet, generates a setDisNet",
           "Substract one disNet from another",
           "Split a disNet based on a list of identifiers into a setDisNet",
           "Visualizes a datatable to explore a disNet",
           "Visualizes cross-reference relationships for the provided identifier",
           "Visualizes a disNet using visNetwork",
           "Convert the provided set of identifiers to another ontology or between concepts",
           "Convert function for ontologies separated by *is_related* edges mainly",
           "Check connection with DODO graphical database",
           "Establish connection with DODO graphical database",
           "Forget a saved connection to DODO",
           "List all saved connections to DODO",
           "Calls a function on the DODO graphical database",
           "Return DODO data model",
           "Return DODO database version", 
           "Returns concept url", 
           "Lists databases in DODO", 
           "Lists node type in DODO", 
           "Returns whole ontology", 
           "Returns concept description")
scope <- c(rep("Disease network functions", 8),
           rep("Visualization and exploration", 3),
           rep("Conversion", 2),
           rep("Connection and low-level interactions", 9),
           rep("Data information", 3))
toShow <- tibble(Function = ftie,
                 Description = descr,
                 scope = scope)
toShow %>%
  kable(caption = "List of all functions available in DODO R package with description and scope details.",
        format = "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

## ---- include = TRUE, echo = FALSE, message = FALSE, warning=FALSE------------
toShow <- readxl::read_xlsx(here("inst/documentation/CrossreferencesEdges.xlsx"), sheet = "Max_ambiguity_4_across_DBs") 
toShow %>%
  kable(caption = "(ref:tab)",
        format = "latex", booktabs = T) 
  # kable_styling(latex_options = c("striped", "scale_down"))

