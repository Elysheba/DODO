#' ##################################################################################
#' Shiny app to identify and browse indications
#' 
#' @import shiny
#' @importFrom shinythemes shinytheme
#' @importFrom DT DTOutput renderDT
#' 
#' @export

shinyConcept <- function(){
  library(shiny)
  library(DT)
  library(shinythemes)
  
  if(!DODO:::check_dodo_connection()){
    stop("No connection to DODO instance")
  }
  
  ui <- fluidPage(theme = shinytheme("cerulean"),
                  # navbarPage(title = "UCB - New Medicines - Braine on bel040344",
                  fluidRow(column(11,
                                  conceptSearchInput(id = "query",
                                                     internal = TRUE)),
                           column(1,
                                  actionButton(inputId = "done",
                                               label = "Done"))
                  ),
                  fluidRow(DTOutput(outputId = "search"))
  )
  
  
  server <- function(input, output, session){
    ## setup -----
    outputSearch <- reactiveVal()
    
    ## search concept----
    outputSearch <- callModule(conceptSearch,
                               id = "query",
                               internal = TRUE) 
    
    ## Return diseases that match query and show datatable ----
    output$search <- renderDT({
      validate(need(outputSearch(), ""))
      toShow <- outputSearch() %>%
        dplyr::mutate(database = as.factor(database)) %>%
        dplyr::select("Concept identifier" = id,
                      "Concept database" = database,
                      "Concept" = label,
                      "Level in hierarchy" = level,
                      "Synonyms" = synonym,
                      "Definition" = definition)
      
      DT::datatable(
        toShow,
        selection=list(mode="multiple", selected=c(), target="row"),
        filter = "top",
        escape=FALSE,
        rownames = FALSE
      )
    })
    
    ## Done button and return selected IDs ----
    observeEvent(input$done, {
      validate(need(outputSearch(), ""))
      validate(need(input$search_rows_selected, ""))
      tmp <- outputSearch() %>%
        dplyr::slice(input$search_rows_selected) %>%
        dplyr::pull(id) 
      ## Print the IDs in the console
      print(tmp)
      ## Return disNet 
      tmp <- build_disNet(id = tmp)
      stopApp(invisible(tmp))
    })
  }
  
  
  
  # shinyApp(ui = ui, server = server)
  runGadget(app = ui,
            server = server,
            viewer = dialogViewer("Find disease or phenotype",
                                  height=560,
                                  width=850))
}


##########################################################################@
#' Shiny ui module to search concepts
#' 
#' Shiny module to browse diseases and phenotypes by id (db:id) or term 
#' @return disNet
#' 
#' @export
#' 
#' @import shiny
#' 
conceptSearchInput <- function(id, 
                               label = "Enter search term", 
                               internal = TRUE){
  ns <- NS(id)
  
  if(internal){
    tagList(
      fluidRow(
        column(4,
               textInput(ns("searchTerm"),
                         label = label)),
        column(2,
               uiOutput(outputId = ns("fields"))),
        # column(2,
        #        checkboxInput(inputId = ns("exact"),
        #                      label = "Exact match (Y/N)",
        #                      value = FALSE)),
        column(4,
               uiOutput(outputId = ns("database"))),
        column(2,
               actionButton(inputId = ns("do"), label = "Search"))
      )
    )
  }else{
    tagList(
      fluidRow(
        column(6,
               textInput(ns("searchTerm"),
                         label = label)),
        column(4,
               uiOutput(outputId = ns("fields"))),
        column(2,
               actionButton(inputId = ns("do"), label = "Search"))
        # column(3,
        #        checkboxInput(inputId = ns("exact"),
        #                      label = "Exact match (Y/N)",
        #                      value = FALSE))
      )
    )
  }
}

####################################################################################@
#' Shiny server module to search concepts
#' 
#' Shiny module to browse diseases and phenotypes by id (db:id) or term 
#' @return disNet
#' @export
#' @import stringr
#' 
conceptSearch <- function(input, output, session, internal = TRUE){
  # Define UI for application that draws a histogram
  ## DODO dbs
  db <- DODO::list_database()$database
  
  ## search fields
  output$fields <- renderUI({
    ns <- session$ns
    checkboxGroupInput(inputId = ns("fields"),
                       label = "Search fields",
                       choices = c("label","synonym"),
                       selected = c("label","synonym") )
  })
  
  ## Specify db
  if(internal){
    output$database <- renderUI({
      ns <- session$ns
      selectInput(inputId = ns("database"),
                  label = "Database",
                  choices = as.list(c("Not specified", sort(db))),
                  multiple = FALSE)
    })
  }
  
  ## Reactive value init
  term <- reactiveVal(NULL)
  
  ## Search term update
  observeEvent(input$do,{
    # print(input$searchTerm)
    validate(need(input$searchTerm, ""),
             need(input$searchTerm, "Please enter a search"))
    # need(stringr::str_length(input$searchTerm) >= 3, "Search term needs to be longer than 3 characters"))
    term(input$searchTerm)
  }) 
  
  ## Do query and return dataframe
  toRet <- reactive({
    validate(need(term(), ""))
    
    if(internal){
      if(input$database == "Not specified" || is.null(input$database)){
        database <- NULL
      }else{
        database <- input$database
      }
    }else{
      database <- "MONDO"
    }
    
    if(grepl(paste(db, collapse = "|"),term())){
      type <- "id"
    }else{
      type <- "term"
    }
    ##
    tmp <- try(DODO:::searchDODO(searchTerm = term(),
                                   type =  type,
                                   fields = input$fields,
                                   database = database),
               # exact = input$exact),
               silent = T)
    
    ## Notifications
    if(is.null(tmp) || inherits(tmp, "try-error")){
      tmp <- NULL
      showNotification("No results", session = session)
    }else{
      showNotification(paste0(nrow(tmp), " results"),
                       session = session)
    }
    if(type == "term" & !is.null(tmp)){
      tmp %>% dplyr::mutate(label = DODO:::highlightText(text = label, value = term()),
                            synonym = DODO:::highlightText(text = synonym, value = term()),
                            definition = DODO:::highlightText(text = definition, value = term()))
    }else{
      tmp
    }
  })
  return(toRet)
}


###############################################################################@
#' Search concept
#' 
#' @param searchTerm a term to search for, can be either an ID or a term
#' @param database if provided, 
#' @param type the type of the searchTerm, either id or name
#' @param fields If a term is search, fields can be specified to label and synonym. By default all fields are used.
#' 
#' 
searchDODO <- function(searchTerm,
                       database = NULL,
                       type = c(),
                       fields = c("label","synonym"),
                       exact = FALSE
){
  if(length(type) > 1){
    stop("Only one type can be provided")
  }else if (is.null(type)){
    stop("Please provide type (id or term)")
  }
  match.arg(type, c("id","term"), several.ok = FALSE)
  
  
  ##=======================================@
  ##=======================================@
  ## By ID ----
  if(type == "id"){
    if(is.null(database) & !all(grepl(":",searchTerm))){
      stop("Please provide either a database or use full ID (db:id)")
    }
    if(!is.null(database) & all(grepl(":", searchTerm))){
      match.arg(database, gsub(":.*","",searchTerm), several.ok = F)
    }
    
    ## Seed
    seed <- find_id(id = searchTerm)
    
    ## query to get name, definition, label, synonym, level
    cql <- c('MATCH (n:Concept)-[:is_known_as]->(s:Synonym)',
             sprintf('WHERE n.name IN $from %s',
                     ifelse(is.null(database), "", "AND n.database IN $database")),
             'RETURN DISTINCT n.name AS id, n.label AS label, n.database as database,', 
             'n.definition AS definition, ',
             's.value AS synonym, n.level AS level ORDER BY level DESC')
    toRet <- call_dodo(cypher,
                       prepCql(cql),
                       result = "row",
                       parameters = list(from = as.list(seed))) %>%
      dplyr::arrange(level)
    return(toRet)
  }
  
  ##=======================================@
  ##=======================================@
  ## By term ----
  
  if(type == "term"){
    fields <- match.arg(
      fields,
      c("synonym", "label"),
      several.ok=TRUE
    )
    input = searchTerm
    seed <- find_term(term = searchTerm, fields = fields)
    
    ## query to get name, definition, label, synonym, level
    cql <- c('MATCH (n:Concept)-[:is_known_as]->(s:Synonym)',
             sprintf('WHERE n.name IN $from %s',
                     ifelse(is.null(database), "", "AND n.database IN $database")),
             'RETURN DISTINCT n.name AS id, n.label AS label, n.database as database,', 
             'n.definition AS definition, ',
             's.value AS synonym, n.level AS level ORDER BY level DESC')
    toRet <- call_dodo(cypher,
                       prepCql(cql),
                       result = "row",
                       parameters = list(from = as.list(seed),
                                         database = as.list(database))) %>%
      dplyr::arrange(level)
    return(toRet)
  }
}


