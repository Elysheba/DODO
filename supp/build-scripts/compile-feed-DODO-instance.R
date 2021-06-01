# Compile markdown report for building DODO 

## Arguments from command line
library(DODO)
args <- commandArgs(trailingOnly=T) 

## checks
stopifnot(length(args) <= 2)
  file = here::here(paste0('supp/build-scripts/feed-DODO-instance.Rmd'))
  message("###################################")
  message("Compiling ", print(file))
  fn = paste("building",args[2], args[1], "report", sep = "-")
  
  ## Compile markdown
  rmarkdown::render(input = file, 
                    params = list(type = args[1],
                                  name = args[2]),
                    output_file = fn)     
  
  ## Comparison
  file = here::here(paste0('supp/build-scripts/compare-DODO-instance.Rmd'))
  message("###################################")
  message("Compiling ", print(file))
  fn = paste("compare",args[2], args[1], "report", sep = "-")
  rmarkdown::render(input = file, 
                    params = list(type = args[1],
                                  name = args[2]),
                    output_file = fn)   


