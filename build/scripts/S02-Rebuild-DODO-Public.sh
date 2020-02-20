R -e "Sys.setenv(PATH=paste(Sys.getenv('PATH'), '/usr/lib/rstudio-server/bin/pandoc', sep=':')); rmarkdown::render('Rebuild-DODO-Public.Rmd')";
