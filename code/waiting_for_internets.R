#!/usr/bin/env Rscript

internet_out <- function(){
  while(is.null(curl::nslookup("r-project.org", error=FALSE))) {
    Sys.sleep(30)
  }
  
  system("say internet is back")
}

internet_out()
