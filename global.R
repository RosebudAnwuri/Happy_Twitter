
#Load all modules and initialize connection with sqlite database
source("modules/overview.R")
source("modules/demographics.R")
source("modules/timeline.R")
#Sys.setenv(https_proxy = "")
library(dplyr) #for handling database connections
library(dbplyr)
library(RSQLite)
tweets_database=src_sqlite("Tweets",create = F)

#This creates a short text of 10 characters (or less) of the tweet which is used in the exploding box plots
short_text <- function(text){
  text=str_split(text,' ',simplify = T)
  if(length(text)<10){
    text%>%paste(collapse = ' ')%>%
      paste0('...')
  }
  else{
    text <- text[1:10]%>%paste(collapse = ' ')%>%
      paste0('...')
  }
  return(text)
}





