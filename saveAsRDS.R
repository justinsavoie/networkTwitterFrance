library(stringr)
library(dplyr)
#library(twitteR)
#library(tm)
#library(ggplot2)
#library(quanteda)
#library(lubridate)
#library(scales)
library(streamR)
#library(RCurl)
#library(RJSONIO)
#library(ROAuth)
#library(network)
#library(ggnetwork)
#library(igraph)

setwd("/Users/justinsavoie/Dropbox/BoussolleFrancaise")

listOfSCrapes <- list.files() %>% .[
  
  (list.files() %>%
     str_detect("tweets") %>%
     which)]

saving <- function(x) {
  tweets <- parseTweets(x, simplify = TRUE)
  saveRDS(tweets, paste0("tweets",str_extract_all(x,"[0-9]+") %>% unlist() %>% as.numeric(),".rds"), compress="xz")
}

#lapply(whateverIwanttosave,saving)

