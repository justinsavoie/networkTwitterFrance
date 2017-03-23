############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
#https://rpubs.com/kateto/netviz
#http://sites.stat.psu.edu/~dhunter/Rnetworks/
#http://kateto.net/networks-r-igraph
###Doing The Thing
library(stringr)
library(dplyr)
library(twitteR)
library(tm)
library(ggplot2)
library(quanteda)
library(lubridate)
library(scales)
library(streamR)
library(RCurl)
library(RJSONIO)
library(ROAuth)
library(network)
library(ggnetwork)
library(igraph)
library(readr)

setwd("/Users/justinsavoie/Dropbox/BoussolleFrancaise")

listOfSCrapes <- list.files() %>% .[
  
  (list.files() %>%
     str_detect(".rds") %>%
     which)]


readRDSTweets <- function(x) {
  
  tweets.df <- readRDS(paste0(x))
  
  tweets.df$is.retweet <- ifelse(str_sub(tweets.df$text,1,2)=="RT",1,0)
  tweets.df <- tweets.df %>% filter(is.retweet == 1)
  hasbeenretweeted <- str_extract(tweets.df$text, "@(.*?):")
  hasretweeted <- tweets.df$screen_name
  hasbeenretweeted <- str_sub(hasbeenretweeted,2,-2)
  hasretweetedNames <- hasretweeted$names
  
  remove(edges)
  
  if (class(edges)[1] == "tbl_df"){
    edges <- bind_rows(edges, tibble::data_frame(hasretweeted, hasbeenretweeted, hasretweetedNames))
  }
  
  if (class(edges)[1] == "function"){
    edges <- tibble::data_frame(hasretweeted,hasbeenretweeted, hasretweetedNames)
  }
  
}



a <- readRDSTweets(listOfSCrapes)






edges <- edges %>%
  dplyr::filter(!is.na(hbr))

edges <- edges %>%
  dplyr::filter(!is.na(hr))

weightNodes <- edges %>%
  group_by(hbr) %>%
  summarise (n = n())

edges$n <- 1
edges <- aggregate(edges[,3], edges[,-3], sum)

weightEdges <- edges$x

dfEdges <- edges %>%
  dplyr::select(-x)

nodes <- unique(c(edges$hbr,edges$hr))

nodes <- data.frame(vertex.name = nodes, stringsAsFactors = FALSE)

dfNodes <- left_join(nodes, weightNodes, by = c("vertex.name" = "hbr"))

dfEdges$weight <- weightEdges


dfEdges <- dfEdges[,c(2,1,3)]

matchFile <- data.frame(vertex.name = dfNodes$vertex.name, id = 1:(length(dfNodes$vertex.name)), stringsAsFactors = FALSE)
matchFile$id <- as.character(matchFile$id)

dfNodes <- left_join(dfNodes,matchFile)
dfNodes <- dfNodes %>% dplyr::select(-vertex.name)

dfNodes$n[is.na(dfNodes$n)] <- 0.1

dfEdges <- left_join(dfEdges, matchFile, by = c("hr" = "vertex.name"))
dfEdges <- dfEdges %>% select(-hr)
names(dfEdges)[3] <- "hr"

dfEdges <- left_join(dfEdges, matchFile, by = c("hbr" = "vertex.name"))
dfEdges <- dfEdges %>% select(-hbr)
names(dfEdges)[3] <- "hbr"
dfEdges <- dfEdges[,c(2,3,1)]
dfNodes <- dfNodes[,c(2,1)]

dfNodes <- left_join(dfNodes, matchFile)
names(dfNodes) <- c("id", "weight", "Label")

dfNodes$id <- paste0("s", dfNodes$id)
dfEdges$hr <- paste0("s", dfEdges$hr)
dfEdges$hbr <- paste0("s", dfEdges$hbr)

##Getting them for gephi
dfEdges$type <- "Directed"
dfEdges <- dfEdges[,c(1,2,4,3)]

names(dfNodes) <- c("id", "weight", "Label")
names(dfEdges) <- c("Source", "Target", "Type", "weightEdges")


###Cluster
gg <- graph_from_data_frame(dfEdges, directed=TRUE, vertices = dfNodes)
wc <- cluster_walktrap(gg)

dfNodes$group <- wc$membership
###Filter (only some) to make it less heavy
dfNodes <- (dfNodes) %>% dplyr::filter(weight != 0.1)
dfNodes$group[!(dfNodes$group %in% (as.numeric(names(tail(sort(table(dfNodes$group)))))))] <- 99
dfNodes$Label <- ifelse(dfNodes$weight > quantile(dfNodes$weight,0.98), dfNodes$Label, "")


write.csv(dfEdges, file = "dfEdges.csv", row.names = FALSE)
write.csv(dfNodes, file = "dfNodes.csv", row.names = FALSE)
