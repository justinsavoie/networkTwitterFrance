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

setwd("/Users/justinsavoie/Dropbox/BoussolleFrancaise")

#consumerKey <- "..."
#consumerSecret <- "..."
#access_token <- "..."
#access_secret <- "..."
#
#
#requestURL <- "https://api.twitter.com/oauth/request_token"
#accessURL <- "https://api.twitter.com/oauth/access_token"
#authURL <- "https://api.twitter.com/oauth/authorize"
#
#
#my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
#                             consumerSecret = consumerSecret,
#                             requestURL = requestURL,
#                             accessURL = accessURL,
#                             authURL = authURL)
#
#my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#
#
#
#save(my_oauth, file = "my_oauth.Rdata")

################STARTHERE######################
library(streamR)
load("my_oauth.Rdata")

# why a while loop: sorry I never learned error management -_-
while ( 1 < 2){
  filterStream(file.name = "tweets6.json", # Save tweets in a json file
               track = c("#presidentielle2017",
                         "#presidentielle",
                         "fillon2017",
                         "macron2017",
                         "#jlm2017",
                         "#hamon2017",
                         "#lepen2017",
                         "#fillon",
                         "#macron",
                         "#jlm",
                         "#hamon",
                         "#lepen"),
               language = "fr",
               timeout = 0, # Keep connection alive for 60 seconds
               oauth = my_oauth) # Use my_oauth file as the OAuth credentials
  
}


