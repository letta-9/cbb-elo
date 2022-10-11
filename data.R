library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(dplyr)
library(tidyr)
library(plyr)

rankings <- read.csv('pre_szn_22.csv')
hca <- read.csv('home_court.csv')
rankings <- merge(rankings, hca, by = 'Team')
rankings$HCA <- as.numeric(rankings$HCA)
rankings$pHCA <- .50 - (((rankings$HCA / 2) - 17.018) / -32.891)
rankings <- rankings[c(2,1,3,4,5,6,7,8,9,10,11)]
rankings <- rankings %>% arrange(Rk)

conf <- aggregate(rankings$Elo, by=list(Name=rankings$Conf), FUN=mean)
colnames(conf)[2] <- 'avgElo'
conf$avgElo <- round(conf$avgElo, 0)
conf <- conf %>% arrange(desc(avgElo))
conf <- cbind(Rk = 1:32, conf)

#write_csv(rankings, 'cbb_rankings.csv')



