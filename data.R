library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(dplyr)
library(tidyr)
library(plyr)
library(rvest)
library(ncaahoopR)
library(stringr)

rankings <- read.csv('cbb_rankings_ncaahoopr.csv')
# hca <- read.csv('home_court.csv')
# sched <- read.csv('schedule.csv')

###########
# HCA Calcs
###########

# rankings <- merge(rankings, hca, by = 'Team')
# rankings$HCA <- as.numeric(rankings$HCA)
# rankings$pHCA <- .50 - (((rankings$HCA / 2) - 17.018) / -32.891)
# rankings <- rankings[c(2,1,3,4,5,6,7,8,9,10,11)]
# rankings <- rankings %>% arrange(Rk)

# conf <- aggregate(rankings$Elo, by=list(Conf=rankings$Conf), FUN=mean)
# colnames(conf)[2] <- 'avgElo'
# conf$avgElo <- round(conf$avgElo, 0)
# conf <- conf %>% arrange(desc(avgElo))
# conf <- cbind(Rk = 1:33, conf)
# max_team <- rankings[order(rankings$Conf, -rankings$Elo),]
# min_team <- rankings[order(rankings$Conf, rankings$Elo),]
# max_team <- max_team[!duplicated(max_team$Conf),]
# min_team <- min_team[!duplicated(min_team$Conf),]
# max_team <- max_team[,c(2,3,5)]
# min_team <- min_team[,c(2,3,5)]
# 
# conf <- merge(conf, c(max_team,min_team), by='Conf')
# conf <- conf[,c(2,1,3,4,5,6,8)]
# colnames(conf) <- c('Rk','Conf','avgElo','maxTeam','maxElo','minTeam','minElo')
# conf <- conf %>% arrange(Rk)
# 
# conf_list <- unique(rankings[,3])
# conf_list <- data.frame(conf_list)
# conf_list <- conf_list %>% arrange(conf_list)
# conf_list$Conf_Rk <- c(1:nrow(conf_list))


####################################
# INPUT DATA
####################################


scores <- get_master_schedule('2022-11-11')
scores$away <- iconv(scores$away, from = "UTF-8", to = "ASCII//TRANSLIT")
scores$home <- iconv(scores$home, from = "UTF-8", to = "ASCII//TRANSLIT")
scores$away <- str_trim(scores$away, "left")
scores$home <- str_trim(scores$home, "left")
scores <- scores %>% filter(away %in% rankings$Team & home %in% rankings$Team)
scores <- scores %>% filter(!is.na(game_id))

####################################
# ADD BETTING LINES AND ABREVIATIONS
####################################

# ids <- as.list(scores$game_id)
# lines <- list()
# away_abv_list <- list()
# home_abv_list <- list()
# 
# for (i in ids){
#   url <- paste0('https://www.espn.com/mens-college-basketball/game/_/gameId/', i)
#   webpage <- read_html(url)
#   pagetext <- rvest::html_text(webpage)
#   
#   #Get Abreviations
#   abv <- unlist(strsplit(pagetext, "Rebounds"))[[4]][1]
#   abv <- unlist(strsplit(abv, "Full"))[1]
#   abv <- gsub('[0-9]+', ' ', abv)
#   away_abv <- unlist(strsplit(abv, " "))[1]
#   away_abv_list <- append(away_abv_list, away_abv)
#   home_abv <- unlist(strsplit(abv, " "))[2]
#   home_abv_list <- append(home_abv_list, home_abv)
#   
#   #Get Odds
#   odds <- unlist(strsplit(pagetext, "Line: "))[[2]][1]
#   odds <- unlist(strsplit(odds, "\n"))[1]
#   lines <- append(lines, odds)
# }
# 
# away_abv <- as.data.frame(away_abv_list)
# home_abv <- as.data.frame(home_abv_list)
# away_abv <- t(away_abv)
# home_abv <- t(home_abv)
# row.names(away_abv) <- NULL
# row.names(home_abv) <- NULL
# scores <- cbind(scores, away_abv)
# scores <- cbind(scores, home_abv)
# scores <- scores %>% relocate(away_abv, .before = away_rank)
# scores <- scores %>% relocate(home_abv, .before = away_rank)
# 
# 
# lines <- as.data.frame(lines)
# lines <- t(lines)
# row.names(lines) <- NULL
# scores <- cbind(scores, lines)
# 
# scores <- scores %>% separate(lines, c("fav_abv", "fav_spd"), " ")
# 
# 
# scores$away_spd[scores$away_abv == scores$fav_abv] <- scores$fav_spd[scores$away_abv == scores$fav_abv]
# scores$home_spd[scores$home_abv == scores$fav_abv] <- scores$fav_spd[scores$home_abv == scores$fav_abv]
# 
# scores$away_spd <- as.numeric(scores$away_spd)
# scores$home_spd <- as.numeric(scores$home_spd)
# 
# 
# scores$away_spd[is.na(scores$away_spd)] <- (scores$home_spd[is.na(scores$away_spd)] * -1)
# scores$home_spd[is.na(scores$home_spd)] <- (scores$away_spd[is.na(scores$home_spd)] * -1)



#####################
# Next Day Schedule
#####################

day_sched <- scores[c(2,3)]
names(day_sched) <- c('Away', 'Home')
day_sched$Away <- iconv(day_sched$Away, from = "UTF-8", to = "ASCII//TRANSLIT")
day_sched$Home <- iconv(day_sched$Home, from = "UTF-8", to = "ASCII//TRANSLIT")
day_sched$Away <- str_trim(day_sched$Away, "left")
day_sched$Home <- str_trim(day_sched$Home, "left")

write_csv(day_sched, 'day_sched.csv')


######################
# Elo Update #
######################
# for (i in 1:nrow(scores)){
#   away <- scores[i,2]
#   home <- scores[i,3]
#   
#   rankings$One[rankings$Team == away] <- rankings$Two[rankings$Team == away]
#   rankings$One[rankings$Team == home] <- rankings$Two[rankings$Team == home]
#   
#   rankings$Two[rankings$Team == away] <- rankings$Three[rankings$Team == away]
#   rankings$Two[rankings$Team == home] <- rankings$Three[rankings$Team == home]  
#   
#   rankings$Three[rankings$Team == away] <- rankings$Elo[rankings$Team == away]
#   rankings$Three[rankings$Team == home] <- rankings$Elo[rankings$Team == home]
#   
#   ptsAway <- scores[i,6]
#   ptsHome <- scores[i,7]
#   rAway <- rankings$Elo[rankings$Team == away]
#   rHome <- rankings$Elo[rankings$Team == home]
# 
#   hca <- rankings$pHCA[rankings$Team == home]
# 
#   scores[i,9] <- (1/(1+10**((rHome-rAway)/400))) + hca
#   scores[i,10] <- 1 - scores[i,9]
# 
#   pHome <- scores[i,10]
#   pAway <- 1 - pHome
# 
#   if (ptsAway > ptsHome){
#     rankings$Win[rankings$Team == away] = rankings$Win[rankings$Team == away] + 1
#     rankings$Loss[rankings$Team == home] = rankings$Loss[rankings$Team == home] + 1
#     
#     rankings$Elo[rankings$Team == away] <- round(rAway + 32*(1 - pAway),0)
#     rankings$Elo[rankings$Team == home] <- round(rHome + 32*(0 - pHome),0)
#   } else {
#     rankings$Win[rankings$Team == home] = rankings$Win[rankings$Team == home] + 1
#     rankings$Loss[rankings$Team == away] = rankings$Loss[rankings$Team == away] + 1
#     
#     rankings$Elo[rankings$Team == away] <- round(rAway + 32*(0 - pAway),0)
#     rankings$Elo[rankings$Team == home] <- round(rHome + 32*(1 - pHome),0)
#   }
# }
# 
# rankings$Rec <- paste0(rankings$Win, '-', rankings$Loss)
# rankings <- rankings %>% arrange(desc(Elo))
# rankings$Rk <- c(1:363)
# rankings$Last.3 <- (rankings$Elo - rankings$Three) + (rankings$Three - rankings$Two) + (rankings$Two - rankings$One)



