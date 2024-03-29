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

####################################
# NEXT DAY SCHEDULE
####################################


# scores <- get_master_schedule('2022-11-19')
# scores$away <- iconv(scores$away, from = "UTF-8", to = "ASCII//TRANSLIT")
# scores$home <- iconv(scores$home, from = "UTF-8", to = "ASCII//TRANSLIT")
# scores$away <- str_trim(scores$away, "left")
# scores$home <- str_trim(scores$home, "left")
# scores <- scores %>% filter(away %in% rankings$Team & home %in% rankings$Team)
# scores <- scores %>% filter(!is.na(game_id))
#
# hca <- rankings$pHCA[rankings$Team == home]
#
# for (i in 1:nrow(scores)){
#   scores[i,4] <- rankings$Rk[rankings$Team == scores[i,2]]
#   scores[i,5] <- rankings$Rk[rankings$Team == scores[i,3]]
#
#   scores[i,9] <- (1/(1+10**((scores[i,5]-scores[i,4])/400))) + hca
#   scores[i,10] <- 1 - scores[i,9]
#
#   if (scores[i,10] > scores[i,9]){
#     hspr <- (-128.07 * (scores[i,10]**2)) + (117.25 * scores[i,10]) - 28.482
#     hspr <- round(hspr, 1)
#     aspr <- -hspr
#   } else {
#     aspr <- (-128.07 * (scores[i,9]**2)) + (117.25 * scores[i,9]) - 28.482
#     aspr <- round(aspr, 1)
#     hspr <- -aspr
#   }
#
#   scores[i,11] <- aspr
#   scores[i,12] <- hspr
#
#   names(scores)[11] <- 'mod_away_spd'
#   names(scores)[12] <- 'mod_home_spd'
#
# }
#
# scores$away <- paste(scores$away_rank,scores$away)
# scores$home <- paste(scores$home_rank,scores$home)
#
# day_sched <- scores[c(2,3)]
# names(day_sched) <- c('Away', 'Home')
# day_sched$Away <- iconv(day_sched$Away, from = "UTF-8", to = "ASCII//TRANSLIT")
# day_sched$Home <- iconv(day_sched$Home, from = "UTF-8", to = "ASCII//TRANSLIT")
# day_sched$Away <- str_trim(day_sched$Away, "left")
# day_sched$Home <- str_trim(day_sched$Home, "left")
#
# write_csv(day_sched, 'day_sched.csv')
#
# lines <- scores[c(2,3,11,12)]
#
# write_csv(lines, 'day_lines.csv')



######################
# Elo Update #
######################

scores <- get_master_schedule('2022-11-19')
scores$away <- iconv(scores$away, from = "UTF-8", to = "ASCII//TRANSLIT")
scores$home <- iconv(scores$home, from = "UTF-8", to = "ASCII//TRANSLIT")
scores$away <- str_trim(scores$away, "left")
scores$home <- str_trim(scores$home, "left")
scores <- scores %>% filter(away %in% rankings$Team & home %in% rankings$Team)
scores <- scores %>% filter(!is.na(game_id))
scores$note <- NA

#scores[c(2,3,26,62,67,72),9] <- 'Neutral' 11/11
#scores[c(20,20),9] <- 'Neutral' 11/13
#scores[39,9] <- 'Neutral' 11/14
#scores[c(22,34,35),9] <- 'Neutral' 11/15
#scores[c(4,27),9] <- 'Neutral' 11/16
#scores[c(5,6,7,8,23,25,44,48),9] <- 'Neutral' 11/17
#scores[c(1,4,8,10,11,12,13,14,16,17,18,20,22,25,27,39,40,41,50,52,54,57,62,68,69),9] <- 'Neutral' 11/18
#scores[c(2,5,6,18,24,32,37,45),9] <- 'Neutral' 11/19

for (i in 1:nrow(scores)){
  away <- scores[i,2]
  home <- scores[i,3]

  rankings$One[rankings$Team == away] <- rankings$Two[rankings$Team == away]
  rankings$One[rankings$Team == home] <- rankings$Two[rankings$Team == home]

  rankings$Two[rankings$Team == away] <- rankings$Three[rankings$Team == away]
  rankings$Two[rankings$Team == home] <- rankings$Three[rankings$Team == home]

  rankings$Three[rankings$Team == away] <- rankings$Elo[rankings$Team == away]
  rankings$Three[rankings$Team == home] <- rankings$Elo[rankings$Team == home]

  ptsAway <- scores[i,6]
  ptsHome <- scores[i,7]
  rAway <- rankings$Elo[rankings$Team == away]
  rHome <- rankings$Elo[rankings$Team == home]

  hca <- rankings$pHCA[rankings$Team == home]

  if (!is.na(scores[i,9])){
    scores[i,10] <- (1/(1+10**((rHome-rAway)/400)))
    scores[i,11] <- 1 - scores[i,10]
  } else {
    scores[i,10] <- (1/(1+10**((rHome-rAway)/400))) - hca
    scores[i,11] <- 1 - scores[i,10]
  }

  if (scores[i,10] < 0) {
    scores[i,10] <- 0.01
    scores[i,11] <- 0.99
  }


  names(scores)[10] <- 'away_prob'
  names(scores)[11] <- 'home_prob'

  if (ptsAway > ptsHome){
    rankings$Win[rankings$Team == away] <- rankings$Win[rankings$Team == away] + 1
    rankings$Loss[rankings$Team == home] <- rankings$Loss[rankings$Team == home] + 1

    rankings$Elo[rankings$Team == away] <- round(rAway + 32*(1 - scores[i,10]),0)
    rankings$Elo[rankings$Team == home] <- round(rHome + 32*(0 - scores[i,11]),0)
  } else {
    rankings$Win[rankings$Team == home] <- rankings$Win[rankings$Team == home] + 1
    rankings$Loss[rankings$Team == away] <- rankings$Loss[rankings$Team == away] + 1

    rankings$Elo[rankings$Team == away] <- round(rAway + 32*(0 - scores[i,10]),0)
    rankings$Elo[rankings$Team == home] <- round(rHome + 32*(1 - scores[i,11]),0)
  }
}


rankings$Rec <- paste0(rankings$Win, '-', rankings$Loss)
rankings <- rankings %>% arrange(desc(Elo))
rankings$Rk <- c(1:363)
rankings$Last.3 <- (rankings$Elo - rankings$Three) + (rankings$Three - rankings$Two) + (rankings$Two - rankings$One)

write_csv(rankings, 'cbb_rankings_ncaahoopr.csv')


####################################################################################################################


###########
# HCA Calcs
###########
# hca <- read.csv('home_court.csv')

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
# NEXT DAY SCHEDULE
####################################


# scores <- get_master_schedule('2022-11-19')
# scores$away <- iconv(scores$away, from = "UTF-8", to = "ASCII//TRANSLIT")
# scores$home <- iconv(scores$home, from = "UTF-8", to = "ASCII//TRANSLIT")
# scores$away <- str_trim(scores$away, "left")
# scores$home <- str_trim(scores$home, "left")
# scores <- scores %>% filter(away %in% rankings$Team & home %in% rankings$Team)
# scores <- scores %>% filter(!is.na(game_id))
#
# hca <- rankings$pHCA[rankings$Team == home]
#
# for (i in 1:nrow(scores)){
#   scores[i,4] <- rankings$Rk[rankings$Team == scores[i,2]]
#   scores[i,5] <- rankings$Rk[rankings$Team == scores[i,3]]
#
#   scores[i,9] <- (1/(1+10**((scores[i,5]-scores[i,4])/400))) + hca
#   scores[i,10] <- 1 - scores[i,9]
#
#   if (scores[i,10] > scores[i,9]){
#     hspr <- (-128.07 * (scores[i,10]**2)) + (117.25 * scores[i,10]) - 28.482
#     hspr <- round(hspr, 1)
#     aspr <- -hspr
#   } else {
#     aspr <- (-128.07 * (scores[i,9]**2)) + (117.25 * scores[i,9]) - 28.482
#     aspr <- round(aspr, 1)
#     hspr <- -aspr
#   }
#
#   scores[i,11] <- aspr
#   scores[i,12] <- hspr
#
#   names(scores)[11] <- 'mod_away_spd'
#   names(scores)[12] <- 'mod_home_spd'
#
# }
#
# scores$away <- paste(scores$away_rank,scores$away)
# scores$home <- paste(scores$home_rank,scores$home)
#
# day_sched <- scores[c(2,3)]
# names(day_sched) <- c('Away', 'Home')
# day_sched$Away <- iconv(day_sched$Away, from = "UTF-8", to = "ASCII//TRANSLIT")
# day_sched$Home <- iconv(day_sched$Home, from = "UTF-8", to = "ASCII//TRANSLIT")
# day_sched$Away <- str_trim(day_sched$Away, "left")
# day_sched$Home <- str_trim(day_sched$Home, "left")
#
# write_csv(day_sched, 'day_sched.csv')
#
# lines <- scores[c(2,3,11,12)]
#
# write_csv(lines, 'day_lines.csv')


####################################
# ADD BETTING LINES AND ABREVIATIONS
####################################

# scores <- get_master_schedule('2022-11-18')
# scores$away <- iconv(scores$away, from = "UTF-8", to = "ASCII//TRANSLIT")
# scores$home <- iconv(scores$home, from = "UTF-8", to = "ASCII//TRANSLIT")
# scores$away <- str_trim(scores$away, "left")
# scores$home <- str_trim(scores$home, "left")
# scores <- scores %>% filter(away %in% rankings$Team & home %in% rankings$Team)
# scores <- scores %>% filter(!is.na(game_id))
# scores$note <- NA

#scores[48,] <- c('401483454', 'Colorado', 'Umass', 'NA', "NA",'63','66', '2022-11-17','NA')
#scores[c(2,3,26,62,67,72),9] <- 'Neutral' 11/11
#scores[c(20,20),9] <- 'Neutral' 11/13
#scores[39,9] <- 'Neutral' 11/14
#scores[c(22,34,35),9] <- 'Neutral' 11/15
#scores[c(4,27),9] <- 'Neutral' 11/16
#scores[c(5,6,7,8,23,25,44,48),9] <- 'Neutral' 11/17
#scores[c(1,4,8,10,11,12,13,14,16,17,18,20,22,25,27,39,40,41,50,52,54,57,62,68,69),9] <- 'Neutral' 11/18



# ids <- as.list(scores$game_id)
# lines <- list()
# away_abv_list <- list()
# home_abv_list <- list()



# for (i in ids){
#  url <- paste0('https://www.espn.com/mens-college-basketball/game/_/gameId/', i)
#  webpage <- read_html(url)
#  pagetext <- rvest::html_text(webpage)
#
#  #Get Abreviations
#  abv <- unlist(strsplit(pagetext, "Rebounds"))[[4]][1]
#  abv <- unlist(strsplit(abv, "Full"))[1]
#  abv <- gsub('[0-9]+', ' ', abv)
#  away_abv <- unlist(strsplit(abv, " "))[1]
#  away_abv_list <- append(away_abv_list, away_abv)
#  home_abv <- unlist(strsplit(abv, " "))[2]
#  home_abv_list <- append(home_abv_list, home_abv)
#
#  # if (i == 401483444){
#  #   odds == 'CAL -1.5'
#  # } else {
#  #   odds <- unlist(strsplit(pagetext, "Line: "))[[2]][1]
#  #   odds <- unlist(strsplit(odds, "\n"))[1]
#  # }
#
#  odds <- unlist(strsplit(pagetext, "Line: "))[[2]][1]
#  odds <- unlist(strsplit(odds, "\n"))[1]
#
#  lines <- append(lines, odds)
# }
#
#
# # lines[[40]] <- 'CAL -1.5'
# # lines <- append(lines, "UCSD -2.0")
# # away_abv_list <- append(away_abv_list, 'SAC')
# # home_abv_list <- append(home_abv_list, 'UCSD')
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
# scores$lines[scores$lines == 'EVEN'] <- paste(scores$home_abv[scores$lines == 'EVEN'], '-0.1')
#
#
# scores <- scores %>% separate(lines, c("fav_abv", "fav_spd"), " ")
#
# scores$und_abv[scores$fav_abv == scores$home_abv] <- scores$away_abv[scores$fav_abv == scores$home_abv]
# scores$und_abv[scores$fav_abv == scores$away_abv] <- scores$home_abv[scores$fav_abv == scores$away_abv]
#
# scores$fav_score[scores$fav_abv == scores$home_abv] <- scores$home_score[scores$fav_abv == scores$home_abv]
# scores$fav_score[scores$fav_abv == scores$away_abv] <- scores$away_score[scores$fav_abv == scores$away_abv]
#
# scores$und_score[scores$fav_abv == scores$home_abv] <- scores$away_score[scores$fav_abv == scores$home_abv]
# scores$und_score[scores$fav_abv == scores$away_abv] <- scores$home_score[scores$fav_abv == scores$away_abv]
#
# scores$away_spd[scores$away_abv == scores$fav_abv] <- scores$fav_spd[scores$away_abv == scores$fav_abv]
# scores$home_spd[scores$home_abv == scores$fav_abv] <- scores$fav_spd[scores$home_abv == scores$fav_abv]
#
# scores$away_spd <- as.numeric(scores$away_spd)
# scores$home_spd <- as.numeric(scores$home_spd)
# scores$fav_spd <- as.numeric(scores$fav_spd)
#
#
# scores$away_spd[is.na(scores$away_spd)] <- (scores$home_spd[is.na(scores$away_spd)] * -1)
# scores$home_spd[is.na(scores$home_spd)] <- (scores$away_spd[is.na(scores$home_spd)] * -1)
#
# scores <- scores[c(1,10,2,3,4,5,13,11,16,17,12,8,9,15,14)]


######################
# Elo Update #
######################

# for (i in 1:nrow(scores)){
#  away <- scores[i,2]
#  home <- scores[i,3]
#
#  rankings$One[rankings$Team == away] <- rankings$Two[rankings$Team == away]
#  rankings$One[rankings$Team == home] <- rankings$Two[rankings$Team == home]
#
#  rankings$Two[rankings$Team == away] <- rankings$Three[rankings$Team == away]
#  rankings$Two[rankings$Team == home] <- rankings$Three[rankings$Team == home]
#
#  rankings$Three[rankings$Team == away] <- rankings$Elo[rankings$Team == away]
#  rankings$Three[rankings$Team == home] <- rankings$Elo[rankings$Team == home]
#
#  ptsAway <- scores[i,6]
#  ptsHome <- scores[i,7]
#  rAway <- rankings$Elo[rankings$Team == away]
#  rHome <- rankings$Elo[rankings$Team == home]
#
#  hca <- rankings$pHCA[rankings$Team == home]
#
#  if (!is.na(scores[i,9])){
#  scores[i,10] <- (1/(1+10**((rHome-rAway)/400)))
#  scores[i,11] <- 1 - scores[i,10]
#  } else {
#  scores[i,10] <- (1/(1+10**((rHome-rAway)/400))) - hca
#  scores[i,11] <- 1 - scores[i,10]
#  }
#
#  if (scores[i,10] < 0) {
#    scores[i,10] <- 0.01
#    scores[i,11] <- 0.99
#  }
#
#
#  names(scores)[10] <- 'away_prob'
#  names(scores)[11] <- 'home_prob'
#
#  if (ptsAway > ptsHome){
#    rankings$Win[rankings$Team == away] <- rankings$Win[rankings$Team == away] + 1
#    rankings$Loss[rankings$Team == home] <- rankings$Loss[rankings$Team == home] + 1
#
#    rankings$Elo[rankings$Team == away] <- round(rAway + 32*(1 - scores[i,10]),0)
#    rankings$Elo[rankings$Team == home] <- round(rHome + 32*(0 - scores[i,11]),0)
#  } else {
#    rankings$Win[rankings$Team == home] <- rankings$Win[rankings$Team == home] + 1
#    rankings$Loss[rankings$Team == away] <- rankings$Loss[rankings$Team == away] + 1
#
#    rankings$Elo[rankings$Team == away] <- round(rAway + 32*(0 - scores[i,10]),0)
#    rankings$Elo[rankings$Team == home] <- round(rHome + 32*(1 - scores[i,11]),0)
#  }


  ####################
  # Betting Line Calcs
  ####################

 # if (scores[i,17] > scores[i,16]){
 #   hspr <- (-128.07 * (scores[i,17]**2)) + (117.25 * scores[i,17]) - 28.482
 #   hspr <- round(hspr, 1)
 #   aspr <- -hspr
 # } else {
 #   aspr <- (-128.07 * (scores[i,16]**2)) + (117.25 * scores[i,16]) - 28.482
 #   aspr <- round(aspr, 1)
 #   hspr <- -aspr
 # }
 #
 # if (hspr == -0 || aspr == -0){
 #   hspr <- 0
 #   aspr <- 0
 # }
 #
 # scores[i,18] <- aspr
 # scores[i,19] <- hspr
 #
 # names(scores)[18] <- 'mod_away_spd'
 # names(scores)[19] <- 'mod_home_spd'
 #
 #
 # if (scores[i,18] < scores[i,9]) {
 #   scores[i,20] <- scores[i,5]
 #   scores[i,21] <- scores[i,9]
 # } else {
 #   scores[i,20] <- scores[i,6]
 #   scores[i,21] <- scores[i,10]
 # }
 #
 # names(scores)[20] <- 'pick_abv'
 # names(scores)[21] <- 'pick_spd'
 #
 # if (scores[i,21] > 0) {
 #   scores[i,22] <- scores$und_score[scores$und_abv == scores[i,20]]
 #   scores[i,23] <- scores$fav_abv[scores$und_abv == scores[i,20]]
 #   scores[i,24] <- scores$fav_score[scores$fav_abv == scores[i,23]]
 # } else {
 #   scores[i,22] <- scores$fav_score[scores$fav_abv == scores[i,20]]
 #   scores[i,23] <- scores$und_abv[scores$fav_abv == scores[i,20]]
 #   scores[i,24] <- scores$und_score[scores$und_abv == scores[i,23]]
 # }
 #
 # names(scores)[22] <- 'pick_score'
 # names(scores)[23] <- 'opp_abv'
 # names(scores)[24] <- 'opp_score'
 #
 # scores$act_spd <- scores$opp_score - scores$pick_score
 #
 # scores$ATS.hit <- scores$act_spd < scores$pick_spd
 #
 # if (scores[i,26] == TRUE){
 #   rankings$ATS.W[rankings$Abv == scores[i,20]] <- rankings$ATS.W[rankings$Abv == scores[i,20]] + 1
 #   rankings$Units[rankings$Abv == scores[i,20]] <- rankings$Units[rankings$Abv == scores[i,20]] + 0.91
 # } else {
 #   rankings$ATS.L[rankings$Abv == scores[i,20]] <- rankings$ATS.L[rankings$Abv == scores[i,20]] + 1
 #   rankings$Units[rankings$Abv == scores[i,20]] <- rankings$Units[rankings$Abv == scores[i,20]] - 1
 # }
 #
 # if (scores[i,20] %in% scores$home_abv){
 #   scores[i,27] <- scores[i,19]
 # } else {
 #   scores[i,27] <- scores[i,18]
 # }
 #
 # names(scores)[27] <- 'mod_pick_spd'

# }


# rankings$Rec <- paste0(rankings$Win, '-', rankings$Loss)
# rankings <- rankings %>% arrange(desc(Elo))
# rankings$Rk <- c(1:363)
# rankings$Last.3 <- (rankings$Elo - rankings$Three) + (rankings$Three - rankings$Two) + (rankings$Two - rankings$One)

#write_csv(rankings, 'cbb_rankings_ncaahoopr.csv')


######################
# RESULTS
######################

# master_results <- read.csv('results.csv')
# master_results$date <- as.Date(master_results$date)
#
# results <- scores
#
# results$mod_diff <- abs(results$mod_pick_spd - results$pick_spd)
#
# results <- results[c(2,1,20,21,28,26)]
#
# master_results <- rbind(master_results, results)

# write_csv(master_results, 'results.csv')
