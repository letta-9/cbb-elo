library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(dplyr)
library(tidyr)
library(plyr)
library(rvest)
library(ncaahoopR)

# rankings <- read.csv('cbb_rankings.csv')
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


#############################
# Scrape Full Season Schedule
#############################


# conf <- c('aac', 'acc', 'america-east', 'atlantic-10', 'atlantic-sun', 'big-12', 'big-east', 'big-sky',
#           'big-south', 'big-ten', 'big-west', 'colonial', 'cusa', 'horizon', 'ivy', 'maac', 'mac',
#           'meac', 'mvc', 'mwc', 'northeast', 'ovc', 'pac-12', 'patriot', 'sec', 'southern', 'southland',
#           'summit', 'sun-belt', 'swac', 'wac', 'wcc')

# teams <- unique(rankings$Team)
# teams <- tolower(teams)
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = "(", replacement = "", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = ")", replacement = "", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = "'", replacement = "", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = " ", replacement = "-", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = ".", replacement = "", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = "&", replacement = "", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = "tcu", replacement = "texas-christian", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = "uab", replacement = "alabama-birmingham", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = "nc-state", replacement = "north-carolina-state", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = "unc-greensboro", replacement = "north-carolina-greensboro", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = "uc-santa-barbara", replacement = "california-santa-barbara", x = t, fixed = TRUE))
# teams <- lapply(X = teams, FUN = function(t) gsub(pattern = "louisiana", replacement = "louisiana-lafayette", x = t, fixed = TRUE))
# 
# 
# 
# sched <- data.frame()
# 
# 
# for (i in teams){
#   url <- paste0('https://www.sports-reference.com/cbb/schools/',i,'/2023-schedule.html')
#   page <- read_html(url)
#   team_sched <- page %>% html_nodes('table') %>% html_table() %>% .[[2]]
#   sched <- rbind(sched, team_sched)
# }
# 
# 
# write_csv(full_sched, 'schedule.csv')

#####################
# Clean Schedule Data
#####################

# names(sched) <- c('Date','Away','A.PTS','Home','H.PTS','OT','Notes')
# 
# sched$Date <- as.Date(sched$Date, '%a, %b %d, %Y')
# 
# sched <- sched %>% filter(Notes == '') %>%
#   filter(A.PTS != '') %>%
#   select(Date, Away, A.PTS, Home, H.PTS) %>%
#   arrange(Date)
# 
# sched$pAway <- 0
# sched$pHome <- 0
# 
# 
# rankings$Prev <- rankings$Elo
# 
# for (i in 1:nrow(sched)){
#   away <- sched[i,2]
#   home <- sched[i,4]
#   ptsAway <- sched[i,3]
#   ptsHome <- sched[i,5]
#   rAway <- rankings$Elo[rankings$Team == away]
#   rHome <- rankings$Elo[rankings$Team == home]
#   
#   hca <- rankings$pHCA[rankings$Team == home]
#   
#   sched[i,7] <- (1/(1+10**((rAway-rHome)/400))) + hca
#   sched[i,6] <- 1 - sched[i,7]
#   
#   pHome <- sched[i,7]
#   pAway <- 1 - pHome
#   
#   if (ptsAway > ptsHome){
#     rankings$Elo[rankings$Team == away] <- round(rAway + 32*(1 - pAway),0)
#     rankings$Elo[rankings$Team == home] <- round(rHome + 32*(0 - pHome),0)
#   } else {
#     rankings$Elo[rankings$Team == away] <- round(rAway + 32*(0 - pAway),0)
#     rankings$Elo[rankings$Team == home] <- round(rHome + 32*(1 - pHome),0)  
#   }
# }
# 
# rankings <- rankings %>% arrange(desc(Elo))
# rankings$Rk <- c(1:363)
# rankings$Change <- rankings$Elo - rankings$Prev


rankings <- read.csv('cbb_rankings_ncaahoopr.csv')

######################
# Elo Update #
######################

scores <- get_master_schedule('2022-11-07')
scores$away <- iconv(scores$away, from = "UTF-8", to = "ASCII//TRANSLIT")
scores$home <- iconv(scores$home, from = "UTF-8", to = "ASCII//TRANSLIT")
scores$away <- str_trim(scores$away, "left")
scores$home <- str_trim(scores$home, "left")
scores <- scores %>% filter(away %in% rankings$Team & home %in% rankings$Team)


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

  scores[i,9] <- (1/(1+10**((rHome-rAway)/400))) + hca
  scores[i,10] <- 1 - scores[i,9]

  pHome <- scores[i,10]
  pAway <- 1 - pHome

  if (ptsAway > ptsHome){
    rankings$Elo[rankings$Team == away] <- round(rAway + 32*(1 - pAway),0)
    rankings$Elo[rankings$Team == home] <- round(rHome + 32*(0 - pHome),0)
  } else {
    rankings$Elo[rankings$Team == away] <- round(rAway + 32*(0 - pAway),0)
    rankings$Elo[rankings$Team == home] <- round(rHome + 32*(1 - pHome),0)
  }
}

rankings <- rankings %>% arrange(desc(Elo))
rankings$Rk <- c(1:363)
rankings$Last.3 <- (rankings$Elo - rankings$Three) + (rankings$Three - rankings$Two) + (rankings$Two - rankings$One)



#####################
# Next Day Schedule
#####################

# day_sched <- get_master_schedule('2022-11-09')
# day_sched <- day_sched[c(2,3)]
# names(day_sched) <- c('Away', 'Home')
# day_sched$Away <- iconv(day_sched$Away, from = "UTF-8", to = "ASCII//TRANSLIT")
# day_sched$Home <- iconv(day_sched$Home, from = "UTF-8", to = "ASCII//TRANSLIT")
# day_sched$Away <- str_trim(day_sched$Away, "left")
# day_sched$Home <- str_trim(day_sched$Home, "left")
# 
# write_csv(day_sched, 'day_sched.csv')



