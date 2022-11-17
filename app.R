library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(dplyr)
library(tidyr)
library(plyr)
library(shinyBS)
library(data.table)
library(recoder)

##################
# USER INTERFACE #
##################

rankings <- read.csv('cbb_rankings_ncaahoopr.csv')
rankings_disp <- rankings[c(1,2,3,4,5)]
day_sched <- read.csv('day_sched.csv') 


teams <- select(rankings,Team)
teams <- teams %>% arrange(teams)

conf <- aggregate(rankings$Elo, by=list(Conf=rankings$Conf), FUN=mean)
colnames(conf)[2] <- 'avgElo'
conf$avgElo <- round(conf$avgElo, 0)
conf <- conf %>% arrange(desc(avgElo))
conf <- cbind(Rk = 1:33, conf)
max_team <- rankings[order(rankings$Conf, -rankings$Elo),]
min_team <- rankings[order(rankings$Conf, rankings$Elo),]
max_team <- max_team[!duplicated(max_team$Conf),]
min_team <- min_team[!duplicated(min_team$Conf),]
max_team <- max_team[,c(2,3,5)]
min_team <- min_team[,c(2,3,5)]
conf <- merge(conf, c(max_team,min_team), by='Conf')
conf <- conf[,c(2,1,3)]
colnames(conf) <- c('Rk','Conf','avgElo')
conf <- conf %>% arrange(Rk)

next_games <- rankings[c(1,2,3,4,10)]

conf_list <- unique(rankings[,3])
conf_list <- data.frame(conf_list)
conf_list <- conf_list %>% arrange(conf_list)

top_hot <- rankings %>% arrange(desc(Last.3)) %>% slice(1:10)
top_hot <- top_hot[c(1,2,4,5,9)]

top_not <- rankings %>% arrange(Last.3) %>% slice(1:10)
top_not <- top_not[c(1,2,4,5,9)]



ui <- dashboardPage(
  

  
  dashboardHeader(title = 'CBB-ELO'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Rankings", tabName = "rank_tab", icon = icon("chart-simple")),
      menuItem("Game Simulation", tabName = "sim_tab", icon = icon("basketball")),
      menuItem("Projections", icon = icon("umbrella"), tabName = "proj_tab",
               badgeLabel = "Coming Soon", badgeColor = "red"),
      menuItem("Results", icon = icon("chart-line"), tabName = "res_tab",
               badgeLabel = "Coming Soon", badgeColor = "red")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "rank_tab",
              fluidRow(
                valueBoxOutput("first", width = 3),
                valueBoxOutput("last", width = 3),     
                valueBoxOutput("hot", width = 3),
                valueBoxOutput("not", width = 3)
              ),
              br(),
              br(),
              fluidRow(
                box(title = "More Tables",
                    width = 4, 
                    collapsible = T,
                    collapsed = T,
                    tabBox(id = 'conf_box',
                        width = 4,
                        tabPanel("Who's Hot", 
                                 tableOutput('top_hot')),
                        tabPanel("Who's Not", 
                                 tableOutput('top_not')),                       
                        tabPanel("Conf Summary", 
                                 tableOutput('conferences')),
                        tabPanel("By Conf",
                                 selectInput('conf_drop','Conference', conf_list),
                                 tableOutput('by_conf')),
                        ),
                ),
                box(title='All 363',
                    solidHeader = F,
                    width = 6, 
                    collapsible = F,
                    DTOutput('rankings'), style = "font-size: 75%;"),
              ),
      ),
      
      tabItem(tabName = "sim_tab",
              box(title = "Game Simulation",
                  solidHeader = F,
                  width = 4,
                  collapsible = F,
                  selectInput('away_team','Away Team', choices = teams),                  
                  selectInput('home_team','Home Team', choices = teams),
                  checkboxInput('neutral', 'Neutral Site', FALSE),
                  actionButton('submit', 'Submit')),
              box(title = "11/16 Games",
                  solidHeader = F,
                  width = 4,
                  collapsible = F,
                  DTOutput('next_games'), style = "font-size: 75%;",
              ),
              bsModal('hth','Game Simulation','submit', size='large',
                      column(4,htmlOutput('home_logo')),
                      column(4,tableOutput('modal_tbl')),
                      column(4,htmlOutput('away_logo'))
              )
      ),
      
      tabItem(tabName = "proj_tab",
              h2("March Madness Projections - Coming Soon")
      )
    )
  )
)




##########
# SERVER #
##########






server <- function(input, output, session){
  
  output$first <- renderValueBox({
    first <- rankings$Team[rankings$Elo == max(rankings$Elo)]
    first <- first[1]
    valueBox(paste0(first,' (',max(rankings$Elo),')'),
             "Highest Ranked (Elo)", icon = icon("crown"), color = "yellow")
  })
  
  output$last <- renderValueBox({
    last <- rankings$Team[rankings$Elo == min(rankings$Elo)]
    last <- last[1]
    valueBox(paste0(last,' (',min(rankings$Elo),')'),
             "Lowest Ranked (Elo)", icon = icon("poop"), color = "purple")
  })
  
  
  output$hot <- renderValueBox({
    hot <- rankings$Team[rankings$Last.3 == max(rankings$Last.3)]
    hot <- hot[1]
    valueBox(paste0(hot,' (+',rankings$Last.3[rankings$Team == hot],')'),
             "Who's Hot (Last 3)", icon = icon("fire"), color = "red")
  })
  
  output$not <- renderValueBox({
    not <- rankings$Team[rankings$Last.3 == min(rankings$Last.3)]
    not <- not[1]
    valueBox(paste0(not,' (',rankings$Last.3[rankings$Team == not],')'),
             "Who's Not (Last 3)", icon = icon("snowflake"), color = "aqua")
  })
  
  
  output$rankings <- renderDT(
    rankings_disp,
    options = list(pageLength = 400),
    rownames = FALSE,
    selection = "single",
  )
  
  output$top_hot <- renderTable(
    top_hot
  )  
  
  
  output$top_not <- renderTable(
    top_not
  )  
  
  output$conferences <- renderTable(
    conf
  )
  
  output$by_conf <- renderTable(
    rankings %>% filter(Conf == input$conf_drop) %>% select(Rk, Team, Rec, Elo),
    rankings$Conf_Rk <- c(1:nrow(rankings))
  )
  
  
  head_to_head = reactive({
    
    rh <- rankings$Elo[rankings$Team == input$home_team]
    ra <- rankings$Elo[rankings$Team == input$away_team]
    
    if (input$neutral == TRUE) {
      hca <- 0
    } else {
      hca <- rankings$pHCA[rankings$Team == input$home_team]  
    }
    
  
    ph <- (1/(1+10**((ra-rh)/400))) + hca
    ph <- round(ph,2) 
    pa <- 1 - ph 
    
    hml <- 1/ph
    hml <- round(hml,2)
    aml <- 1/pa
    aml <- round(aml,2)
    
    if (ph > pa){
      hspr <- (-128.07 * (ph**2)) + (117.25 * ph) - 28.482
      hspr <- round(hspr, 1)
      aspr <- -hspr
    } else {
      aspr <- (-128.07 * (pa**2)) + (117.25 * pa) - 28.482
      aspr <- round(aspr, 1)
      hspr <- -aspr
    }
    
    if (hspr == -0 || aspr == -0){
      hspr <- 0
      aspr <- 0
    }
    
    ph <- ph * 100
    
    if (ph > 100){
      ph <- 100
    }
    
    pa <- 100 - ph
    
    head_to_head <- data.frame(Home = c(input$home_team, rh, rankings$Rec[rankings$Team == input$home_team], ph, hml, hspr),
                               Cat = c('Team','Elo', 'Rec', 'xWin %', 'Fair Dec Odds', 'Fair Spread'),
                               Away = c(input$away_team, ra, rankings$Rec[rankings$Team == input$away_team], pa, aml, aspr)
    )
  })
  
  output$home_logo <- renderText({
    paste0('<img src ="',input$home_team,'.png"', ' height="200" width="200" ></img>')
    })
  
  output$modal_tbl <- renderTable(
    head_to_head()
  )
  
  output$away_logo <- renderText({
    paste0('<img src ="',input$away_team,'.png"', ' height="200" width="200" ></img>')
  })
  
  output$next_games <- renderDT(
    day_sched,
    options = list(pageLength = 400),
    rownames = FALSE,
    selection = "single",
  )
  
}

shinyApp(ui, server)