library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(dplyr)
library(tidyr)
library(plyr)
library(shinyBS)

##################
# USER INTERFACE #
##################

rankings <- read.csv('cbb_rankings.csv')
rankings_disp <- rankings[1:9]

teams <- select(rankings,Team)
teams <- teams %>% arrange(teams)

conf <- aggregate(rankings$Elo, by=list(Name=rankings$Conf), FUN=mean)
colnames(conf)[2] <- 'avgElo'
conf$avgElo <- round(conf$avgElo, 0)
conf <- conf %>% arrange(desc(avgElo))
conf <- cbind(Rk = 1:32, conf)


ui <- fluidPage(
  
  titlePanel(title = 'CBB-ELO'),
  sidebarLayout(
    sidebarPanel(
      h3('Game Simulation'),
      br(),
      selectInput('home_team','Home Team', choices = teams),
      selectInput('away_team','Away Team', choices = teams),
      br(),
      checkboxInput('neutral', 'Neutral Site', FALSE),
      actionButton('submit', 'Submit'),
      br(),
      br(),
      h3('Conferences'),
      br(),
      tableOutput('conferences')
    ),
    mainPanel(
      DTOutput('rankings'),
      bsModal('hth','Game Simulation','submit', size='large', tableOutput('modal_tbl'))
    )
  )
)



##########
# SERVER #
##########


server <- function(input, output, session){
  
  output$rankings <- renderDT(
    rankings_disp,
    options = list(pageLength = 400),
    rownames = FALSE,
    selection = "single",
  )
  
  output$conferences <- renderTable(
    conf
  )
  
  
  head_to_head = reactive({
    
    rh <- rankings$Elo[rankings$Team == input$home_team]
    ra <- rankings$Elo[rankings$Team == input$away_team]
    
    hca <- rankings$pHCA[rankings$Team == input$home_team]
    
    ph <- (1/(1+10**((ra-rh)/400))) + hca
    ph <- round(ph,2) 
    pa <- 1 - ph
    
    hml <- 1/ph
    hml <- round(hml,2)
    aml <- 1/pa
    aml <- round(aml,2)
    
    if (ph > pa){
      hspr <- (-32.891 * ph)  + 17.018
      hspr <- round(hspr, 1)
      aspr <- -hspr
    } else {
      aspr <- (-32.891 * pa)  + 17.018
      aspr <- round(aspr, 0)
      hspr <- -aspr
    }
    
    if (hspr == -0 || aspr == -0){
      hspr <- 0
      aspr <- 0
    }
    
    ph <- ph * 100
    pa <- 100 - ph
    
    head_to_head <- data.frame(Home = c(input$home_team, rh, rankings$Rec[rankings$Team == input$home_team], ph, hml, hspr),
                            Cat = c('Team','Elo', 'Rec', 'xWin %', 'Fair Dec Odds', 'Fair Spread'),
                            Away = c(input$away_team, ra, rankings$Rec[rankings$Team == input$away_team], pa, aml, aspr)
    )
  })
  
   output$modal_tbl <- renderTable(
    head_to_head()
  )
   
  
}


shinyApp(ui, server)