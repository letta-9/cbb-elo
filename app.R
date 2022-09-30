library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(dplyr)
library(tidyr)

rankings <- read.csv('cbb_rankings.csv')
teams <- select(rankings,Team)
teams <- teams %>% arrange(teams)

ui <- dashboardPage(
  
  dashboardHeader(
    title = 'CBB-ELO'
    ),
  
  dashboardSidebar(
    img(src='Letta.png', align = 'center', height="97%", width="97%"),
    selectInput('home team','Home Team', choices = teams),
    selectInput('away team','Away Team', choices = teams),
    br(),
    actionButton('submit', 'Submit')
  ),
  
  dashboardBody(
    DTOutput('rankings')
  )
)

server <- function(input, output, session){
  
  output$rankings <- renderDT(
    rankings,
    options = list(pageLength = 400),
    rownames = FALSE,
    selection = "single",
  )
  
}

shinyApp(ui, server)