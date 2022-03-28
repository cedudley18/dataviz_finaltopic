library(tidyverse)
coefs <- read_csv("data/coefs2017.csv")
model_df <- read_csv("data/model_subset.csv")

# fix the team names
coefs <-
  coefs %>%
  mutate(Team = substr(Team, 29, 40))

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(selectizeInput(inputId = "teamchoice",
                                label = "Choose a Team",
                                choices = factor(model_df$HomeTeam))),
    mainPanel(plotOutput(outputId = "majorplot"))
))

server <- function(input, output, session) {
  model_update <- reactive({
    model_df <-
      model_df %>%
      mutate(Teams = paste(HomeTeam, ",", AwayTeam)) %>%
      filter(str_detect(Teams, input$teamchoice)) %>%
      mutate(BoundaryProb = ifelse(HomeTeam == input$teamchoice,
                                   BoundaryProbHome2, BoundaryProbAway2))
})
  
  output$majorplot <- renderPlot({
    ggplot(data = model_update(), aes(x = DeadlineDays, y = BoundaryProb)) +
      geom_point() 
  })
  
  
}

shinyApp(ui, server)