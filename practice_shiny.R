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
                                choices = factor(model_df$HomeTeam),
                                selected = "Buffalo")),
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
  
  coef_update <- reactive({
    coefs <-
    coefs %>%
      filter(Team == input$teamchoice)
  })
  
  output$majorplot <- renderPlot({
    ggplot(data = model_update(), aes(x = DeadlineDays, y = BoundaryProb)) +
      geom_point() +
      geom_segment(aes(x = -150, 
                       y = coef_update()$main_intercept +
                         coef_update()$intercept + 
                         (-150 * (coef_update()$DeadlineDays + coef_update()$deadline_days)),
                       xend = 0, yend = coef_update()$main_intercept +
                         coef_update()$intercept)) +
      geom_segment(aes(x = -0, 
                       y = coef_update()$main_intercept +
                         coef_update()$intercept + 
                         coef_update()$DeadlineInd +
                         coef_update()$deadline_indicator, 
                       xend = 50, 
                       yend = coef_update()$predictedend
                       )) +
      theme_classic() +
      labs(x = "Days Relative to the Trade Deadline",
           y =  "Vegas's Predicted Probability of Winning",
           title = input$teamchoice)
      
  })
  
  
}

shinyApp(ui, server)