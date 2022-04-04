library(tidyverse)

# putting together all of the seasons
coefs <- read_csv("data/coefs2017.csv")
model_df <- read_csv("data/model_subset.csv")

coefs16 <- read_csv("data/coefs2016.csv")
model_df16 <- read_csv("data/model_subset16.csv")

coefs18 <- read_csv("data/coefs2018.csv")
model_df18 <- read_csv("data/model_subset18.csv")

coefs <-
  coefs %>%
  mutate(Season = "2017-18")
coefs16 <-
  coefs16 %>%
  mutate(Season = "2016-17")
coefs18 <-
  coefs18 %>%
  mutate(Season = "2018-19")
model_df <-
  model_df %>%
  mutate(Season = "2017-18")
model_df16 <-
  model_df16 %>%
  mutate(Season = "2016-17")
model_df18 <-
  model_df18 %>%
  mutate(Season = "2018-19")

total_coefs <-
  rbind(coefs16, coefs, coefs18)
total_modeldf <-
 rbind(model_df16, model_df, model_df18)

# fix the team names
total_coefs <-
  total_coefs %>%
  mutate(Team = substr(Team, 29, 40))

# App

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(selectizeInput(inputId = "teamchoice",
                                label = "Choose a Team",
                                choices = as.character(total_modeldf$HomeTeam),
                                selected = "Boston"),
                 radioButtons(inputId = "yearselect",
                              label = "Choose a Season",
                              choices = levels(factor(total_coefs$Season)))),
    mainPanel(plotOutput(outputId = "majorplot"),
              tableOutput(outputId = "table1"),
              tableOutput(outputId = "table2"),
              tableOutput(outputId = "table3"),
              tableOutput(outputId = "table4"))
))

server <- function(input, output, session) {
  model_update <- reactive({
    total_modeldf <-
      total_modeldf %>%
      filter(Season == input$yearselect) %>%
      mutate(Teams = paste(HomeTeam, ",", AwayTeam)) %>%
      filter(str_detect(Teams, input$teamchoice)) %>%
      mutate(BoundaryProb = ifelse(HomeTeam == input$teamchoice,
                                   BoundaryProbHome2, BoundaryProbAway2))
})
  
  coef_update <- reactive({
    total_coefs <-
    total_coefs %>%
      filter(Season == input$yearselect,
        Team == input$teamchoice)
  })
  
  coef_order1 <- reactive({
    total_coefs <-
      total_coefs %>%
      filter(Season == input$yearselect) %>%
      slice_max(diffat0, n = 5) %>%
      select(Team, diffat0)
  })
  
  coef_order2 <- reactive({
    total_coefs <-
      total_coefs %>%
      filter(Season == input$yearselect) %>%
      slice_min(diffat0, n = 5) %>%
      select(Team, diffat0)
  })
  
  coef_order3 <- reactive({
    total_coefs <-
      total_coefs %>%
      filter(Season == input$yearselect) %>%
      slice_max(diffat40, n = 5) %>%
      select(Team, diffat40)
  })
  
  coef_order4 <- reactive({
    total_coefs <-
      total_coefs %>%
      filter(Season == input$yearselect) %>%
      slice_min(diffat40, n = 5) %>%
      select(Team, diffat40)
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
  
  output$table1 <- renderTable({coef_order1()
    
  })
  
  output$table2 <- renderTable({coef_order2()
    
  })
  
  output$table3 <- renderTable({coef_order3()
    
  })
  
  output$table4 <- renderTable({coef_order4()
    
  })
  
}

shinyApp(ui, server)