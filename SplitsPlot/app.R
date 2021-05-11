library(shiny)
library(tidyverse)
library(plotly)
library(shinyjs)
library(stringr)


load("data/splits_data.Rdata")

battingSplits <- splits_data %>%
  mutate(hit_out = case_when(str_detect(events,"out") ~ "Out",
                            str_detect(events,"error") ~ "Out",
                            str_detect(events,"play") ~ "Out",
                            str_detect(events,"out") ~ "Out",
                            str_detect(events,"run") ~ "Hit",
                            str_detect(events,"single") ~ "Hit",
                            str_detect(events,"triple") ~ "Hit",
                            str_detect(events,"double") ~ "Hit")) %>%
           filter(!is.na(hit_out))

player_list = battingSplits$player_name %>%
  unique() %>%
  sort()

ui <- fluidPage(
  titlePanel("Splits Plot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "player",
        label = "Choose the batter",
        choices = player_list,
        selected = c("Altuve, Jose", "Judge, Aaron", "Posey, Buster", "Yelich, Christian"),
        multiple = TRUE
      ),
      radioButtons(
        inputId = "split",
        label = "Choose data to split by",
        choices = c("home_away", "p_throws", "runners_on", "count", "pitch_type"),
        selected = "home_away",
        inline = FALSE,
      
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("splitPlot"),
      
    )
  )
  
)




server <- function(input, output){
  output$splitPlot <- renderPlot({
    battingPlot <- battingSplits %>%
      filter(player_name %in% input$player) %>%
      group_by(player_name, input$split) %>%
      summarize(average = sum(hit_out == "Hit")/n(), slugging = (sum(events == "single") +
                                     sum(events == "double")*2 + sum(events == "triple")*3 + 
                                     sum(events == "home_run")*4) / n()) %>%
      pivot_wider(names_from = input$split, values_from = c("average", "slugging"))
    
    plot <- ggplot(data) +
      geom_point(aes(x = player_name, y = val_epl, color = "Premier League"), size = 16) +
      geom_point(aes(x = squad_and_year, y = val_champ, color = "Championship"), size = 16) +
      # add arrow to show which year was first
      labs(x = "Player Statistic", 
           y = statChoicesNames[statChoicesValues == input$selectedStat],
           title = "Performance in EPL vs Championship", color = "League") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot
  })
    

   
}


shinyApp(ui = ui, server = server)