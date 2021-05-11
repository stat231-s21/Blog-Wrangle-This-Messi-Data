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
      plotlyOutput("splitPlot"),
      
    )
  )
  
)




server <- function(input, output){
  output$splitPlot <- renderPlotly({
    # Get name of columns for pivot wider down below
    columns <- c()
    for (stat in c("average","slugging")) {
      for (split in  unique(battingSplits[input$split])) {
        columns <- append(columns, paste(stat, split, sep = "_"))
      }
    }
    
    # Get data into the right format
    battingPlot <- battingSplits %>%
      filter(player_name %in% input$player) %>%
      group_by(player_name, !!rlang::sym(input$split)) %>%
      summarize(average = sum(hit_out == "Hit")/n(), slugging = (sum(events == "single") +
                                     sum(events == "double")*2 + sum(events == "triple")*3 + 
                                      sum(events == "home_run")*4) / n()) %>%
      pivot_wider(names_from = input$split, values_from = c("average", "slugging")) %>%
      pivot_longer(cols = columns,
                   names_to = "stat", values_to = "val") %>%
      separate(stat, into = c("stat","split"), sep = "_") %>%
      pivot_wider(names_from = "split", values_from = "val")
    
    # Create plot
    plot <- ggplot(battingPlot) 
    
    # Add a geom_point for every split
    for (i in 3:ncol(battingPlot)) {
      print(colnames(battingPlot)[i])
      plot <- plot +
        geom_point(aes(x = player_name, y = !!rlang::sym(colnames(battingPlot)[i]),
                       text = paste(colnames(battingPlot)[i]), "-", 
                                    round(!!rlang::sym(colnames(battingPlot)[i]), 3)),
                   color = ifelse(i == 3, "blue", "red")) +
        facet_wrap(~stat)
    }

    plot <- plot +
      labs(x = "", y = "Value of Stat") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(plot, tooltip = "text")
  })
    

   
}

shinyApp(ui = ui, server = server)