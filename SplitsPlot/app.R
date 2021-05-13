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
        choices = list("Home/Away"= "home_away", "Pitcher Side" = "p_throws", "Runners on" = "runners_on", 
                      "Count" = "count", "Pitch Type" = "pitch_name"),
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
      separate(stat, into = c("stat","split"), sep = "_") 
    
    # %>%
     # rename("Batting Average" = average, "Slugging Percentage" = slugging)
    
    # Create plot
   #stat_names <- list("Batting Average", "Slugging Percentage")
    
   # stat_labeller <- function(variable,value){
      #return(stat_names[value])
   # }
    
    plot <- ggplot(battingPlot) +
      geom_point(aes(x = player_name, color = split, y = val,
                     text = paste0("<b>", toupper(split), ":</b> ", 
                                   str_pad(round(val, 3), width = 5, pad = "0", side = "right")))) +
      facet_wrap(~stat) +
      labs(x = "", y = "Value of Stat", color = "Split") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(plot, tooltip = "text") %>%
      layout(hovermode = 'compare')
  })
    

   
}

shinyApp(ui = ui, server = server)
