library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(GeomMLBStadiums)

load("data/savant_data_filtered.Rdata")
load("data/spatial_data.Rdata")


#want to see use spatial data where zones are portions of the field
#find angle from the home plate point and classify zone 
spatial_data <- spatial_data %>%
  mutate(angle = ifelse(hc_x < 125, 
                        atan((208 - hc_y) / (125 - hc_x)) * 180 / pi - 45, 
                        135 - atan((208 - hc_y) / (hc_x - 125)) * 180 / pi)) %>%
  mutate(zone = case_when((angle < 18) ~ "1",
                          (18<angle & angle<36) ~ "2", 
                          (36<angle & angle <54) ~ "3",
                          (54<angle & angle <72) ~ "4",
                          (72<angle & angle <90) ~ "5")) %>%
  mutate(events = case_when(str_detect(events,"out") ~ "Out",
                            str_detect(events,"error") ~ "Error",
                            str_detect(events,"play") ~ "Out",
                            str_detect(events,"out") ~ "Out",
                            str_detect(events,"run") ~ "HR",
                            str_detect(events,"single") ~ "1B",
                            str_detect(events,"triple") ~ "3B",
                            str_detect(events,"double") ~ "2B",
                            str_detect(events,"fly") ~ "Sacrifice"))


testSpatial <- spatial_data %>%
  group_by(zone, player_name) %>%
  summarize(zoneTot = n()) %>%
  distinct() %>%
  mutate(zone = as.factor(zone))



#the code was used to find coordinates of our spatialPts dataset (was com)

#g <- ggplot(data = testSpatial, aes(x = hc_x, y = hc_y, color = zone)) +
#geom_point() +
#geom_mlb_stadium(stadium_ids = "generic", stadium_segments = "all") +
#scale_y_reverse() +
#geom_segment(aes(x = 125, y = 208, xend = 48, yend = 208 - tan(63*pi/180)*77)) +
#geom_segment(aes(x = 125, y = 208, xend = 95, yend =  208 - tan(81*pi/180)*30)) +
#geom_segment(aes(x = 125, y = 208, xend = 155, yend = 208 + tan(99*pi/180)*30)) +
#geom_segment(aes(x = 125, y = 208, xend = 202, yend = 208 + tan(117*pi/180)*77)) 

#create the outline for our field so we can make chlorpleth map 
spatialPts <- data.frame(x = c("125", "30", "48",
                               "125", "48", "90", 
                               "125", "90", "160",
                               "125", "160", "203",
                               "125", "203", "220"),
                            y = c("208", "100", "60", 
                                  "208", "60", "20",
                                  "208", "20", "20",
                                  "208", "20", "60",
                                  "208", "60", "100"),
                            zone = c("1", "1", "1", 
                                     "2", "2", "2", 
                                     "3", "3", "3", 
                                     "4", "4", "4",
                                     "5", "5", "5"))

spatialPts <- spatialPts %>%
  mutate(x = as.numeric(x), y = as.numeric(y))


playerChoices <- unique(spatial_data$player_name)
hitChoices <- unique(spatial_data$events)

ui <- fluidPage(
  
 titlePanel("Player Spray Charts"),
 sidebarLayout(
   sidebarPanel(
     selectizeInput(inputId  = "player",
                 label = "Choose Player to View",
                 choices = playerChoices,
                 selected = "Trout, Mike"),
     
     checkboxGroupInput(inputId = "hitLocations",
                   label = "Include Outcome(s)",
                   choices = hitChoices,
                   selected = c("HR", "3B", "2B", "1B"),
                   inline = TRUE)),
   mainPanel(
     plotlyOutput(outputId = "spray")
   )
 )
  
)


server <- function(input, output) {
  
  output$spray <- renderPlotly({
  
    #calculate the proportion hit to each zone based on what player is chosen
  dataSpatial <- spatial_data %>%
    filter(player_name %in% input$player) %>%
    group_by(zone) %>%
    summarize(zoneNum = n()) %>%
    distinct() %>%
    group_by(zone) %>%
    summarize(zoneNum = zoneNum, zoneTot = sum(zoneNum)) %>%
    mutate(zone = as.factor(zone), zoneProp = 100*zoneTot/sum(zoneTot)) %>%
    right_join(spatialPts)
  
  dataSpray <- spatial_data %>%
    filter(player_name %in% input$player) %>%
    filter(events %in% input$hitLocations)
  
  plot <- ggplot(dataSpatial) +
    geom_polygon(aes(x = x, y = y, group = zone
                     , fill = zoneProp), color = "black") +
    theme_void() +
    coord_fixed(ratio = 1.3) +
    scale_y_reverse() 
  
    plot <- plot +
      geom_point(data = dataSpray, aes(x = hc_x, y=hc_y, color = events))
    # +
    #geom_mlb_stadium(stadium_ids = "generic", stadium_segments = "all")
    
  
 ggplotly(plot, tooltip = "all") %>%
   layout(hovermode = 'closest')

  })
}
 
shinyApp(ui = ui, server = server)