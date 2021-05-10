library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

load("data/savant_data_filtered.Rdata")
load("data/spatial_data.Rdata")

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

ui <- fluidPage(
  
 titlePanel("Player Spray Charts"),
 sidebarLayout(
   sidebarPanel(
     selectInput(inputID = "player",
                 label = "Choose Player(s) to View",
                 choices = playerChoices,
                 selected = "Trout, Mike")),
   mainPanel(
     plotOutput(outputId = "spray")
   )
 )
  
)


server <- function(input, output) {
  
  dataSpray <- spatial_data %>%
    filter(player_name %in% input$player)
  
  
}
 