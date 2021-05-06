load("data/savant_data_filtered.Rdata")
load("data/spatial_data.Rdata")
library(tidyverse)
library(ggplot2)
library(GeomMLBStadiums)

#want to make distance variable
#want to separate into 5 categories based on angle
#also want to make our field work with the geom_polygon similar to our maps

testSpatial <- spatial_data %>%
  filter(player_name == "Judge, Aaron") %>%
  mutate(zone = case_when((angle < 18) ~ "1",
                          (18<angle & angle<36) ~ "2", 
                          (36<angle & angle <54) ~ "3",
                          (54<angle & angle <72) ~ "4",
                          (72<angle & angle <90) ~ "5"))# %>%
  #group_by(zone) %>%
 # mutate(zone = as.numeric(zone))
 # summarize (player_name = player_name, angle = angle, sumZone = sum(zone))
  




genericStadium <- MLBStadiumsPathData %>%
  filter(team == "generic") 
  
# need to create new data frame wtih x and y coordinates 



g <- ggplot(data = testSpatial, aes(x = hc_x, y = hc_y, color = zone)) +
    geom_point() +
    geom_mlb_stadium(stadium_ids = "generic", stadium_segments = "all") +
    scale_y_reverse() +
    geom_segment(aes(x = 125, y = 208, xend = 48, yend = 208 - tan(63*pi/180)*77)) +
    geom_segment(aes(x = 125, y = 208, xend = 95, yend =  208 - tan(81*pi/180)*30)) +
    geom_segment(aes(x = 125, y = 208, xend = 155, yend = 208 + tan(99*pi/180)*30)) +
    geom_segment(aes(x = 125, y = 208, xend = 202, yend = 208 + tan(117*pi/180)*77)) 
  
  
g

#
