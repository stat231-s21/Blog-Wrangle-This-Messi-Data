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
                          (72<angle & angle <90) ~ "5"))



genericStadium <- MLBStadiumsPathData %>%
  filter(team == "generic") 
  

g <- ggplot(data = testSpatial, aes(x = hc_x, y = hc_y)) +
    #geom_point() +
    geom_mlb_stadium(stadium_ids = "generic", stadium_segments = "all") +
    scale_y_reverse() + 
    #geom_segment(aes(x = 125, y = 208, xend = 90, yend = tan(63*pi/180)*25)) +
    geom_segment(aes(x = 125, y = 208, xend = 115, yend =  208-tan(81*pi/180)*10)) +
    geom_segment(aes(x = 125, y = 208, xend = 150, yend = 208 + tan(99*pi/180)*25)) 
  

g