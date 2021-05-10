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
  
spatialPts <- data.frame(x = c("125", "30", "48",
                               "125", "48", "90", 
                               "125", "90", "160",
                               "125", "160", "203",
                               "125", "203", "220"
                                    ),
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


testSpatial2 <- testSpatial %>%
  group_by(zone) %>%
  summarize(player_name = player_name, zoneTot = n()) %>%
  distinct() %>%
  mutate(zone = as.factor(zone), zoneProp = zoneTot/sum(testSpatial2$zoneTot)) 
  

plotTest <- right_join(testSpatial2, spatialPts) 

ggplot(plotTest) +
  geom_polygon(aes(x = x, y = y, group = zone
                   , fill = zoneProp), color = "black") +
  theme_void() +
  coord_fixed(ratio = 1.3) +
  scale_y_reverse() +
  geom_point(data = testSpatial, aes(x = hc_x, y=hc_y, color = events))


# need to create new data frame wtih x and y coordinates 


#code was used to find coordinates of 
#g <- ggplot(data = testSpatial, aes(x = hc_x, y = hc_y, color = zone)) +
#geom_point() +
#geom_mlb_stadium(stadium_ids = "generic", stadium_segments = "all") +
#scale_y_reverse() +
#geom_segment(aes(x = 125, y = 208, xend = 48, yend = 208 - tan(63*pi/180)*77)) +
#geom_segment(aes(x = 125, y = 208, xend = 95, yend =  208 - tan(81*pi/180)*30)) +
#geom_segment(aes(x = 125, y = 208, xend = 155, yend = 208 + tan(99*pi/180)*30)) +
#geom_segment(aes(x = 125, y = 208, xend = 202, yend = 208 + tan(117*pi/180)*77)) 


