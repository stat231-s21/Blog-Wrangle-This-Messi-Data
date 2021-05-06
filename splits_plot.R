load("data/splits_data/Rdata")
library(tidyverse)
library(ggplot2)
library(GeomMLBStadiums)

splitsTest <- splits_data %>%
  filter(player_name == "Trout, Mike") 