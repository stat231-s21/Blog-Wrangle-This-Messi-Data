load("data/savant_data_filtered.Rdata")
load("data/savant_data.Rdata")
library(tidyverse)
library(ggplot2)
library(GeomMLBStadiums)

spatial_data <- savant_data_filtered %>%
  filter(events %in% c("field_out", "single", "double", "home_run", "sac_fly",
                         "sac_bunt_double_play", "force_out", "grounded_into_double_play",
                         "field_error", "fielders_choice", "double_play", "triple",
                         "sac_bunt", "fielders_choice_out", "sac_fly_double_play",
                         "triple_play"),
         !is.na(hc_x), !is.na(hc_y)) %>%
  select(player_name, hc_x, hc_y, events, stand, launch_speed, launch_angle) %>%
  mutate(angle = ifelse(hc_x < 125, 
                        atan((208 - hc_y) / (125 - hc_x)) * 180 / pi - 45, 
                        135 - atan((208 - hc_y) / (hc_x - 125)) * 180 / pi))

  
splits_data <- savant_data_filtered %>%
  filter(events %in% c("field_out", "single", "double", "home_run", "sac_fly",
                        "sac_bunt_double_play", "force_out", "grounded_into_double_play",
                        "field_error", "fielders_choice", "double_play", "triple",
                        "sac_bunt", "fielders_choice_out", "sac_fly_double_play",
                        "triple_play", "walk", "hit_by_pitch", "strikeout", "other_out", 
                       "strikeout_double_play", "catcher_interf"),
         balls != 4, strikes != 3) %>%
  mutate(pitch_type = case_when(pitch_type == "KC" ~ "CU",
                                TRUE ~ pitch_type)) %>%
  filter(pitch_type %in% c("FF", "CH", "SL", "FT", "SI", "CU", "FC")) %>%
  select(player_name, events, pitch_type, p_throws, strikes, balls, on_1b, on_2b, on_3b,
         inning_topbot) %>%
  mutate(count = paste(balls, "-", strikes, sep = " "),
         runners_on = ifelse(is.na(on_1b), 0, 1) + 
                      ifelse(is.na(on_2b), 0, 1) +
                      ifelse(is.na(on_3b), 0, 1),
         home_away = ifelse(inning_topbot == "Top", "away", "home")) %>%
  select(-inning_topbot)

game_sim_data <- savant_data %>%
  mutate(pitch_type = case_when(pitch_type == "KC" ~ "CU",
                                TRUE ~ pitch_type),
         pitch_name = case_when(pitch_name == "Knuckle Curve" ~ "Curveball",
                                TRUE ~ pitch_name),
         des = sub("Jr.", "Jr", des)) %>%
  filter(pitch_type %in% c("FF", "CH", "SL", "FT", "SI", "CU", "FC")) %>%
  select(player_name, pitch_type, pitch_name, events, stand, p_throws, strikes, balls, 
         outs_when_up, on_1b, on_2b, on_3b, hc_x, hc_y, type, events, description, 
         plate_z, plate_x, release_pos_x, release_pos_z, des)

# User is pitcher -> they pick the pitch type, goal is to get a strikeout
# start with a 0-0 count no outs.
# User picks throwing hand, batting side for entire at bat
# For each pitch select pitch type

# We filter all these things

# Randomly select an event and maybe animate it happening

save(splits_data, file = "SplitsPlot/data/splits_data.Rdata")
save(spatial_data, file = "data/spatial_data.Rdata")
save(game_sim_data, file = "BaseballGame/data/game_sim.Rdata")
save(game_sim_data, file = "data/game_sim.csv")

