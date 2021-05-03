library(baseballr)
library(stringr)
library(dplyr)

savant_data = data.frame()

for (month in 4:9) {
  for (day in 1:31) {
    if (day == 31 & month %in% c(4, 6, 9)) {
      next
    }
    print(paste(day, month))
    date <- paste("2019", str_pad(month, 2, pad = "0"), str_pad(day, 2, pad = "0"), sep = "-")
    data <- scrape_statcast_savant(date, date)
    savant_data <- rbind(savant_data, data)
  }
}

save(savant_data, file="data/savant_data.Rdata")

savant_data_filtered <- savant_data %>%
  select(pitch_type, game_date, release_speed, release_pos_x, release_pos_y, player_name, 
         pitcher...8, events, zone, stand, p_throws, type, hit_location, balls, strikes, on_3b, 
         on_2b, on_1b, outs_when_up, inning, hc_x, hc_y, launch_speed, launch_angle, effective_speed,
         estimated_ba_using_speedangle, estimated_woba_using_speedangle, pitch_number, pitch_name,
         bat_score, fld_score, inning_topbot)

save(savant_data_filtered, file="data/savant_data_filtered.Rdata")

