#Marlins Hitters 

#JT Realmuto
playerid_lookup(last_name = "Realmuto")

#MLBAM ID: 592663      FangrpahsID: 11739

statcast_2017 %>%
filter(batter == 592663) %>%
write_csv("/Users/owner/Documents/BaseballBookRData/BookWork/pitch level data/Marlins_Hitters/JT_Realmuto_2017.csv")

#load JT Realmuto data from baseballr
Realmuto_Data <- read_csv("/Users/owner/Documents/BaseballBookRData/BookWork/pitch level data/Marlins_Hitters/JT_Realmuto_2017.csv")

library(readr)
library(dplyr)
library(ggplot2)
library(mgcv)

ggplot(JT_Realmuto_2017, 
       aes(x = pitch_type,
           # Barchart with percent along y-axis
           y = (..count..) / sum(..count..))) +
  geom_bar() +
  labs(title = "Types of Pitches Thrown Against JT Realmuto in 2017",
       x = "Pitch Type",
       y = "Proportion of Pitches",
       caption = "Data courtesy of MLBAM") + 
  theme_bw()

realmuto_pitch_summary_stats <- JT_Realmuto_2017 %>%
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL")) %>%
  mutate(
    swing = ifelse(description %in%
                     c("foul", "foul_bunt",
                       "foul_tip", "hit_into_play",
                       "hit_into_play_no_out",
                       "hit_into_play_score",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked"), 
                  1, 0),
    single = ifelse(type == "X" & events == "single", 1, 0),
    double = ifelse(type == "X" & events == "double", 1, 0),
    triple = ifelse(type == "X" & events == "triple", 1, 0),
    home_run = ifelse(type == "X" & events == "home_run", 1, 0)) %>%
  
 
  

realmuto_pitch_summary_stats %>%
  ggplot(aes(x = pitch_type, y = ops, fill = pitch_type)) + 
  scale_fill_brewer(palette = "Set1", guide = FALSE) +
  geom_bar(stat = "identity") + 
  labs(title = "JT Realmuto OPS by Pitch Type in 2017",
       caption = "Data courtesy of MLBAM",
       x = "Pitch Type",
       y = "OPS") +
  theme_bw()

JT_Realmuto_2017 %>%
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL"),
         type == "X") %>%
  ggplot(aes(x = pitch_type, y = launch_speed, fill = pitch_type)) +
  scale_fill_brewer(palette = "Set1", guide = FALSE) + 
  geom_violin(alpha = 0.8, color = "black") + 
  labs(title = "JT Realmuto's Exit Velocity by Pitch Type in 2017",
       caption = "Data courtesy of MLBAM",
       x = "Pitch Type",
       y = "Exit Velocity (mph)") +
  coord_flip() + 
  theme_bw()

top_zone <- 3.5
bot_zone <- 1.6
left_zone <- -0.95
right_zone <- 0.95
strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)

ggplot(JT_Realmuto_2017, aes(x = plate_x, y = plate_z)) + 
  geom_point(alpha = 0.5) + 
  geom_path(data = strike_zone_df,aes(x, y), lwd = 1.5, color = "red") + 
  coord_fixed() +
  labs(title = "Location of All Pitches Thrown Against JT Realmuto in 2017 ",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)") +
  theme_bw()

JT_Realmuto_2017 %>%
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL")) %>%
  ggplot(aes(x = plate_x, y = plate_z, color = pitch_type)) + 
  geom_point(alpha = 0.5) + 
  scale_color_brewer(palette = "Set1", "Pitch Type") + 
  geom_path(data = strike_zone_df,aes(x, y), lwd = 1.5, color = "black") + 
  coord_fixed() +
  labs(title = "Location of Pitches Thrown Against JT RealMuto in 2017 by Pitch Type",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)") +
  theme_bw()

realmuto_data <- JT_Realmuto_2017 %>%
  mutate(swing = ifelse(description %in%
                          c("foul", "foul_bunt",
                            "foul_tip", "hit_into_play",
                            "hit_into_play_no_out",
                            "hit_into_play_score",
                            "missed_bunt", "swinging_strike",
                            "swinging_strike_blocked"), 
                        1, 0),
         miss = ifelse(description %in%
                         c("missed_bunt", "swinging_strike",
                           "swinging_strike_blocked"), 
                       1, 0))

swing_model_fit <- gam(swing ~ s(plate_x, plate_z), family = binomial, data = realmuto_data)
x <- seq(-1.5, 1.5, length.out=50)
z <- seq(0.5, 5, length.out=50)
swing_predict_data <- data.frame(plate_x = c(outer(x, z * 0 + 1)),
                                 plate_z = c(outer(x * 0 + 1, z)))
swing_model_preds <- predict(swing_model_fit, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
  mutate(swing_prob = exp(swing_model_preds) / (1 + exp(swing_model_preds)))

ggplot(swing_predict_data) +
  geom_tile(aes(x = plate_x, y = plate_z, fill = swing_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Swing Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  labs(title = "JT Realmuto's Swing Probability in 2017",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 

miss_model_fit <- gam(miss ~ s(plate_x, plate_z), family=binomial, 
                      data = filter(realmuto_data, swing == 1))
miss_model_preds <- predict(miss_model_fit, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
  mutate(miss_prob = exp(miss_model_preds) / (1 + exp(miss_model_preds)))

ggplot(swing_predict_data) +
  geom_tile(aes(x = plate_x, y = plate_z, fill = miss_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Whiff Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  labs(title = "JT Realmuto's Whiff Probability in 2017",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM")

realmuto_data <- realmuto_data %>%
  mutate(count = paste(as.character(balls), "-", as.character(strikes)))
miss_model_fit_two_strikes <- gam(miss ~ s(plate_x, plate_z), family=binomial, 
                                  data = filter(realmuto_data, swing == 1, 
                                                count %in% c( "0 - 2", "1 - 2", "2 - 2", "3 - 2")))
miss_model_preds_two_strikes <- predict(miss_model_fit_two_strikes, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
  mutate(miss_prob_two_strikes = exp(miss_model_preds_two_strikes) / (1 + exp(miss_model_preds_two_strikes)))

ggplot(swing_predict_data) +
  geom_tile(aes(x = plate_x, y = plate_z, fill = miss_prob_two_strikes)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Whiff Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  labs(title = "JT Realmuto's Whiff Probability in 2017 with Two Strikes",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 

velo_model_fit <- gam(launch_speed ~ s(plate_x, plate_z), 
                      data = filter(realmuto_data, type == "X"))
velo_model_preds <- predict(velo_model_fit, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
  mutate(exit_velo = velo_model_preds)
ggplot(swing_predict_data) +
  geom_tile(aes(x = plate_x, y = plate_z, fill = exit_velo)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Exit Velocity (mph)") +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  labs(title = "JT Realmuto's Exit Velocity in 2017",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 

realmuto_data <- JT_Realmuto_2017 %>%
  mutate(hit_x = hc_x - 125.42, 
         hit_y = 198.27 - hc_y)
realmuto_data %>%
  filter(type == "X") %>%
  ggplot(aes(x = hit_x, y = hit_y)) + 
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="darkblue", high="darkorange1", "Density") +
  geom_segment(x=0, xend = 100, y=0, yend = 100, color = "white") +
  geom_segment(x=0, xend = -100, y=0, yend = 100, color = "white") +
  geom_curve(x = -45, xend = 45, y = 53, yend = 53, curvature = -.65, linetype = "dotted", color = "white") +
  theme_bw() + 
  labs(title = "Spray Chart of JT Realmuto's Batted Balls in 2017",
       caption = "Data courtesy of MLBAM") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
