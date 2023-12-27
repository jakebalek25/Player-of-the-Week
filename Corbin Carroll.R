library(baseballr)
library(sportyR)
library(ggplot2)

corbin_carroll_pow <- scrape_statcast_savant_batter(start_date = "2023-10-02",
                              end_date = "2023-10-6",
                              batterid = "682998")



ggspraychart(coribin_carroll_pow, x_value = "hc_x", y_value = "-hc_y", 
             fill_value = "events") +
  scale_fill_manual(values = c("blue", "red", "green", "yellow", "orange",
                               "purple"))

corbin_carroll_season <- scrape_statcast_savant_batter(start_date = "2023-03-30",
                                                         end_date = "2023-10-01",
                                                         batterid = "682998")

ggspraychart(corbin_carroll_season, x_value = "hc_x", y_value = "-hc_y", 
             fill_value = "events") +
  scale_fill_manual(values = c("blue", "red", "green", "yellow", "orange",
                               "purple", "white", "grey", "black", "cyan", 
                               "brown", "deeppink", "seagreen", "aquamarine", 
                               "darkgoldenrod", "burlywood", "dodgerblue4", "tan4"))


remove <- c("", "walk")
corbin_carroll_pow <- subset(corbin_carroll_pow, !(events %in% remove))

corbin_carroll_zone_pow <- ggplot(corbin_carroll_pow, aes(plate_x, plate_z, 
                                                                color = pitch_type)) +
  geom_point() +
  geom_hline(yintercept = c(0, 1.5, 3.5, 5), linetype = "solid", color = "black") +
  geom_vline(xintercept = c(-2, -1, 1, 2), linetype = "solid", color = "black") +
  labs(title = "Hitting Zone Chart", 
       x = "Horizontal Position",
       y = "Vertical Position",
       color = "Pitch Type") +
  theme_minimal()
corbin_carroll_zone_pow

#subset
corbin_carroll_season <- subset(corbin_carroll_season, !(events %in% remove))


#plot
corbin_carroll_zone_season <- ggplot(corbin_carroll_season, aes(plate_x, plate_z, 
                                                      color = pitch_type)) +
  geom_point() +
  geom_hline(yintercept = c(1.5, 3.5), linetype = "solid", color = "black") +
  geom_vline(xintercept = c(-1, 1), linetype = "solid", color = "black") +
  labs(title = "Hitting Zone Chart", 
       x = "Horizontal Position",
       y = "Vertical Position",
       color = "Pitch Type") +
  theme_minimal()
corbin_carroll_zone_season 


wpa_result <- wpa_bat(corbin_carroll_pow)
print(wpa_result)
