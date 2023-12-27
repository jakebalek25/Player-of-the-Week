
## Yordan Alvarez ##

library(baseballr)
library(sportyR)
library(ggplot2)
geom_baseball(league = "MLB")

playerid_lookup(last_name = "Alvarez", first_name = "Yordan")

## POW Spray Chart ##

yordan_alvarez_pow <- scrape_statcast_savant_batter(start_date = "2023-10-07",
                              end_date = "2023-10-14",
                              batterid = "670541")

remove <- c("", "walk", "strikeout")
yordan_alvarez_pow <- subset(yordan_alvarez_pow, !(events %in% remove))

yordan_alvarez_pow <- yordan_alvarez_pow %>%
  mutate(
    events = recode(
      events,
      "double" = "Double",
      "home_run" = "Home Run",
      "single" = "Single",
      "field_error" = "Field Error",
      "field_out" = "Field Out",
      "force_out" = "Force Out"
    )
  )

#### Code from here ####
yordan_alvarez_pow <- mutate(yordan_alvarez_pow, location_x = hc_x - 125.42,
       location_y = 198.27 - hc_y)

yordan_alvarez_pow <- mutate(yordan_alvarez_pow, location_x = 2 * (hc_x - 125.42),
       location_y = 2.5 * (198.27 - hc_y))

yordan_alvarez_pow_spray <- geom_baseball(league = "MLB") +
  geom_point(data = yordan_alvarez_pow,
             aes(location_x, location_y,
                 color = events)) +
  scale_colour_manual(values =
                        c("yellow", "red", "blue", "purple", "green", "black")) +
  ggtitle("Yordan Alvarez ALDS Hits Spray Chart") 
yordan_alvarez_pow_spray


## Season Spray Chart ##
yordan_alvarez_season <- scrape_statcast_savant_batter(start_date = "2023-03-30",
                                                    end_date = "2023-10-01",
                                                    batterid = "670541")
remove1 <- c("", "walk", "strikeout", "hit_by_pitch", "field_out", "field_error",
             "force_out", "double_play", "grounded_into_double_play", "sac_fly")
yordan_alvarez_season <- subset(yordan_alvarez_season, !(events %in% remove1))

yordan_alvarez_season <- yordan_alvarez_season %>%
  mutate(
    events = recode(
      events,
      "double" = "Double",
      "home_run" = "Home Run",
      "triple" = "Triple",
      "single" = "Single"
    )
  )

yordan_alvarez_season <- mutate(yordan_alvarez_season, location_x = hc_x - 125.42,
                             location_y = 198.27 - hc_y)

yordan_alvarez_season <- mutate(yordan_alvarez_season, location_x = 2.05 * (hc_x - 125.42),
                             location_y = 2.25 * (198.27 - hc_y))

yordan_alvarez_season_spray <- geom_baseball(league = "MLB") +
  geom_point(data = yordan_alvarez_season,
             aes(location_x, location_y,
                 color = events)) +
  scale_colour_manual(values =
                        c("red", "blue", "yellow", "purple")) +
  ggtitle("Yordan Alvarez Season Hits Spray Chart") 
yordan_alvarez_season_spray


### POW zone ###
topKzone = 3.5
botKzone = 1.6
inKzone = -.95
outKzone = 0.95
kZone = data.frame(
  x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
  , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

yordan_alvarez_zone_pow <- ggplot(yordan_alvarez_pow, aes(plate_x, plate_z, 
                                                          color = pitch_type)) +
  geom_point() +
  geom_hline(yintercept = c(0, 1.5, 3.5, 5), linetype = "solid", color = "black") +
  geom_vline(xintercept = c(-3, -1, 1, 3), linetype = "solid", color = "black") +
  labs(title = "Hitting Zone Chart", 
       x = "Horizontal Position",
       y = "Vertical Position") +
  geom_path(aes(x, y), data = kZone, color = "red") +
  theme_minimal()
yordan_alvarez_zone_pow


## Season zone ##
yordan_alvarez_zone_season <- ggplot(yordan_alvarez_season, aes(plate_x, plate_z, 
                                                          color = pitch_type)) +
  geom_point() +
  geom_hline(yintercept = c(0, 1.5, 3.5, 5), linetype = "solid", color = "black") +
  geom_vline(xintercept = c(-3, -1, 1, 3), linetype = "solid", color = "black") +
  labs(title = "Hitting Zone Chart", 
       x = "Horizontal Position",
       y = "Vertical Position") +
  geom_path(aes(x, y), data = kZone, color = "red") +
  theme_minimal()
yordan_alvarez_zone_season

