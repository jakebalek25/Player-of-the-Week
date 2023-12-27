
## Nick Castellanos ##

library(baseballr)
library(sportyR)
library(ggplot2)
geom_baseball(league = "MLB")

playerid_lookup(last_name = "Harper", first_name = "Bryce")

## POW Spray Chart ##

bryce_harper_pow <- scrape_statcast_savant_batter(start_date = "2023-10-07",
                                                    end_date = "2023-10-13",
                                                    batterid = "547180")

remove <- c("", "walk", "strikeout")
bryce_harper_pow <- subset(bryce_harper_pow, !(events %in% remove))

bryce_harper_pow <- bryce_harper_pow %>%
  mutate(
    events = recode(
      events,
      "double" = "Double",
      "home_run" = "Home Run",
      "single" = "Single",
      "field_error" = "Field Error",
      "field_out" = "Field Out",
      "force_out" = "Force Out",
      "grounded_into_double_play" = "Double Play"
    )
  )

#### Code from here ####
bryce_harper_pow <- mutate(bryce_harper_pow, location_x = hc_x - 125.42,
                             location_y = 198.27 - hc_y)

bryce_harper_pow <- mutate(bryce_harper_pow, location_x = 2.25 * (hc_x - 125.42),
                             location_y = 2.25 * (198.27 - hc_y))

bryce_harper_pow_spray <- geom_baseball(league = "MLB") +
  geom_point(data = bryce_harper_pow,
             aes(location_x, location_y,
                 color = events)) +
  scale_colour_manual(values =
                        c("green", "red", "blue", "orange")) +
  ggtitle("Bryce NLDS Hits Spray Chart") 
bryce_harper_pow_spray


## Season Spray Chart ##
bryce_harper_season <- scrape_statcast_savant_batter(start_date = "2023-03-30",
                                                       end_date = "2023-10-01",
                                                       batterid = "547180")
remove1 <- c("", "walk", "strikeout", "hit_by_pitch", "field_out", "field_error",
             "force_out", "double_play", "grounded_into_double_play", "sac_fly")
bryce_harper_season <- subset(bryce_harper_season, !(events %in% remove1))

bryce_harper_season <- bryce_harper_season %>%
  mutate(
    events = recode(
      events,
      "double" = "Double",
      "home_run" = "Home Run",
      "triple" = "Triple",
      "single" = "Single"
    )
  )

bryce_harper_season <- mutate(bryce_harper_season, location_x = hc_x - 125.42,
                                location_y = 198.27 - hc_y)

bryce_harper_season <- mutate(bryce_harper_season, location_x = 2 * (hc_x - 125.42),
                                location_y = 2.25 * (198.27 - hc_y))

bryce_harper_season_spray <- geom_baseball(league = "MLB") +
  geom_point(data = bryce_harper_season,
             aes(location_x, location_y,
                 color = events)) +
  scale_colour_manual(values =
                        c("red", "blue", "yellow", "purple")) +
  ggtitle("Yordan Alvarez Season Hits Spray Chart") 
bryce_harper_season_spray


### POW zone ###
topKzone = 3.5
botKzone = 1.6
inKzone = -.95
outKzone = 0.95
kZone = data.frame(
  x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
  , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

bryce_harper_pow_zone <- ggplot(bryce_harper_pow, aes(plate_x, plate_z, 
                                                          color = pitch_type)) +
  geom_point() +
  geom_hline(yintercept = c(0, 1.5, 3.5, 5), linetype = "solid", color = "black") +
  geom_vline(xintercept = c(-3, -1, 1, 3), linetype = "solid", color = "black") +
  labs(title = "Hitting Zone Chart", 
       x = "Horizontal Position",
       y = "Vertical Position") +
  geom_path(aes(x, y), data = kZone, color = "red") +
  theme_minimal()
bryce_harper_pow_zone


## Season zone ##
bryce_harper_season_zone <- ggplot(bryce_harper_season, aes(plate_x, plate_z, 
                                                                color = pitch_type)) +
  geom_point() +
  geom_hline(yintercept = c(0, 1.5, 3.5, 5), linetype = "solid", color = "black") +
  geom_vline(xintercept = c(-3, -1, 1, 3), linetype = "solid", color = "black") +
  labs(title = "Hitting Zone Chart", 
       x = "Horizontal Position",
       y = "Vertical Position") +
  geom_path(aes(x, y), data = kZone, color = "red") +
  theme_minimal()
bryce_harper_season_zone
