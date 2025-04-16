### HEROES AND ZEROS - Player Rating Analysis and Visuals

### Preamble

# Purpose: 

### Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(writexl)
library(ggbeeswarm)
library(packcircles)
library(ggrepel)
library(fitzRoy)

### Constants and variable initialisation
round_to_analyse = 202505
seasons <- 2025 # Can make this a spread of seasons using 2016:2025
player_stats <- data.frame() # Initialize an empty data frame to store results

### Data Ingestion

# Loop through each season and fetch player stats
for (season in seasons) {
  cat("Fetching data for season:", season, "\n")  # Print progress
  
  # Fetch player stats for the given season
  season_stats <- fetch_player_stats_afl(season = season)
  
  # Combine with the main dataset
  player_stats <- bind_rows(player_stats, season_stats)
}


### Preparation

# Create some useful variables and then limit data to analysis round.
relevant_player_stats <- player_stats %>%
  mutate(
    match_date = as.Date(ymd_hms(utcStartTime)),
    match_year = year(match_date),
    match_year_round = as.numeric(paste0(match_year, str_pad(round.roundNumber, 2, pad = "0"))),
    player.fullName = paste(player.givenName, player.surname)
  ) 


# Beeswarm Chart Preparation

rand_points <- data.frame(x_group = "group",
                          x=rnorm(200, mean=50, sd=10),
                          size=rexp(200, 1))


beeswarm_player_stats <- relevant_player_stats %>%
  filter(match_year_round == round_to_analyse,
         timeOnGroundPercentage>=25,
         !is.na(ratingPoints)) %>%
  select(player.playerId, player.fullName, team.name, ratingPoints) %>%
  arrange(desc(ratingPoints)) %>%
  # flag top 5, bottom 5 for the current round
  mutate(
    rating_rank = row_number(desc(ratingPoints)),
    rating_group = case_when(
      rating_rank <= 5 ~ "Top 5",
      rating_rank > n() - 5 ~ "Bottom 5",
      TRUE ~ "Other"
    )
  )

### Charts

# Beeswarm

# Store values so we can get the dot labelling done correctly when plotting
# Defensive build of labelled players
labelled_players <- beeswarm_player_stats %>%
  arrange(desc(ratingPoints)) %>%
  mutate(rating_rank = row_number()) %>%
  filter(rating_rank <= 5 | rating_rank > n() - 5) %>%
  mutate(nudge_y = rep(c(0.25, -0.25), length.out = n()))

# The actual plot
p <- ggplot(beeswarm_player_stats, aes(x = ratingPoints, y = 0, colour = rating_group)) +
  geom_quasirandom(
    width = 0.3,
    size = 4,
    alpha = 0.8,
    method = "quasirandom"
  ) +
  geom_text_repel(
    data = labelled_players,
    aes(x = ratingPoints, y = 0, label = player.fullName),
    colour = "black",
    size = 3.5,
    segment.color = "black",
    segment.size = 0.2,
    segment.curvature = 0.1,
    segment.ncp = 3,
    segment.angle = 20,
    direction = "y",
    nudge_y = labelled_players$nudge_y,
    force = 2,
    force_pull = 0.01,
    box.padding = 0.15,
    point.padding = 0.1,
    max.overlaps = 50,
    inherit.aes = FALSE
  ) +
  ylim(-0.5, 0.5) +  # ← CRITICAL!
  scale_color_manual(values = c("Top 5" = "#0E6ECE", "Bottom 5" = "#F56580", "Other" = "#BCBFBE")) +
  labs(
    title = paste("Heroes and Zeros – Round", str_sub(round_to_analyse, 5, 6)),
    subtitle = paste("Player Ratings –", str_sub(round_to_analyse, 1, 4)),
    x = "Player Rating Points",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 18, hjust = 0, colour = "black"),
    plot.subtitle = element_text(size = 14, hjust = 0, colour = "black"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(30, 30, 30, 30)
  )


# Print the plot
print(p)


### Outputs