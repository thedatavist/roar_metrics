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

rating_colors <- c(
  "Top 5" = "#0E6ECE",
  "Bottom 5" = "#F56580",
  "Other" = "#BCBFBE"
)

# Beeswarm chart
labelled_players <- beeswarm_player_stats %>%
  filter(rating_group != "Other") %>%
  mutate(
    y_position = rep(c(0.15, -0.15), length.out = n()),
    label_color = case_when(
      rating_group == "Top 5" ~ "#0E6ECE",
      rating_group == "Bottom 5" ~ "#F56580",
      TRUE ~ "#BCBFBE"
    ),
    segment_color = label_color
  )



swarm_plot <- ggplot(beeswarm_player_stats, aes(x = ratingPoints, y = 0, colour = rating_group)) +
  geom_quasirandom(
    aes(fill = rating_group, 
        colour = rating_group,
        alpha = I(0.5)
    ),
    method = "quasirandom",
    width = 0.3,
    size = 4,
    stroke = 1,
    shape = 21
  ) +
  geom_text_repel(
    data = labelled_players,
    aes(
      x = ratingPoints,
      y = y_position,  # manual y placement
      label = player.fullName,
      color = rating_group
    ),
    size = 3.5,
    direction = "y",        # only up/down
    nudge_y = 0,            # no auto nudge
    force = 1,
    force_pull = 0.01,
    box.padding = 0.05,
    point.padding = 0.05,
    max.overlaps = 100,
    segment.color = labelled_players$segment_color,
    segment.linetype = "dashed",
    segment.size = 0.15,
    segment.alpha = 0.7,
    segment.curvature = 0,
    min.segment.length = 0,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(NULL, breaks = NULL, limits = c(-.5, .5)) +
  scale_fill_manual(values = rating_colors) +
  scale_color_manual(values = rating_colors) +
  labs(
    title = paste("Heroes and Zeros – Round", str_sub(round_to_analyse, 5, 6)),
    subtitle = paste("Player Ratings –", str_sub(round_to_analyse, 1, 4)),
    x = "Player Rating Points",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

print(swarm_plot)



### Outputs



### Outputs