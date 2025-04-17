# Preamble ---------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(fitzRoy)
library(tidyverse)
library(showtext)


# Variables, constants, global settings ----------------------------------------

round_to_analyse = 202505 # The round in which we want to generate the H&Z
seasons <- 2025 # Can make this a spread of seasons using 2016:2025

# Load data --------------------------------------------------------------------

player_stats <- data.frame() # empty data frame which we will recursively fillup

# Loop through each season and fetch player stats
for (season in seasons) {
  cat("Fetching data for season:", season, "\n")  # Print progress
  
  # Fetch player stats for the given season
  season_stats <- fetch_player_stats_afl(season = season)
  
  # Combine with the main dataset
  player_stats <- bind_rows(player_stats, season_stats)
}



# Load fonts -------------------------------------------------------------------
font_add_google("Roboto",  "roboto")
font_add_google("Poppins", "poppins")
font_add_google("Inter",   "inter")
showtext_auto()
showtext_opts(dpi = 300)

## Compute scaled base_size ----
ref_w       <- 5
ref_base    <- 16
out_w       <- 10
scale_fact  <- out_w / ref_w
base_sz     <- ref_base * scale_fact  # 32


# Prepare data -----------------------------------------------------------------

# Create some useful variables and then limit data to analysis round.
relevant_player_stats <- player_stats %>%
  mutate(
    match_date = as.Date(ymd_hms(utcStartTime)),
    match_year = year(match_date),
    match_year_round = as.numeric(paste0(match_year, 
                                         str_pad(round.roundNumber, 2, pad = "0"))),
    player.fullName = paste(player.givenName, player.surname)
  ) 

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

rating_colours <- c(
  "Top 5" = "#0E6ECE",
  "Bottom 5" = "#F56580",
  "Other" = "#BCBFBE"
)

labelled_players <- beeswarm_player_stats %>%
  filter(
    rating_rank == 1 | rating_rank == max(rating_rank)  # Top 1 and Bottom 1
  ) %>%
  mutate(
    y_position = c(0.3, -0.3),  # Position label above (top) and below (bottom)
    label_text = paste0(player.fullName, "\n", sprintf("%.1f", ratingPoints)),  # Label with name + rating
    segment_colour = case_when(
      rating_group == "Top 5"    ~ "#0E6ECE",
      rating_group == "Bottom 5" ~ "#F56580",
      TRUE                       ~ "#BCBFBE"
    )
  )

x_min <- floor(min(beeswarm_player_stats$ratingPoints, na.rm = TRUE) / 5) * 5
x_max <- ceiling(max(beeswarm_player_stats$ratingPoints, na.rm = TRUE) / 5) * 5


# Charting ---------------------------------------------------------------------

warm_plot <- ggplot(beeswarm_player_stats, aes(x = ratingPoints, y = 0, colour = rating_group)) +
  geom_quasirandom(
    aes(fill = rating_group, alpha = 0.6),
    method = "quasirandom", width = 0.3, size = 3, stroke = 1, shape = 21
  ) +
  scale_y_continuous(NULL, breaks = NULL, limits = c(-0.5, 0.5)) +
  scale_x_continuous(
    breaks      = seq(x_min, x_max, by = 5),
    minor_breaks= NULL,
    position    = "top"
  ) +
  scale_fill_manual(values = rating_colours) +
  scale_colour_manual(values = rating_colours) +
  labs(
    title    = "<b>Footy Heroes and Zeros</b> <span style='font-weight:300;'>AFL 2025, Round 5</span>",
    subtitle = "‘Beeswarm’ distribution of <b>AFL player ratings</b> …",
    x        = "Player Rating Points",
    caption  = "Visual: Darragh Murray for Roar Metrics | Data: fitzRoy"
  ) +
  theme_minimal(base_family = "roboto", base_size = 16) +
  theme(
    plot.title           = element_markdown(size = 38, family = "inter", margin = margin(b = 6), lineheight = 1),
    plot.subtitle        = element_markdown(size = 18, family = "roboto", margin = margin(b = 30)),
    axis.title.x.top     = element_text(size = 16, margin = margin(b = 5)),
    axis.text.x          = element_text(size = 16),
    plot.caption         = element_text(size = 16, family = "roboto", colour = "grey30", hjust = 1),
    legend.position      = "none",
    panel.grid.major.x   = element_line(linetype = "dotted", size = 0.3, colour = "#898989"),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor     = element_blank(),
    plot.background      = element_rect(fill = "#f5f5f5", colour = NA),
    panel.background     = element_rect(fill = "#f5f5f5", colour = NA),
    plot.margin          = margin(10, 20, 10, 20)
  ) +
  coord_cartesian(xlim = c(x_min, x_max))

# — Add labels & segments —

final_plot <- swarm_plot +
  geom_text(
    data        = labelled_players,
    aes(
      x     = ratingPoints,
      y     = y_position * 0.5,
      label = label_text,
      colour = segment_colour
    ),
    size        = 8,
    lineheight  = 0.5,
    family      = "roboto",
    inherit.aes = FALSE
  ) +
  geom_curve(
    data      = labelled_players,
    aes(
      x     = ratingPoints,
      y     = y_position * 0.5 - sign(y_position) * 0.05,
      xend  = ratingPoints,
      yend  = 0 + sign(y_position) * 0.025
    ),
    curvature = 0.25,
    arrow     = arrow(length = unit(0.01, "npc"), type = "open"),
    size      = 0.6,
    colour    = labelled_players$segment_colour,
    inherit.aes = FALSE
  )


# Output -----------------------------------------------------------------------

print(final_plot)

ggsave(
  filename = paste0("heroes_zeros_", round_to_analyse, ".png"),
  plot = final_plot,
  width = 10,
  height = 6,
  dpi = 300
)
