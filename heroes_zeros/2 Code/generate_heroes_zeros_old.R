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
library(showtext)
library(ggtext)
library(grid)
library(gridExtra)
library(fitzRoy)

#### Fonts
font_add_google("Roboto", "roboto")
font_add_google("Poppins", "poppins")
showtext_opts(dpi = 96)

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

rating_colours <- c(
  "Top 5" = "#0E6ECE",
  "Bottom 5" = "#F56580",
  "Other" = "#BCBFBE"
)

# Beeswarm chart
# Label top and bottom rated players for annotation
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

# Calculate axis range snapped to nearest 5s
x_min <- floor(min(beeswarm_player_stats$ratingPoints, na.rm = TRUE) / 5) * 5
x_max <- ceiling(max(beeswarm_player_stats$ratingPoints, na.rm = TRUE) / 5) * 5


swarm_plot <- ggplot(beeswarm_player_stats, aes(x = ratingPoints, y = 0, colour = rating_group)) +
  geom_quasirandom(
    aes(fill = rating_group, 
        colour = rating_group,
        alpha = I(0.6)
    ),
    method = "quasirandom",
    width = 0.3,
    size = 2,
    stroke = 1,
    shape = 21
  ) +
  scale_y_continuous(NULL, breaks = NULL, limits = c(-.5, .5)) +
  scale_x_continuous(breaks = seq(x_min, x_max, by = 5),
                     minor_breaks = NULL,
                     position = "top") +
  scale_fill_manual(values = rating_colours) +
  scale_color_manual(values = rating_colours) +
  labs(
    title = "<b>Footy Heroes and Zeros</b> <span style='font-weight:300;'>| AFL 2025, Round 5</span>",
    subtitle = "‘Beeswarm’ distribution of <b>AFL player ratings</b> (at least 25% game time), highlighting the <span style='color:#0E6ECE; font-weight:700;'>top 5</span> and <span style='color:#F56580; font-weight:700;'>bottom 5</span> of the round.",
    x = "Player Rating Points",
    color = NULL
  ) +
  theme_minimal(base_family = "roboto", base_size = 16) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.title.x.top = element_text(size = 14, margin = margin(b = 5)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.3, linetype = "dotted", colour = "#898989"),
    plot.background = element_rect(fill = "#f5f5f5", colour = NA),
    panel.background = element_rect(fill = "#f5f5f5", colour = NA),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
    plot.title = ggtext::element_markdown(size = rel(1.8), family = "poppins", margin = margin(b = 6), lineheight = 2),
    plot.subtitle = ggtext::element_markdown(size = rel(1.2), family = "roboto", margin = margin(b = 30), lineheight = 2),
  ) + 
  coord_cartesian(xlim = c(x_min, x_max))

#Add repelled labels

swarm_plot_w_labels <- swarm_plot +
  geom_text_repel(
    data = labelled_players,
    aes(
      x = ratingPoints,
      y = 0,
      label = label_text,
      segment.colour = segment_colour  # line only
    ),
    color = labelled_players$segment_colour,       # ✅ text color outside aes
    nudge_y = labelled_players$y_position * 0.5,
    direction = "y",
    force = 2,
    min.segment.length = 0,
    segment.size = 0.6,
    segment.ncp = 3,
    size = 8,
    lineheight = 0.5,
    box.padding = 1.5,
    point.padding = 3,
   # arrow = arrow(length = unit(0.01, "npc"), type = "open"),
    inherit.aes = FALSE
  )

print(swarm_plot_w_labels)

### Outputs
ggsave(
  filename = paste0("heroes_zeros_", round_to_analyse, ".png"),
  plot = swarm_plot_w_labels,
  width = 10,
  height = 6,
  dpi = 300
)


### ALT

swarm_plot_w_labels <- swarm_plot +
  geom_text(
    data = labelled_players,
    aes(
      x = ratingPoints, 
      y = y_position * 0.5, 
      label = label_text,
      colour = segment_colour       # ✅ inside aes, maps per row
    ),
    size = 8,
    lineheight = 0.5,
    family = "roboto",
    inherit.aes = FALSE,
    color = labelled_players$segment_colour,  
  ) +
  geom_curve(
    data = labelled_players,
    aes(
      x = ratingPoints,
      y = y_position * 0.5 - sign(y_position) * 0.05,  # ✅ move start point outside label
      xend = ratingPoints,
      yend = 0 + sign(y_position) * 0.025 
    ),
    curvature = 0.25,
    arrow = arrow(length = unit(0.01, "npc"), type = "open"),
    size = 0.6,
    colour = labelled_players$segment_colour,
    inherit.aes = FALSE
  )

print(swarm_plot_w_labels)

caption_text <- textGrob(
  label = "Visual: Darragh Murray for Roar Metrics | Data: fitzRoy",
  x = 1, y = 0,
  just = c("right", "bottom"),
  gp = gpar(fontsize = 16, fontfamily = "roboto", col = "grey30")
)

final_plot <- swarm_plot_w_labels +
  annotation_custom(caption_text)

print(final_plot)

### Outputs
ggsave(
  filename = paste0("heroes_zeros_", round_to_analyse, ".png"),
  plot = final_plot,
  width = 10,
  height = 6,
  dpi = 300
)
