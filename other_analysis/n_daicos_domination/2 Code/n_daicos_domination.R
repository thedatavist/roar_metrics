# Preamble ---------------------------------------------------------------------
#
# NICK DAICOS DOMINATION
# Why is Naicos so g-damn good?
#
# Scripted by: Darragh Murray
# Date: 2025-04-18

# Libraries --------------------------------------------------------------------
library(fitzRoy)
library(tidyverse)
library(showtext)
library(rstudioapi)
library(ggtext)
library(ggrepel)


# Variables, constants, global settings ----------------------------------------

# Get the path of the currently open script
script_path <- getActiveDocumentContext()$path
script_dir <- dirname(script_path)

# Build path: one level up + "4 Visuals"
save_dir <- file.path(script_dir, "..", "4 Visuals")
dir.create(save_dir, showWarnings = FALSE)  # Create if doesn't exist

playerId = 'CD_I1023261' # The specific player we're interested in - in this case Nick Daicos
seasons <- 2022:2025 # Can make this a spread of seasons using 2016:2025


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
font_add_google("Roboto", family = "roboto")
font_add_google("Bebas Neue", family = "bebas_neue")
showtext_auto()
showtext_opts(dpi = 300)

# Plot Scaling -----------------------------------------------------------------

ref_w       <- 5
ref_base    <- 16
out_w       <- 10
scale_fact  <- out_w / ref_w
base_sz     <- ref_base * scale_fact  # 32


# Prepare data -----------------------------------------------------------------

# Create some useful variables and then select the variables we wish to analyse

relevant_player_stats <- player_stats %>% 
  mutate(
    match.date = as.Date(ymd_hms(utcStartTime)),
    match.year = year(match.date),
    match.yearRound = as.numeric(paste0(match.year, 
                                        str_pad(round.roundNumber, 2, pad = "0"))),
    player.fullName = paste(player.givenName, player.surname)
  ) %>%
  select(match.yearRound, player.playerId, player.fullName, disposals, metresGained, scoreInvolvements)

player_summary <- relevant_player_stats %>%
  group_by(player.playerId, player.fullName) %>%
  summarise(
    games.played = n_distinct(match.yearRound),
    total.disposals = sum(disposals, na.rm = TRUE),
    total.metresGained = sum(metresGained, na.rm = TRUE),
    total.scoreInvolvements = sum(scoreInvolvements, na.rm = TRUE),
    avg.disposals = mean(disposals, na.rm = TRUE),
    avg.metresGained = mean(metresGained, na.rm = TRUE),
    avg.scoreInvolvements = mean(scoreInvolvements, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    metres.perDisposal = total.metresGained / total.disposals,
    scoreInvolvements.perDisposal = total.scoreInvolvements / total.disposals,
    target.player = (player.playerId == playerId),
    impact_per_game = (avg.metresGained + (avg.scoreInvolvements * 50)) / games.played
  ) %>%
  filter(games.played >= 5)
        # avg.disposals >=25)


# Get Nick Daicos' stats
target_stats <- player_summary %>%
  filter(player.playerId == playerId)

# Choose a window around him (you can adjust these)
label_buffer <- 2.5  # +/- window for disposals and metres gained

# Get players near him
labelled_players <- player_summary %>%
  filter(
    avg.disposals > (target_stats$avg.disposals - label_buffer) &
      avg.disposals < (target_stats$avg.disposals + label_buffer) &
      avg.metresGained > (target_stats$avg.metresGained - label_buffer * 20) &
      avg.metresGained < (target_stats$avg.metresGained + label_buffer * 20)
  )

labelled_players <- labelled_players %>%
  mutate(
    is_nick = player.playerId == playerId,
    label_colour = if_else(is_nick, "#000000", alpha("#666666", 0.5)),
    line_colour  = if_else(is_nick, "#000000", alpha("#999999", 0.4)),
    fontface     = if_else(is_nick, "bold", "plain")
  )




# Charting ---------------------------------------------------------------------

## Base theme function ---------------------------------------------------------

theme_roar_metrics <- function(x_min = NULL, x_max = NULL, x_pad = NULL) {
  list(
    theme_minimal(base_family = "roboto", base_size = 12),
    theme(
      # Title in Bebas Neue, extra bottom margin
      plot.title = element_markdown(
        size       = 28,
        family     = "bebas_neue",
        lineheight = 1,
        margin     = margin(b = 5)
      ),
      # Subtitle in Roboto, padded from title
      plot.subtitle = element_markdown(
        size       = 10,
        family     = "roboto",
        lineheight = 1.1,
        margin     = margin(t = 5, b = 5)
      ),
      # Remove swarm’s built‐in caption (we'll add it later)
      plot.caption = element_blank(),
      
      # Axis styling
      axis.title = element_text(
        size   = 8,
        margin = margin(t = 6, b = 4)
      ),
      axis.text         = element_text(size = 8),
      legend.position     = "none",
      
      # Gridlines for print
      panel.grid.major.x = element_line(
        linetype = "dotted",
        size     = 0.25,
        colour   = "#AAAAAA"
      ),
      panel.grid.minor   = element_blank(),
      
      # Background & margins
      panel.background = element_rect(fill = "#f5f5f5", colour = NA),
      plot.background  = element_rect(fill = "#f5f5f5", colour = NA),
      plot.margin      = margin(t = 15, r = 20, b = 15, l = 20)
    ),
    coord_cartesian(
      xlim = c(x_min, x_max + x_pad),  # ⬅️ Add extra space on the right
      clip = "off"
    )
  )
}

x_min <- floor(min(player_summary$avg.disposals, na.rm = TRUE))
x_max <- ceiling(max(player_summary$avg.disposals, na.rm = TRUE))
# Identify Nick separately
labelled_nick <- labelled_players %>% filter(player.playerId == playerId)
labelled_others <- labelled_players %>% filter(player.playerId != playerId)

ggplot(player_summary, aes(x = avg.disposals, y = avg.scoreInvolvements)) +
  # Base points
  geom_point(aes(colour = scoreInvolvements.perDisposal),
             size = 3,
             alpha = 0.4,
             show.legend = FALSE) +
  
  # Highlight Nick Daicos' point
  geom_point(
    data = filter(player_summary, player.playerId == playerId),
    shape = 21,
#    fill = "#0E6ECE",
 #   colour = "#0E6ECE",
    size = 3,
    stroke = 1
  ) +
  geom_text_repel(
    data = labelled_players,
    aes(
      label = player.fullName,
      fontface = fontface,
      segment.color = line_colour
    ),
    colour = I(labelled_players$label_colour),  # ⬅️ inject manually
    nudge_x = 10,
    size = 3,
    family = "roboto",
    direction = "y",
    hjust = 0,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = 20,
    show.legend = FALSE
  ) +
  # Theme + scale + labels
  theme_roar_metrics(x_min = x_min, x_max = x_max, x_pad = 0) +
  scale_color_identity() +  # needed to respect our manual colour columns
  scale_color_gradient(low = "#F56580", high = "#0E6ECE") +
  labs(
    title = "Disposals vs Score Involvements",
    subtitle = paste0("Highlighting ", target_stats$player.fullName),
    x = "Average Disposals",
    y = "Average Score Involvements"
  )



# Output -----------------------------------------------------------------------


# Save the plot
ggsave(
  filename = file.path(save_dir, "my_plot.png"),
  plot = my_plot,
  width = 8,
  height = 6,
  dpi = 300
)
