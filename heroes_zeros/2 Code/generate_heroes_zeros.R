# ───────────────────────────────────────────────────────────────────────────────
# Full R script with much smaller, screen‑friendly font sizes
# ───────────────────────────────────────────────────────────────────────────────

library(fitzRoy)
library(tidyverse)
library(ggbeeswarm)  # for geom_quasirandom()
library(ggtext)      # for element_markdown()
library(showtext)    # for Google fonts
library(grid)        # for arrow() and unit()

# 1. Fetch data (same as before) -----------------------------------------------
round_to_analyse <- 202505
seasons          <- 2025

player_stats <- tibble()
for (season in seasons) {
  season_stats <- fetch_player_stats_afl(season = season)
  player_stats <- bind_rows(player_stats, season_stats)
}

# 2. Load & configure fonts ---------------------------------------------------
font_add_google("Roboto", family = "roboto")
font_add_google("Bebas Neue", family = "bebas_neue")
showtext_auto()
showtext_opts(dpi = 300)

# 3. Prep data (same as before) ------------------------------------------------
relevant_player_stats <- player_stats %>%
  mutate(
    match_date       = as.Date(ymd_hms(utcStartTime)),
    match_year       = year(match_date),
    match_year_round = as.numeric(paste0(match_year, str_pad(round.roundNumber,2,pad = "0"))),
    player.fullName  = paste(player.givenName, player.surname)
  )

beeswarm_player_stats <- relevant_player_stats %>%
  filter(match_year_round == round_to_analyse,
         timeOnGroundPercentage >= 25,
         !is.na(ratingPoints)) %>%
  select(player.playerId, player.fullName, team.name, ratingPoints) %>%
  arrange(desc(ratingPoints)) %>%
  mutate(
    rating_rank  = row_number(desc(ratingPoints)),
    rating_group = case_when(
      rating_rank <= 5      ~ "Top 5",
      rating_rank > n() - 5 ~ "Bottom 5",
      TRUE                  ~ "Other"
    )
  )

labelled_players <- beeswarm_player_stats %>%
  filter(rating_rank %in% c(1, max(rating_rank))) %>%
  mutate(
    y_position     = c(0.3, -0.3),
    label_text     = paste0(player.fullName, "\n", sprintf("%.1f", ratingPoints)),
    segment_colour = if_else(rating_rank == 1, "#0E6ECE", "#F56580")
  )

x_min <- floor(min(beeswarm_player_stats$ratingPoints)/5)*5
x_max <- ceiling(max(beeswarm_player_stats$ratingPoints)/5)*5
rating_colours <- c("Top 5"="#0E6ECE","Bottom 5"="#F56580","Other"="#BCBFBE")

# 4. Build plot with tiny fonts -----------------------------------------------
swarm_plot <- ggplot(beeswarm_player_stats,
                     aes(x = ratingPoints, y = 0, colour = rating_group)) +
  geom_quasirandom(aes(fill = rating_group, alpha = 0.4),
                   method = "quasirandom", width = 0.3, size = 3, stroke = 0.8, shape = 21) +
  scale_y_continuous(NULL, breaks = NULL, limits = c(-0.5, 0.5)) +
  scale_x_continuous(breaks = seq(x_min, x_max, by = 5),
                     minor_breaks = NULL, position = "top") +
  scale_fill_manual(values = rating_colours) +
  scale_colour_manual(values = rating_colours) +
  labs(
    title    = "<b>Footy Heroes and Zeros</b> |<span style='font-weight:10;'>AFL 2025, Round 5</span>",
    subtitle = "Beeswarm of AFL player ratings (≥25% time on ground), highlighting the <span style='color:#0E6ECE; font-weight:700;'>top 5</span> and <span style='color:#F56580; font-weight:700;'>bottom 5</span> of the round.",
    x        = "Player Rating Points",
    caption  = "Visual: Darragh Murray | Data: fitzRoy"
  ) +
  theme_minimal(base_family = "roboto", base_size = 12) +
  theme(
    plot.title = element_markdown(
      size       = 18,
      family     = "bebas_neue",        # or exactly "bebas neue"
      margin     = margin(b = 10),      # ← up from 10 to 25
      lineheight = 1
    ),
    plot.subtitle = element_markdown(
      size   =  8,
      family = "roboto",
      margin = margin(t =  5, b = 16)   # a little padding above the subtitle
    ),
    axis.title.x.top      = element_text(size = 8, margin = margin(t=10, b = 10)),
    axis.text.x           = element_text(size = 8),
    plot.caption          = element_text(size = 6, family = "roboto", colour = "grey50", hjust = 1),
    legend.position       = "none",
    panel.grid.major.x    = element_line(size = 0.2, linetype = "dotted", colour = "#898989"),
    panel.grid.major.y    = element_blank(),
    panel.grid.minor      = element_blank(),
    plot.background       = element_rect(fill = "#f5f5f5", colour = NA),
    panel.background      = element_rect(fill = "#f5f5f5", colour = NA),
    plot.margin           = margin(8, 16, 8, 16)
  ) +
  coord_cartesian(xlim = c(x_min, x_max))

final_plot <- swarm_plot +
  geom_text(
    data        = labelled_players,
    aes(x = ratingPoints, y = y_position*0.5, label = label_text),
    size        = 2.5,
    lineheight  = 1.5,
    family      = "roboto",
    inherit.aes = FALSE,
    colour = labelled_players$segment_colour
  ) +
  geom_curve(
    data       = labelled_players,
    aes(x = ratingPoints, y = y_position*0.4 - sign(y_position)*0.03,
        xend = ratingPoints, yend = 0 + sign(y_position)*0.02),
    curvature = 0.2, arrow = arrow(length = unit(0.008, "npc"), type = "open"),
    size = 0.4, colour = labelled_players$segment_colour, inherit.aes = FALSE
  )

final_plot 

# 5. Save with ragg at 300 dpi ------------------------------------------------
ggsave(
  filename = paste0("heroes_zeros_", round_to_analyse, ".png"),
  plot     = final_plot,
  device   = ragg::agg_png,
  width    = 9, height = 5, units = "in", dpi = 300
)

