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
swarm_plot <- ggplot(beeswarm_player_stats, aes(x = ratingPoints, y = 0, colour = rating_group)) +
  geom_quasirandom(
    aes(fill = rating_group, alpha = 0.4),
    method = "quasirandom",
    width  = 0.3,
    size   = 3,
    stroke = 0.8,
    shape  = 21
  ) +
  scale_y_continuous(
    NULL,
    breaks  = NULL,
    limits  = c(-0.5, 0.5),
    expand  = expansion(add = 0.02)
  ) +
  scale_x_continuous(
    breaks       = seq(x_min, x_max, by = 5),
    minor_breaks = NULL,
    position     = "bottom"
  ) +
  scale_fill_manual(values = rating_colours) +
  scale_colour_manual(values = rating_colours) +
  labs(
    title    = "<b>Footy Heroes and Zeros</b> |<span style='font-weight:100;'>AFL 2025, Round 5</span>",
    subtitle = "Beeswarm distribution of AFL player ratings (≥25% time on ground), highlighting the <span style='color:#0E6ECE;'><b>Top 5</b></span> & <span style='color:#F56580;'><b>bottom 5</b></span> of the round.",
    x        = "Player Rating Points",
    caption  = "Visual: Darragh Murray for <span><b>Roar Metrics</b></span> | Data: fitzRoy"
  ) +
  theme_minimal(base_family = "roboto", base_size = 12) +
  theme(
    # Title in Bebas Neue, with extra bottom margin
    plot.title       = element_markdown(
      size       = 22,
      family     = "bebas_neue", 
      lineheight = 1,
      margin     = margin(b = 5)
    ),
    # Subtitle in Roboto, padded from title
    plot.subtitle    = element_markdown(
      size       = 10,
      family     = "roboto",
      lineheight = 1.1,
      margin     = margin(t = 5, b = 5)
    ),
    # switch to element_markdown for the caption
    plot.caption = element_markdown(
      size   = 6,
      family = "roboto",
      colour = "grey50",
      hjust  = 1
    ),
    # Move axis title to bottom, remove top title
    axis.title.x.top    = element_blank(),
    axis.title.x.bottom = element_text(
      size   = 8,
      margin = margin(t = 6, b = 4)
    ),
    
    axis.text.x      = element_text(size = 8),
    legend.position     = "none",
    
    # Lighter, slightly thicker gridlines for print
    panel.grid.major.x  = element_line(
      linetype = "dotted",
      size     = 0.25,
      colour   = "#AAAAAA"
    ),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank(),
    
    # Background & margins
    panel.background    = element_rect(fill = "#f5f5f5", colour = NA),
    plot.background     = element_rect(fill = "#f5f5f5", colour = NA),
    plot.margin         = margin(t = 15, r = 20, b = 15, l = 20)
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
    aes(x = ratingPoints, y = y_position*0.35 - sign(y_position)*0.01,
        xend = ratingPoints, yend = 0 + sign(y_position)*0.03),
    curvature = 0.2, arrow = arrow(length = unit(0.02, "npc"), type = "open"),
     size = 0.4, colour = labelled_players$segment_colour, inherit.aes = FALSE
  )

final_plot 


# BAR CHARTS -----
library(patchwork)
library(ggtext)

# compute each extreme
max_top    <- max(top5_df$ratingPoints)
min_bottom <- min(bottom5_df$ratingPoints)

# 1) A new base theme for the little bar panels
bar_panel_theme <- theme_minimal(base_family = "roboto", base_size = 10) +
  theme(
    panel.background    = element_rect(fill   = "white", colour = NA),
    plot.background     = element_rect(
      fill     = "white",
      colour   = "#AAAAAA",
      linetype = "dotted",
      size     = 0.3
    ),
    
    # POSITION RELATIVE TO PLOT, AND BUMP RIGHT A TAD
    plot.title.position = "plot",
    plot.title          = element_markdown(
      size   = 11,
      family = "roboto",
      hjust  = 0.03,   # <- shift this number until it lines up
      margin = margin(t = 5, r = 0, b = 8, l = 0)
    ),
    
    # Y labels flush left at the same spot
    axis.text.y         = element_text(
      size   = 9,
      colour = "grey30",
      hjust  = 0,
      margin = margin(l = 15)  # <- roughly match the hjust offset
    ),
    
    # strip out everything else
    axis.title.x        = element_blank(),
    axis.title.y        = element_blank(),
    axis.text.x         = element_blank(),
    axis.ticks.x        = element_blank(),
    panel.grid          = element_blank(),
    
    # small white gutter inside the dotted border
    plot.margin         = margin(t = 5, r = 5, b = 5, l = 5)
  )



# 2) Top 5 bar chart
top5_bar <- ggplot(top5_df, aes(
  x = ratingPoints,
  y = fct_reorder(player.fullName, ratingPoints)
)) +
  geom_col(fill = rating_colours["Top 5"], width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", ratingPoints)),
            hjust = -0.1, size = 3, family = "roboto") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "<span style='color:#0E6ECE;'><b>Top 5</b></span> players by rating points"
  ) +
  bar_panel_theme

# 3) Bottom 5 bar chart
bottom5_bar <- ggplot(bottom5_df, aes(
  x = ratingPoints,
  y = fct_reorder(player.fullName, -ratingPoints)
)) +
  geom_col(fill = rating_colours["Bottom 5"], width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", ratingPoints)),
            hjust = ifelse(bottom5_df$ratingPoints < 0, 1.1, -0.1),
            size = 3, family = "roboto") +
  scale_x_continuous(
    limits = c(min_bottom * 1.2, 0),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "<span style='color:#F56580;'><b>Bottom 5</b></span> players by rating points"
  ) +
  bar_panel_theme

grey_spacer <- ggplot() +
  theme_void() +
  theme(
    panel.background = element_rect(fill   = "#f5f5f5", colour = NA),
    plot.background  = element_rect(fill   = "#f5f5f5", colour = NA)
  )


# 4) Patchwork them in under your swarm
# 1) Build the bars row in one go
bars_row <- wrap_plots(
  # left margin | bottom5 | gutter | top5 | right margin
  grey_spacer, 
  bottom5_bar, 
  grey_spacer, 
  top5_bar, 
  grey_spacer,
  ncol   = 5,
  widths = c(0.1, 1, 0.1, 1, 0.1)
)

# 2) Now stack the swarm over the bars row
combined_plot <- wrap_plots(
  final_plot,
  bars_row,
  ncol    = 1,
  heights = c(3, 1.5)    # tweak as needed
) + 
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "#f5f5f5", colour = NA),
      plot.margin     = margin(t = 15, r = 20, b = 40, l = 20)
    )
  )

# Preview
combined_plot

# 3) Save out
ggsave(
  "heroes_zeros_final_boxed.png",
  combined_plot,
  device = ragg::agg_png,
  width  = 8, 
  height = 8, 
  units  = "in", 
  dpi    = 300
)

