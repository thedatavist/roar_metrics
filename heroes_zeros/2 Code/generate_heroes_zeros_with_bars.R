# ───────────────────────────────────────────────────────────────────────────────
# Full R script with much smaller, screen‑friendly font sizes
# ───────────────────────────────────────────────────────────────────────────────

library(fitzRoy)
library(tidyverse)
library(ggbeeswarm)  # for geom_quasirandom()
library(ggtext)      # for element_markdown()
library(showtext)    # for Google fonts
library(grid)        # for arrow() and unit()
library(glue)

# 1. Fetch data (same as before) -----------------------------------------------
round_to_analyse <- 202505
seasons          <- 2025

player_stats <- tibble()
for (season in seasons) {
  season_stats <- fetch_player_stats_afl(season = season)
  player_stats <- bind_rows(player_stats, season_stats)
}

# pull out the year & round from your data
current_year  <- round_to_analyse %/% 100
current_round <- round_to_analyse %% 100


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

# Re‐build the beeswarm with a dynamic title
swarm_plot <- ggplot(beeswarm_player_stats, aes(x = ratingPoints, y = 0, colour = rating_group)) +
  geom_quasirandom(
    aes(fill = rating_group, alpha = 0.4),
    method = "quasirandom",
    width  = 0.3,
    size   = 3.5,
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
    title = glue(
      "<b>Footy Heroes and Zeros</b> |",
      "<span style='font-weight:100;'>AFL {current_year}, Round {current_round}</span>"
    ),
    subtitle = "Beeswarm distribution of AFL player ratings (≥25% time on ground), highlighting the <span style='color:#0E6ECE;'><b>Top 5</b></span> & <span style='color:#F56580;'><b>Bottom 5</b></span> of the round.",
    x        = "Player Rating Points"
  ) +
  theme_minimal(base_family = "roboto", base_size = 12) +
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
    plot.caption       = element_blank(),
    
    # Axis styling
    axis.title.x.top    = element_blank(),
    axis.title.x.bottom = element_text(
      size   = 8,
      margin = margin(t = 6, b = 4)
    ),
    axis.text.x         = element_text(size = 8),
    legend.position     = "none",
    
    # Gridlines for print
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
  coord_cartesian(xlim = c(x_min, x_max), clip = "off")



final_plot <- swarm_plot +
  geom_text(
    data        = labelled_players,
    aes(x = ratingPoints, y = y_position*0.5, label = label_text),
    size        = 5,
    lineheight  = 1,
    family      = "roboto",
    inherit.aes = FALSE,
    colour = labelled_players$segment_colour
  ) +
  geom_curve(
    data       = labelled_players,
    aes(x = ratingPoints, y = y_position*0.35 - sign(y_position)*0.02,
        xend = ratingPoints, yend = 0 + sign(y_position)*0.03),
    curvature = 0.2, arrow = arrow(length = unit(0.02, "npc"), type = "open"),
     size = 0.4, colour = labelled_players$segment_colour, inherit.aes = FALSE
  )

final_plot 


# BAR CHARTS -----
library(patchwork)
library(ggtext)

# Extract the Top/Bottom 5 data for the bar charts
top5_df <- beeswarm_player_stats %>%
  filter(rating_group == "Top 5") %>%
  arrange(ratingPoints)

bottom5_df <- beeswarm_player_stats %>%
  filter(rating_group == "Bottom 5") %>%
  arrange(desc(ratingPoints))

# compute each extreme
max_top    <- max(top5_df$ratingPoints)
min_bottom <- min(bottom5_df$ratingPoints)



# 1) A new base theme for the little bar panels
bar_panel_theme <- theme_minimal(base_family = "roboto", base_size = 10) +
  theme(
    # 1) match the swarm background
    panel.background = element_rect(
      fill   = "#f5f5f5",
      colour = NA
    ),
    
    # 2) darker, heavier dotted border around the whole panel
    plot.background  = element_rect(
      fill     = "#f5f5f5",   # same grey so it blends
      colour   = "#333333",   # darker frame
      linetype = "dotted",
      size     = 0.6          # bump up thickness
    ),
    
    # 3) your existing title + y–text tweaks unchanged
    plot.title.position = "plot",
    plot.title          = element_markdown(
      size   = 11,
      family = "roboto",
      hjust  = 0.03,
      margin = margin(t = 5, r = 0, b = 8, l = 0)
    ),
    axis.text.y         = element_text(
      size   = 9,
      colour = "grey30",
      hjust  = 0,
      margin = margin(l = 15)
    ),
    
    # 4) strip everything else back out
    axis.title.x        = element_blank(),
    axis.title.y        = element_blank(),
    axis.text.x         = element_blank(),
    axis.ticks.x        = element_blank(),
    panel.grid          = element_blank(),
    plot.margin         = margin(t = 5, r = 8, b = 5, l = 8)
  )




# 2) Top 5 bar chart: 0 → 120% of max, no extra padding
top5_bar <- ggplot(top5_df, aes(
  x = ratingPoints,
  y = fct_reorder(player.fullName,  -ratingPoints)
)) +
  geom_col(fill = rating_colours["Top 5"], width = 0.4) +
  geom_text(aes(label = sprintf("%.1f", ratingPoints)),
            hjust = -0.1, size = 3, family = "roboto") +
  scale_x_continuous(
    limits = c(0, max_top * 1.2),
    expand = c(0, 0)
  ) +
  labs(
    title = "<span style='color:#0E6ECE;'><b>Top 5</b></span> players by rating points"
  ) +
  bar_panel_theme

# 3) Bottom 5 bar chart: 120% of min negative → 0, no extra padding
bottom5_bar <- ggplot(bottom5_df, aes(
  x = ratingPoints,
  y = fct_reorder(player.fullName, ratingPoints)
)) +
  geom_col(fill = rating_colours["Bottom 5"], width = 0.4) +
  geom_text(aes(label = sprintf("%.1f", ratingPoints)),
            hjust = ifelse(bottom5_df$ratingPoints < 0, 1.1, -0.1),
            size = 3, family = "roboto") +
  scale_x_continuous(
    limits = c(min_bottom * 1.2, 0),
    expand = c(0, 0)
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

# 4) Stack swarm + bars and then add the overall caption
combined_plot <- wrap_plots(
  final_plot,
  bars_row,
  ncol    = 1,
  heights = c(3, 1.5)
) +   # <— this plus binds the next call onto the wrap_plots result
  plot_annotation(
    caption = "Visual: Darragh Murray for Roar Metrics | Data: fitzRoy",
    theme = theme(
      plot.caption    = element_markdown(
        size   = 6,
        family = "roboto",
        colour = "grey50",
        hjust  = 1
      ),
      plot.background = element_rect(fill   = "#f5f5f5", colour = NA),
      plot.margin     = margin(t = 15, r = 20, b = 20, l = 20)
    )
  )

# Preview & save

ggsave(
  "heroes_zeros_final_boxed.png",
  combined_plot,
  device = ragg::agg_png,
  width  = 10, height = 8,
  units  = "in", dpi = 300
)




