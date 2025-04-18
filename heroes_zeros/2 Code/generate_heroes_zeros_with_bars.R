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

# compute each extreme
max_top    <- max(top5_df$ratingPoints)
min_bottom <- min(bottom5_df$ratingPoints)

# ───────────────────────────────────────────────────────────────────────────────
# 1. Update your bar‐chart theme so panels are #f5f5f5, not white
# ───────────────────────────────────────────────────────────────────────────────
base_bar_theme <- theme_minimal(base_family = "roboto", base_size = 8) +
  theme(
    # keep the grey fill behind the geoms…
    panel.background = element_rect(fill   = "#f5f5f5", colour = NA),
    # …and draw your dotted border around the _whole_ ggplot object
    plot.background  = element_rect(
      fill     = "#f5f5f5",       # same grey so it blends
      colour   = "#AAAAAA",       # border colour
      size     = 0.25,            # line thickness
      linetype = "dotted"
    ),
    # axis/text styling
    axis.title       = element_blank(),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.text.y      = element_text(size = 7, hjust = 1),
    # remove the old panel.border
    panel.border     = element_blank(),
    panel.grid       = element_blank(),
    # small internal margin so the axes/titles don’t butt the border
    plot.margin      = margin(t = 6, r = 6, b = 6, l = 6)
  )
# ───────────────────────────────────────────────────────────────────────────────
# Bottom 5 bar: extend axis to 1.1× the most negative value
# ───────────────────────────────────────────────────────────────────────────────
bottom5_bar <- ggplot(bottom5_df, aes(
  x = reorder(player.fullName, ratingPoints),
  y = ratingPoints
)) +
  geom_col(fill = rating_colours["Bottom 5"], width = 0.3) +
  geom_text(
    aes(
      label = sprintf("%.1f", ratingPoints),
      hjust  = ifelse(ratingPoints < 0, 1.1, -0.1)
    ),
    size   = 2,
    family = "roboto"
  ) +
  coord_flip(expand = FALSE) +
  scale_y_continuous(
    limits = c(min_bottom * 1.2, 0),    # ← stretch left to 110% of the worst drop
    expand = c(0, 0)
  ) +
  labs(title = "Bottom 5 | Details") +
  base_bar_theme +
  theme(plot.margin = margin(5, 5, 5, 5)) +
  theme(plot.title = element_markdown(size   = 8,
                                      family = "roboto",
                                      margin = margin(b = 2)))

bottom5_bar <- bottom5_bar +
  theme(
    plot.title = element_markdown(
      size   = 8,
      family = "roboto",
      margin = margin(t = 8, b = 2)  # 8pt above, 2pt below
    )
  )

# ───────────────────────────────────────────────────────────────────────────────
# Top 5 bar: extend axis to 1.1× the best score
# ───────────────────────────────────────────────────────────────────────────────
top5_bar <- ggplot(top5_df, aes(
  x = reorder(player.fullName, -ratingPoints),
  y = ratingPoints
)) +
  geom_col(fill = rating_colours["Top 5"], width = 0.3) +
  geom_text(
    aes(
      label = sprintf("%.1f", ratingPoints),
      hjust  = ifelse(ratingPoints < 0, 1.1, -0.1)
    ),
    size   = 2.5,
    family = "roboto"
  ) +
  coord_flip(expand = FALSE) +
  scale_y_continuous(
    limits = c(0, max_top * 1.2),      # ← stretch right to 110% of the best haul
    expand = c(0, 0)
  ) +
  labs(title = "Top 5 | Details") +
  base_bar_theme +
  theme(plot.margin = margin(5, 5, 5, 5)) +
  theme(plot.title = element_markdown(size   = 8,
                                      family = "roboto",
                                      margin = margin(b = 2)))

top5_bar <- top5_bar +
  theme(
    plot.title = element_markdown(
      size   = 8,
      family = "roboto",
      margin = margin(t = 8, b = 2)
    )
  )
# ───────────────────────────────────────────────────────────────────────────────
# Combine & save exactly as before
# ───────────────────────────────────────────────────────────────────────────────
combined_plot <- wrap_plots(
  final_plot,
  
  grey_spacer +
    bottom5_bar +
    grey_spacer +
    top5_bar +
    grey_spacer +
    plot_layout(
      ncol   = 5,
      widths = c(0.1, 0.9, 0.1, 0.9, 0.1)
    ),
  
  ncol    = 1,
  heights = c(3, 1.8)   # <-- increase from 1.2 up to 1.8 (or 2.0)
) +
  plot_annotation(
    theme = theme(
      plot.margin     = margin(t = 15, r = 20, b = 40, l = 20),
      plot.background = element_rect(fill = "#f5f5f5", colour = NA)
    )
  )



# 1) Create a "grey spacer" panel instead of plot_spacer()
grey_spacer <- ggplot() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#f5f5f5", colour = NA),
    plot.background  = element_rect(fill = "#f5f5f5", colour = NA)
  )

# 2) Combine everything, using grey_spacer in place of plot_spacer()
combined_plot <- wrap_plots(
  final_plot,
  
  # row 2: left margin | bottom5 | small gap | top5 | right margin
  grey_spacer +
    bottom5_bar +
    grey_spacer +
    top5_bar +
    grey_spacer +
    plot_layout(ncol   = 5,
                widths = c(0.1, 1, 0.05, 1, 0.1)),
  
  ncol    = 1,
  heights = c(3, 1.2)
) +
  # 3) add extra bottom padding so bars never hit the canvas border
  plot_annotation(
    theme = theme(
      plot.margin = margin(t = 15, r = 20, b = 40, l = 20),
      plot.background = element_rect(fill = "#f5f5f5", colour = NA)
    )
  )

# 4) Preview & save
combined_plot

ggsave(
  "heroes_zeros_final_padded.png",
  plot   = combined_plot,
  device = ragg::agg_png,
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 300
)

