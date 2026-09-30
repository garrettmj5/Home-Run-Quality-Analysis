# ------------------------------------------------
# MLB Home Run Quality Analysis
# ------------------------------------------------
# Author: Garrett Johnson
# Objective:
# Evaluate home run quality across players and stadiums
# to distinguish true power from environmentally influenced
# production.
# ------------------------------------------------


# ------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(factoextra)
library(lubridate)
library(ggrepel)
library(scales)
library(forcats)
library(gt)

# Read in home run tracking csv
hr_tracking <- read_csv("home_runs1.csv")


# ------------------------------------------------
# 2. Data Cleaning and Validation
# ------------------------------------------------

# Convert date column into proper date format
hr_tracking$date <- ymd(hr_tracking$date)

# Identify incorrect inputs
hr_tracking %>%
  filter(hit_distance < 300 | 
           exit_velo > 130 | 
           stadiums_gone_in > 30)

# Check data for missing values
colSums(is.na(hr_tracking))
# Review rows for missing HR outcomes
hr_tracking[is.na(hr_tracking$HR), ]

# ------------------------------------------------
# 3. Data Manipulation
# ------------------------------------------------

# Classify home runs based on how many MLB stadiums
# the ball would leave in
hr_tracking <- hr_tracking %>%
  mutate(hr_type = case_when(
    stadiums_gone_in >= 24 & stadiums_gone_in <= 30 ~ "No Doubter",
    
    stadiums_gone_in >= 8  & stadiums_gone_in <= 23 ~ "Mostly Gone",
    
    stadiums_gone_in >= 0  & stadiums_gone_in <= 7  ~ "Doubter"))

# ------------------------------------------------
# 4. Stadium Analysis
# ------------------------------------------------

# Measure which parks produce the highest share of
# borderline home runs
stadium_d_rate <- hr_tracking %>%
  filter(HR == 1) %>%
  filter(stadium != "Field of Dreams" & 
           stadium != "Mexico City" &
           stadium != "Vegas Minor League") %>%
  group_by(stadium) %>%
  summarise(
    total_hr = n(),
    doubter_pct = round(sum(hr_type == "Doubter") / n() * 100, 2)) %>%
  arrange(desc(doubter_pct))

# Visualize the Stadium Doubter Rates
ggplot(stadium_d_rate, aes(x = reorder(stadium, doubter_pct), y = doubter_pct)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +  # nicer color + slimmer bars
  coord_flip() +
  labs(
    title = "HR Doubter Rate by Stadium",
    subtitle = "Share of home runs that would leave 7 or fewer MLB parks",
    x = NULL,
    y = "Doubter Rate"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),   # remove clutter
    panel.grid.minor = element_blank())

# Identify stadiums where elite-quality contact
# does not result in a home run
stadium_nd_rate <- hr_tracking %>%
  filter(HR == 0) %>%
  filter(stadium != "Mexico City") %>%
  group_by(stadium) %>%
  summarise(
    total_hr = n(),
    no_doubter_pct = round(sum(hr_type == "No Doubter") / n() * 100, 2)) %>%
  arrange(desc(no_doubter_pct))

# Visualize the Non-HR No Doubter Rates
ggplot(stadium_nd_rate, aes(x = reorder(stadium, no_doubter_pct), y = no_doubter_pct)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +  # nicer color + slimmer bars
  coord_flip() +
  labs(
    title = "Non-Home Run No Doubter Rate by Stadium",
    subtitle = "Share of home runs that would leave in 24 or more parks but don't",
    x = NULL,
    y = "No Doubter Rate"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),   # remove clutter
    panel.grid.minor = element_blank())

stadium_nd_rate %>%
  filter(is.na(no_doubter_pct) | is.nan(no_doubter_pct) | is.infinite(no_doubter_pct))

# ------------------------------------------------
# 5. Player Home Run Profile Analysis
# ------------------------------------------------

# Build dataset for stacked home run profile visualization
hr_profiles <- hr_tracking %>%
  filter(HR == 1) %>%
  group_by(player) %>%
  filter(n() >= 29) %>% # filter for 7 home runs or greater
  mutate(
    hr_count = n(),
    label = paste0(player, " (", hr_count, ")"), #add hr count to the label
    hr_type = case_when(
      stadiums_gone_in >= 24 ~ "No Doubter",
      stadiums_gone_in >= 8  ~ "Mostly Gone",
      TRUE ~ "Doubter"
    )
  ) %>%
  ungroup()

# Visualize Player home run quality profiles
ggplot(hr_profiles, aes(x = reorder(label, hr_count), fill = hr_type)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(
    title = "HR Profile by Player",
    x = "Player (HR Count)",
    y = "Percentage of HR Type"
  ) +
  theme_minimal()

# ------------------------------------------------
# 6. Power Score Model
# ------------------------------------------------
# Objective:
# Create a composite metric that combines:
# - Home Run quality
# - Production consistency
#
# Exit Velocity retained as a ranking tie breaker

#Player Power
player_power <- hr_tracking %>%
  filter(HR == 1) %>%
  mutate(
    hr_points = case_when(
      hr_type == "No Doubter" ~ 3,
      hr_type == "Mostly Gone" ~ 2,
      hr_type == "Doubter" ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(player) %>%
  summarise(
    total_hr = n(),
    avg_hr_quality = mean(hr_points, na.rm = TRUE),
    no_doubter_rate = mean(hr_type == "No Doubter", na.rm = TRUE),
    avg_exit_velo = mean(exit_velo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_hr >= 13) %>%
  mutate(
    quality_multiplier = 0.75 + (avg_hr_quality / 3) * 0.25,
    quality_adjusted_hr = total_hr * quality_multiplier
  )
# Export player power dataset
write.csv(player_power, "player_power.csv", row.names = FALSE)

# --- Data Preparation ---
# Slice your top 20 and strictly lock the player sorting factor based on total_hr
plot_data <- player_power %>%
  slice_max(total_hr, n = 20, with_ties = FALSE) %>% 
  mutate(player = fct_reorder(player, total_hr)) 

# --- Professional Visual Overhaul ---
ggplot(plot_data) +
  # Draw the connecting dumbbell lines
  geom_segment(
    aes(x = quality_adjusted_hr, xend = total_hr, y = player, yend = player),
    color = "#dcdcdc", 
    linewidth = 1.2
  ) +
  
  # Draw Quality-Adjusted HR points (Triangles)
  geom_point(
    aes(x = quality_adjusted_hr, y = player, color = "Quality-Adjusted HR"), 
    shape = 17, 
    size = 3.5
  ) +
  
  # Draw Actual HR points (Circles)
  geom_point(
    aes(x = total_hr, y = player, color = "Actual HR"), 
    shape = 16, 
    size = 3.5
  ) +
  
  # Inject premium contrast color palette
  scale_color_manual(
    values = c("Actual HR" = "#132B43", "Quality-Adjusted HR" = "#4192D9")
  ) +
  
  # Clean text labels
  labs(
    title = "How HR Quality Changes Home Run Production",
    subtitle = "Top 20 players by actual home runs",
    x = "Home Runs",
    y = NULL,
    color = NULL # Removes the legend title header
  ) +
  
  # Polish the canvas
  theme_minimal(base_size = 12) + 
  theme(
    # Darken and emphasize typography
    plot.title = element_text(face = "bold", size = 15, color = "#222222"),
    plot.subtitle = element_text(size = 11, color = "#555555", margin = margin(b = 15)),
    
    # Strip distracting horizontal gridlines; keep clean vertical lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#f0f0f0"),
    panel.grid.minor.x = element_blank(),
    
    # Keep legend tidy and close to the data
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(b = 5)
  )

col3_plot <- plot_data %>%
  select(
    player,
    total_hr,
    avg_hr_quality,
    quality_adjusted_hr
  )

# 1. Prepare top 20 rows and format calculated metrics
table_data <- player_power %>%
  slice_max(total_hr, n = 20, with_ties = FALSE) %>%
  mutate(
    # Calculate difference between actual and quality adjusted
    hr_diff = quality_adjusted_hr - total_hr,
    player = stringr::str_to_title(player) 
  ) %>%
  arrange(desc(total_hr)) %>%
  select(
    player, total_hr, quality_adjusted_hr, hr_diff, 
    avg_hr_quality, no_doubter_rate, avg_exit_velo
  )

# 2. Build the polished gt table 
gt_table <- table_data %>%
  gt() %>%
  
  # Add Titles & Header Elements
  tab_header(
    title = "How HR Quality Changes Home Run Production",
    subtitle = "Top 20 players ranked by actual home run totals"
  ) %>%
  
  # Format Column Labels cleanly
  cols_label(
    player = "Player",
    total_hr = "Actual",
    quality_adjusted_hr = "Quality-Adj.",
    hr_diff = "+ / -",
    avg_hr_quality = "Quality Score (1-3)",
    no_doubter_rate = "No-Doubter %",
    avg_exit_velo = "Avg Exit Velo (mph)"
  ) %>%
  
  # Group metric subsets under structural headers
  tab_spanner(
    label = "Home Run Totals",
    columns = c(total_hr, quality_adjusted_hr, hr_diff)
  ) %>%
  tab_spanner(
    label = "Advanced Quality Metrics",
    columns = c(avg_hr_quality, no_doubter_rate, avg_exit_velo)
  ) %>%
  
  # Precision Data Formatting
  fmt_integer(columns = total_hr) %>%
  fmt_number(columns = c(quality_adjusted_hr, avg_hr_quality, avg_exit_velo), decimals = 1) %>%
  fmt_percent(columns = no_doubter_rate, decimals = 1) %>%
  fmt_number(columns = hr_diff, decimals = 1, force_sign = TRUE) %>%
  
  # Visual Styling and Typography alignment
  cols_align(align = "left", columns = player) %>%
  cols_align(align = "center", columns = -player) %>%
  
  # Add subtle color highlighting to the difference column
  data_color(
    columns = hr_diff,
    direction = "column",
    palette = c("#d95f02", "#ffffff", "#1b9e77"), 
    domain = c(min(table_data$hr_diff), 0, max(table_data$hr_diff))
  ) %>%
  
  # Apply clean minimalist theme settings
  tab_options(
    heading.title.font.size = px(18),
    heading.title.font.weight = "bold", # Swapped md() for an explicit weight option
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    column_labels.background.color = "#f9f9f9",
    table.border.top.color = "transparent",
    table.border.bottom.color = "#222222",
    table_body.hlines.color = "#eaeaea",
    data_row.padding = px(6)
  )

# Display table
gt_table

gt_table %>% gtsave("hr_quality_table_fin.png")

#----------------------------------------------
# Home Run Expectancy
#----------------------------------------------

# Stadium Quality Multiplier
hr_tracking <- hr_tracking %>%
  mutate(
    hr_weight = case_when(
      hr_type == "No Doubter" ~ 1,
      hr_type == "Mostly Gone" ~ 2/3,
      hr_type == "Doubter" ~ 1/3,
      TRUE ~ NA_real_))

# Remove the "Special" Stadiums from Analysis
stadium_multiplier <- hr_tracking %>%
  filter(HR == 1 & stadium != "Mexico City" & stadium != "Vegas Minor League" &
           stadium != "Field of Dreams") %>%
  group_by(stadium) %>%
  summarise(
    actual_hr = n(),
    quality_multiplier = mean(hr_weight, na.rm = TRUE),
    .groups = "drop")

# Create expected home runs
hr_tracking <- hr_tracking %>%
  mutate(
    xHR = stadiums_gone_in / 30)

# Create home runs above expected column
hr_tracking <- hr_tracking %>%
  mutate(
    hr_above_expected = HR - xHR
  )

stadium_empirical <- hr_tracking %>%
  filter(!is.na(stadiums_gone_in)) %>%
  group_by(stadium) %>%
  summarise(
    batted_balls = n(),
    
    actual_hr = sum(HR, na.rm = TRUE),
    
    expected_hr = sum(xHR, na.rm = TRUE),
    
    hr_above_expected = actual_hr - expected_hr,
    
    .groups = "drop"
  )

# Create the park effect column
stadium_empirical <- stadium_empirical %>%
  mutate(
    park_effect = actual_hr / expected_hr)

# Get data ready for plot
stadium_empirical_plot <- stadium_empirical %>%
  arrange(expected_hr) %>%
  mutate(
    stadium = factor(stadium, levels = stadium)
  ) %>%
  pivot_longer(
    cols = c(actual_hr, expected_hr),
    names_to = "metric",
    values_to = "hr"
  ) %>%
  mutate(
    metric = recode(
      metric,
      actual_hr = "Actual HR",
      expected_hr = "Expected HR"
    )
  )

# Create the difference for the plot
stadium_empirical <- stadium_empirical %>%
  mutate(
    hr_difference = actual_hr - expected_hr)

# Create Home Run Difference Plot
ggplot(
  stadium_empirical,
  aes(
    x = hr_difference,
    y = reorder(stadium, hr_difference)
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = hr_difference,
      yend = reorder(stadium, hr_difference)
    ),
    color = "grey75",
    linewidth = 1
  ) +
  geom_point(
    color = "#3182BD",
    size = 3
  ) +
  labs(
    title = "Home Runs Above or Below Expected by Stadium",
    subtitle = "Expected HR based on how many MLB parks each batted ball would clear",
    x = "Actual HR − Expected HR",
    y = NULL,
    caption = "Positive = more HRs than expected | Negative = fewer HRs than expected"
  ) +
  theme_minimal() +
  theme(
  axis.text.y = element_text(size = 9),
  axis.text.x = element_text(size = 10),
  axis.title.x = element_text(size = 12),
  plot.title = element_text(size = 18, face = "bold"),
  plot.subtitle = element_text(size = 11),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank()
)

ggsave(
  "stadium_hr_difference.png",
  plot = last_plot(),
  width = 12,
  height = 8,
  dpi = 300
)
