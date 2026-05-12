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
library(broom)
library(knitr)
library(tidyverse)
library(class)
library(factoextra)
library(lubridate)
library(ggrepel)
library(scales)

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
  filter(stadium != "Mexico City") %>%
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
    subtitle = "Percentage of home runs classified as 'doubters'",
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
    subtitle = "Percentage of Non-home runs classified as 'No Doubters'",
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


# ------------------------------------------------
# Player Home Run Profile Analysis
# ------------------------------------------------


# Build dataset for stacked home run profile visualization
hr_profiles <- hr_tracking %>%
  filter(HR == 1) %>%
  group_by(player) %>%
  filter(n() >= 8) %>% # filter for 7 home runs or greater
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
# Power Score Model
# ------------------------------------------------
# Objective:
# Create a composite metric that combines:
# - Home Run quality
# - Production consistency
#
# Exit Velocity retained as a ranking tie breaker


# Create a Weighted Home Run quality score
player_power <- hr_tracking %>%
  filter(HR == 1 | hr_type == "No Doubter") %>%
  group_by(player) %>%
  summarise(
    total = n(),
    hr_quality = mean(case_when(
      hr_type == "No Doubter" ~ 3,
      hr_type == "No Doubter" & HR == 0 ~ 2.5,
      hr_type == "Mostly Gone" ~ 2,
      TRUE ~ 1
    )),
    avg_exit_velo = mean(exit_velo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total >= 8) %>%
  mutate(
    q_z = scale(hr_quality)[,1], # values have same scale, q_z = 1 → player is 1 SD above avg HR quality
    volume_weight = log(total), # 
    power_score = .40*q_z + .60*volume_weight
  )
# Export player power dataset
write.csv(player_power, "player_power.csv", row.names = FALSE)

# Power Score Visualization
ggplot(player_power, aes(x = power_score, y = total)) +
  geom_point(color = "#2C7FB8", alpha = 0.7, size = 3) +
  # trend line
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray40") +
  # label top players
  geom_text_repel(
    data = subset(player_power, power_score > 2),
    aes(label = player),
    size = 3,
    max.overlaps = 15
  ) +
  
  labs(
    title = "Power Score vs Home Run Production",
    subtitle = "Comparing power score to total HR output",
    x = "Power Score",
    y = "Total 'Home Runs'",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  )

# Final player rankings 
player_power_rank <- player_power %>%
  select(player, total, power_score, avg_exit_velo) %>%
  arrange(desc(power_score), desc(avg_exit_velo))

# Export ranked dataset
write.csv(player_power_r, "player_power_r.csv", row.names = FALSE)