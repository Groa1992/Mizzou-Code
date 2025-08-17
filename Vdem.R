# One-time installs
install.packages("remotes")
install.packages("tidyverse", dependencies = TRUE)
remotes::install_github("vdeminstitute/vdemdata")
library(tidyverse)  # loads dplyr, ggplot2, tidyr, etc.

# Each session
library(vdemdata) 
library(tidyverse) 

# Load dataset
data("vdem", package = "vdemdata")

# Quick peek
glimpse(vdem)
summary(vdem$year)

install.packages("gitcreds")
library(gitcreds)

# See what token is being used
gitcreds_get("https://github.com")

# Delete it so remotes stops trying a broken PAT
gitcreds_delete("https://github.com")

# Step 1 

library(tidyverse)

# Ensure vdem is loaded
data("vdem", package = "vdemdata")

# Filter USA data
usa <- vdem %>%
  filter(country_name == "United States of America") %>%
  select(country_name, year,
         v2x_libdem,      # Liberal Democracy (0–1)
         v2x_polyarchy,   # Electoral Democracy (0–1)
         v2x_freexp,      # Freedom of Expression (0–1)
         v2x_jucon)       # Judicial Independence (0–1)

# Years available
range(usa$year, na.rm = TRUE)

# Most recent values
usa %>%
  slice_max(year, n = 1)

# Paraguay

# Filter Paraguay data
pry <- vdem %>%
  filter(country_name == "Paraguay") %>%
  select(country_name, year,
         v2x_libdem,      # Liberal Democracy (0–1)
         v2x_polyarchy,   # Electoral Democracy (0–1)
         v2x_freexp,      # Freedom of Expression (0–1)
         v2x_jucon)       # Judicial Independence (0–1)

# Years available
range(pry$year, na.rm = TRUE)

# Most recent values
pry %>%
  slice_max(year, n = 1)

# Plotting for Paraguay 

# Reshape to long format for easier plotting
pry_long <- pry %>%
  select(-country_name) %>%   # remove country_name so only numbers remain
  pivot_longer(cols = -year,
               names_to = "indicator",
               values_to = "score")

# Plot
ggplot(pry_long, aes(x = year, y = score, color = indicator)) +
  geom_line(size = 1) +
  labs(title = "Democracy Indicators in Paraguay (V-Dem)",
       x = "Year",
       y = "Score (0–1)",
       color = "Indicator") +
  theme_minimal()

# Data for Peru 

# --- Filter for Peru and select indicators ---
per <- vdem %>%
  filter(country_name == "Peru") %>%
  select(country_name, year,
         v2x_libdem,      # Liberal Democracy (0–1)
         v2x_polyarchy,   # Electoral Democracy (0–1)
         v2x_freexp,      # Freedom of Expression (0–1)
         v2x_jucon)       # Judicial Independence (0–1)

# Quick checks
range(per$year, na.rm = TRUE)
per %>% slice_max(year, n = 1)

# --- Reshape to long format (drop the character column first) ---
per_long <- per %>%
  select(-country_name) %>%
  pivot_longer(cols = -year,
               names_to = "indicator",
               values_to = "score")

# --- Plot time series ---
ggplot(per_long, aes(x = year, y = score, color = indicator)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Democracy Indicators in Peru (V-Dem)",
    x = "Year",
    y = "Score (0–1)",
    color = "Indicator"
  ) +
  theme_minimal()

# Highest / lowest Liberal Democracy Index year
usa %>% slice_max(v2x_libdem, n = 1) %>% select(year, v2x_libdem)
usa %>% slice_min(v2x_libdem, n = 1) %>% select(year, v2x_libdem)
# Period averages
usa %>%
  mutate(period = if_else(year <= 2000, "1970–2000", "2001–present")) %>%
  group_by(period) %>%
  summarize(avg_libdem = mean(v2x_libdem, na.rm = TRUE))

# Turkey 

# --- Filter for Turkey / Türkiye and select indicators ---
tur <- vdem %>%
  filter(country_name %in% c("Turkey", "Türkiye")) %>%
  select(country_name, year,
         v2x_libdem,      # Liberal Democracy (0–1)
         v2x_polyarchy,   # Electoral Democracy (0–1)
         v2x_freexp,      # Freedom of Expression (0–1)
         v2x_jucon)       # Judicial Independence (0–1)

# Quick checks
range(tur$year, na.rm = TRUE)
tur %>% slice_max(year, n = 1)

# --- Reshape to long format (drop the character column first) ---
tur_long <- tur %>%
  select(-country_name) %>%
  pivot_longer(cols = -year,
               names_to = "indicator",
               values_to = "score")

# --- Plot time series ---
ggplot(tur_long, aes(x = year, y = score, color = indicator)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Democracy Indicators in Turkey (V-Dem)",
    x = "Year",
    y = "Score (0–1)",
    color = "Indicator"
  ) +
  theme_minimal()

# Step 3 

# 3A. Liberal Democracy since 1970 (with 2016 marker)
usa %>%
  filter(year >= 1970) %>%
  ggplot(aes(year, v2x_libdem)) +
  geom_line() +
  geom_vline(xintercept = 2016, linetype = "dashed") +
  labs(title = "Liberal Democracy Index (U.S., 1970–present)",
       x = "Year", y = "Score (0–1)")

# 3B. Electoral Democracy since 1970
usa %>%
  filter(year >= 1970) %>%
  ggplot(aes(year, v2x_polyarchy)) +
  geom_line() +
  labs(title = "Electoral Democracy Index (U.S., 1970–present)",
       x = "Year", y = "Score (0–1)")
# 3C. Multiple indicators in one plot
usa %>%
  filter(year >= 1970) %>%
  pivot_longer(cols = c(v2x_libdem, v2x_polyarchy, v2x_freexp),
               names_to = "indicator", values_to = "score") %>%
  ggplot(aes(year, score, color = indicator)) +
  geom_line() +
  labs(title = "U.S. Democracy Indicators (1970–present)",
       x = "Year", y = "Score (0–1)", color = "Indicator")
# 3D. Faceted view (small multiples)
usa %>%
  filter(year >= 1970) %>%
  pivot_longer(cols = c(v2x_libdem, v2x_polyarchy, v2x_freexp, v2x_jucon),
               names_to = "indicator", values_to = "score") %>%
  ggplot(aes(year, score)) +
  geom_line() + ylim(0.25, 1) +
  facet_wrap(~ indicator, ncol = 4, scales = "free_y") +
  labs(title = "U.S. Democracy Components (faceted)",
       x = "Year", y = "Score")

# Step 4

usa_change <- usa %>%
  arrange(year) %>%
  mutate(d_libdem = v2x_libdem - lag(v2x_libdem))
# Largest 1-year decline
usa_change %>% slice_min(d_libdem, n = 1) %>% select(year, d_libdem)
# Plot year-to-year change (bars)
usa_change %>%
  filter(!is.na(d_libdem), year >= 1970) %>%
  ggplot(aes(year, d_libdem)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Year-to-Year Change in U.S. Liberal Democracy",
       x = "Year", y = "∆ v2x_libdem")

# Step 5 

g7 <- c("United States of America","Canada","France","Germany",
        "Italy","Japan","United Kingdom")
g7_series <- vdem %>%
  filter(country_name %in% g7, year >= 1970) %>%
  group_by(year) %>%
  summarize(g7_mean_libdem = mean(v2x_libdem, na.rm = TRUE), .groups = "drop") %>%
  mutate(country_name = "G7 average")
us_series <- usa %>%
  filter(year >= 1970) %>%
  transmute(year, country_name = "United States", value = v2x_libdem)
plot_data <- g7_series %>%
  transmute(year, country_name, value = g7_mean_libdem) %>%
  bind_rows(us_series)
plot_data %>%
  ggplot(aes(year, value, color = country_name)) +
  geom_line() +
  labs(title = "U.S. vs G7 Average: Liberal Democracy (1970–present)",
       x = "Year", y = "Score (0–1)", color = NULL)

