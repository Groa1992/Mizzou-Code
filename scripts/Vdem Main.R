# ---- Setup ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(vdemdata)
})
data("vdem", package = "vdemdata")

# ---- Helpers -------------------------------------------------------------
country_ts <- function(df, country, indicators = c("v2x_libdem","v2x_polyarchy",
                                                   "v2x_freexp","v2x_jucon"),
                       from = 1970) {
  df %>%
    filter(country_name == country, year >= from) %>%
    select(year, all_of(indicators)) %>%
    pivot_longer(-year, names_to = "indicator", values_to = "score")
}

plot_ts <- function(long_df, title, vlines = integer(0)) {
  ggplot(long_df, aes(year, score, color = indicator)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = vlines, linetype = "dashed") +
    labs(title = title, x = "Year", y = "Score (0–1)", color = "Indicator") +
    theme_minimal()
}

dir.create("figs", showWarnings = FALSE)

# ---- Paraguay: Liberal Democracy only -----------------------------------
vdem %>%
  filter(country_name == "Paraguay", year >= 1970) %>%
  ggplot(aes(year, v2x_libdem)) +
  geom_line() +
  geom_vline(xintercept = c(1989, 2012), linetype = "dashed") +
  labs(title = "Liberal Democracy Index (Paraguay, 1970–present)",
       x = "Year", y = "Score (0–1)")
ggsave("figs/paraguay_libdem_1970_present.png", dpi = 300, width = 8, height = 4.5)

# ---- Peru: all four indicators ------------------------------------------
per_long <- country_ts(vdem, "Peru")
plot_ts(per_long, "Democracy Indicators in Peru (V-Dem)")
ggsave("figs/peru_indicators.png", dpi = 300, width = 8, height = 4.5)

# ---- Türkiye: robust country label --------------------------------------
tur_key <- if ("Türkiye" %in% vdem$country_name) "Türkiye" else "Turkey"
tur_long <- country_ts(vdem, tur_key)
plot_ts(tur_long, "Democracy Indicators in Türkiye (V-Dem)")
ggsave("figs/turkiye_indicators.png", dpi = 300, width = 8, height = 4.5)

# ---- U.S. vs G7 average (liberal democracy) ------------------------------
g7 <- c("United States of America","Canada","France","Germany","Italy","Japan","United Kingdom")
g7_series <- vdem %>%
  filter(country_name %in% g7, year >= 1970) %>%
  group_by(year) %>%
  summarize(value = mean(v2x_libdem, na.rm = TRUE), .groups = "drop") %>%
  mutate(country_name = "G7 average")

us_series <- vdem %>%
  filter(country_name == "United States of America", year >= 1970) %>%
  transmute(year, country_name = "United States", value = v2x_libdem)

bind_rows(g7_series, us_series) %>%
  ggplot(aes(year, value, color = country_name)) +
  geom_line(linewidth = 1) +
  labs(title = "U.S. vs G7 Average: Liberal Democracy (1970–present)",
       x = "Year", y = "Score (0–1)", color = NULL)
ggsave("figs/us_vs_g7_libdem.png", dpi = 300, width = 8, height = 4.5)

