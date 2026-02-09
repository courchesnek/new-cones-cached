#load packages ------------
source("Scripts/00-packages.R")

#read in data ------------
midden_cones <- read.csv("Input/midden_cones.csv")
yearly_cone_index <- read.csv("Input/yearly_cone_index.csv")

#identify and remove cone failure years ---------
failure_years <- yearly_cone_index$year[yearly_cone_index$avg_cone_index < 0.6]
failure_years

midden_cones_filtered <- midden_cones %>%
  filter(!(year %in% failure_years))

#log-transform cached cones --------------
midden_cones_filtered <- midden_cones_filtered %>%
  mutate(log10_cache_size_new = log10(cache_size_new + 1))

#summarise by sex and year --------------
cache_summary_log10 <- midden_cones_filtered %>%
  group_by(year, sex) %>%
  summarise(
    n    = sum(!is.na(log10_cache_size_new)),
    mean = mean(log10_cache_size_new, na.rm = TRUE),
    sd   = sd(log10_cache_size_new,   na.rm = TRUE),
    se   = sd / sqrt(n),
    # 95% CI half-width (mean ± t × SE) calculated on the log10 scale
    ci   = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_)) %>%
  ungroup()

#plot summarised raw data --------------
#source theme
source("Scripts/00-plot_theme.R")

caching_log <- ggplot(cache_summary_log10, aes(x = year, y = mean, colour = sex, group = sex)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.0) +
  scale_colour_manual(values = c("M" = "#88CCEE", "F" = "#CC6677"),
                      labels = c("Male", "Female"),
                      breaks = c("M", "F")) +
  scale_x_continuous(
    breaks = seq(min(cache_summary_log10$year, na.rm = TRUE),
                 max(cache_summary_log10$year, na.rm = TRUE),
                 by = 1),
    labels = ifelse(seq(min(cache_summary_log10$year, na.rm = TRUE),
                        max(cache_summary_log10$year, na.rm = TRUE), by = 1) %% 2 == 0,
                    seq(min(cache_summary_log10$year, na.rm = TRUE),
                        max(cache_summary_log10$year, na.rm = TRUE), by = 1), ""),
    expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(
    breaks = 0:5,
    labels = c("1", "10", "100", "1,000", "10,000", "100,000"),
    expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Year",
    y = expression("Number of Cones Cached (log"[10]*")"),
    colour = "Sex") +
  theme_thesis() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -2),
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.6),
    panel.grid.minor.x = element_blank())

caching_log

#save
ggsave("Output/cones_cached_new_log.jpeg", plot = caching_log, width = 12, height = 8)

#sample sizes
length(unique(midden_cones_filtered$squirrel_id[midden_cones_filtered$sex == "F"]))
length(unique(midden_cones_filtered$squirrel_id[midden_cones_filtered$sex == "M"]))
length(unique(midden_cones_filtered$squirrel_id))
