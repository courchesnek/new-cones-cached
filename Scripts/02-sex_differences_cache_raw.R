#load packages ------------
source("Scripts/00-packages.R")

#read in data ------------
midden_cones <- read.csv("Input/midden_cones.csv")

#summarise by sex and year --------------
#but first, log scale 10
midden_cones <- midden_cones %>%
  mutate(log10_cache_size_new = log10(cache_size_new + 1))

cache_summary_log10 <- midden_cones %>%
  group_by(year, sex) %>%
  summarise(
    n    = sum(!is.na(log10_cache_size_new)),
    mean = mean(log10_cache_size_new, na.rm = TRUE),
    sd   = sd(log10_cache_size_new,   na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_)) %>%
  ungroup()

#plot summarised raw data --------------
caching_log <- ggplot(cache_summary_log10, aes(x = year, y = mean, colour = sex, group = sex)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.0) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2) +
  scale_colour_manual(values = c("F" = "#E31A1C", "M" = "#1F78B4"),
                      labels = c("Female", "Male")) +
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
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.5),
        panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18, margin = margin(t = 13)),
        axis.title.y = element_text(size = 18, margin = margin(r = 13, l = 5)),
        legend.margin = margin(t = -5, b = 10),
        plot.margin = margin(t = 10, r = 20, b = 0, l = 10))

caching_log

#save
ggsave("Output/cones_cached_new_log.png", plot = caching_log, width = 12, height = 8)
