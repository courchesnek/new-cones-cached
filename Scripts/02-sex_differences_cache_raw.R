#load packages ------------
source("Scripts/00-packages.R")

#read in data ------------
midden_cones <- read.csv("Input/midden_cones.csv")

#summarise by sex and year --------------
cache_summary <- midden_cones %>%
  group_by(year, sex) %>%
  summarise(
    n    = sum(!is.na(cache_size_new)),
    mean = mean(cache_size_new, na.rm = TRUE),
    sd   = sd(cache_size_new,   na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_)) %>%
  ungroup()

#plot summarised raw data --------------
caching <- ggplot(cache_summary, aes(x = year, y = mean, colour = sex, group = sex)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.0) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2) +
  scale_colour_manual(values = c("F" = "#E31A1C", "M" = "#1F78B4"),
                      labels = c("Female", "Male")) +
  scale_x_continuous(
    breaks = seq(min(cache_summary$year, na.rm = TRUE),
                 max(cache_summary$year, na.rm = TRUE),
                 by = 1),
    labels = ifelse(seq(min(cache_summary$year, na.rm = TRUE),
                        max(cache_summary$year, na.rm = TRUE), by = 1) %% 2 == 0,
                    seq(min(cache_summary$year, na.rm = TRUE),
                        max(cache_summary$year, na.rm = TRUE), by = 1), ""),
    expand = expansion(mult = c(0.01, 0.01))) +
  coord_cartesian(ylim = c(-1000, 50000)) +
  scale_y_continuous(
    limits = c(0, NA),                      
    expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Year",
    y = "Total Number of New Cones Cached",
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
        legend.margin = margin(t = -5, b = 10))

caching

#save
ggsave("Output/cones_cached_new.png", plot = caching, width = 12, height = 8)
