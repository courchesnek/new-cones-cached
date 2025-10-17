#load packages ------------
source("Scripts/00-packages.R")

#read in data ------------
midden_cones <- read.csv("Input/midden_cones.csv")

#standardize cone availability & change sex and year into factors -----
midden_cones <- midden_cones %>%
  mutate(
    year = factor(year),
    sex = factor(sex),
    total_cones_sc = scale(total_cones))

#model cones new cached ----------
mod_cache <- glmmTMB(cache_size_new ~ sex + year + (1 | squirrel_id),
  data = midden_cones,
  family = gaussian)

summary(mod_cache)

#generate predictions ------------
pred_cache <- ggpredict(
  mod_cache,
  terms = c("year", "sex"))

# clamp negative predictions and CIs to 0
pred_cache$predicted[pred_cache$predicted < 0] <- 0
pred_cache$conf.low[pred_cache$conf.low < 0] <- 0

#plot predictions ------------
caching <- ggplot(pred_cache, aes(x = x, y = predicted, colour = group, group = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_colour_manual(values = c("F" = "#E31A1C", "M" = "#1F78B4"),
                      labels = c("Female", "Male")) +
  coord_cartesian(ylim = c(-1000, 29000)) +
  scale_y_continuous(
    limits = c(0, NA),                      
    expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Year",
    y = "Predicted Number of New Cones Cached",
    colour = "Sex") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 1.0),
        panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
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





