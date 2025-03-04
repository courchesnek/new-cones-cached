#load packages
source("Scripts/00-packages.R")

#read in data
midden_cones <- read.csv("Input/midden_cones.csv")
yearly_cones <- read.csv("Input/yearly_cones.csv")

#average tree cones/year -----------------------------------------------------
yearly_cones <- yearly_cones %>%
  group_by(year) %>%
  summarize(total_cones = mean(total_cones, na.rm = TRUE), .groups = "drop") %>%
  filter(year >= 2008)

#plot check
ggplot(yearly_cones, aes(x = year, y = total_cones)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "red", size = 3) +
  labs(x = "Year", 
       y = "Total cones per tree") +
  theme_minimal()

#average new cones cached by sex by year ------------------------------------
new_cones <- midden_cones %>%
  group_by(year, sex) %>%
  summarise(new_cones = mean(cache_size_new, 3, na.rm = TRUE), 
            new_cones_log = mean(log_cache_size_new, 3, na.rm = TRUE),
            .groups = "drop")

# plot --------------------------------------------------------------------
#define mast years
mast_years <- c(2010, 2014, 2019, 2022)

highlight_df <- data.frame(
  xmin = mast_years - 0.2,
  xmax = mast_years + 0.2,
  ymin = 0,
  ymax = Inf)

newcones_years <- ggplot() +
  geom_rect(data = highlight_df, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 1, position = position_identity(), inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.7) +
  geom_line(data = new_cones, 
            aes(x = year, y = new_cones_log, color = sex), 
            size = 1, position = position_dodge(width = 0.1)) +
  geom_point(data = new_cones, 
             aes(x = year, y = new_cones_log, color = sex), 
             size = 3, position = position_dodge(width = 0.1)) +
  scale_color_manual(values = c("M" = "#99CCFF", "F" = "#FF99CC"), 
                     name = "Sex", 
                     labels = c("M" = "Male", "F" = "Female")) +
  labs(x = "Year", 
       y = "Number of new cones cached (log-scaled)", 
       title = "Average Number of New Cones Cached by Sex") +
  theme_classic() +   
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks.length = unit(0.25, "cm"),     
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5))

newcones_years

#save
ggsave("Output/newcones_years.jpeg", newcones_years, width = 8, height = 6)

# plot without mast years -------------------------------------------------
new_cones_no_mast <- new_cones %>%
  filter(!year %in% mast_years)

newcones_nomasts <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.7) +
  geom_line(data = new_cones_no_mast, 
            aes(x = year, y = new_cones, color = sex), 
            size = 1, position = position_dodge(width = 0.1)) +
  geom_point(data = new_cones_no_mast, 
             aes(x = year, y = new_cones, color = sex), 
             size = 3, position = position_dodge(width = 0.1)) +
  scale_color_manual(values = c("M" = "#99CCFF", "F" = "#FF99CC"), 
                     name = "Sex", 
                     labels = c("M" = "Male", "F" = "Female")) +
  labs(x = "Year", 
       y = "Number of new cones cached", 
       title = "Average Number of New Cones Cached by Sex") +
  theme_classic() +
  coord_cartesian(ylim = c(0, NA), clip = "on") +   
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks.length = unit(0.25, "cm"),     
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5))

newcones_nomasts

#save
ggsave("Output/newcones_nomasts.jpeg", newcones_nomasts, width = 8, height = 6)
