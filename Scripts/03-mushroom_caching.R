#load packages
source("Scripts/00-packages.R")

#connection to KRSP databases
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in behaviour table
behaviour <- tbl(con,"behaviour") %>%
  collect()

#filter for selected grids
caching <- behaviour %>%
  collect() %>%
  dplyr::select(id, behaviour, date, detail, grid, mode, squirrel_id, time, locx, locy) %>%
  mutate(year = year(ymd(date))) %>%
  filter(behaviour == 8,  #caching observations
         mode %in% c(1,3), #cas obs or focals
         grid %in% c("KL", "SU", "CH", "BT") | (grid == "JO" & year >= 2013)) %>% #keep control grids and JO post-food-add (food-add = 2006-2012)
  na.omit()

#we still need the sex of the squirrels here so let's connect to the flastall (first_last_all contains first last records of squirrels and is really handy for this type of stuff)... 
# ...pull squirrel_id and sex, then link that to the feeding table
squirrel_sex <- tbl(con,"flastall") %>%
  collect() %>%
  dplyr::select(squirrel_id, sex)

#join squirrel sex info to feeding obs
caching <- left_join(caching, squirrel_sex, by = "squirrel_id") %>%
  filter(is.na(sex) == FALSE)

#double check if any NAs in sex column
length(caching$sex[is.na(caching$sex) == TRUE])

#filter out hole digging and details in comments
caching_clean <- caching %>%
  filter(!detail %in% c(0, 6)) %>%
  mutate(
    type = case_when(
      detail == 1 ~ "Cone clipping",
      detail == 2 ~ "Travel with mushroom",
      detail == 3 ~ "Travel with cone",
      detail == 4 ~ "Cache mushroom",
      detail == 5 ~ "Cache cone",
      TRUE        ~ NA_character_),
    type = factor(type, levels = c(
      "Cone clipping",
      "Travel with cone",
      "Travel with mushroom",
      "Cache mushroom",
      "Cache cone"))) %>%
  na.omit()

# Do females cache more mushrooms than males? -----------------------------
# compute year×sex×type proportions
prop_data <- caching_clean %>%  
  filter(year >= 2000, year != 2024) %>%
  group_by(year, sex, type) %>%
  summarize(n = n(), .groups="drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# plot proportions
ggplot(prop_data, aes(x = factor(year), y = prop, fill = type)) +
  geom_col() +
  facet_wrap(~ sex, ncol = 1, labeller = as_labeller(c(F = "Female", M = "Male"))) +
  scale_y_continuous(labels = percent_format(1)) +
  scale_fill_brewer("Caching behaviour", palette = "Set2") +
  labs(
    x     = "Year",
    y     = "Proportion of caching behaviour events",
    title = "Proportion of caching behaviours by year, and sex")  +
  theme_minimal(base_size = 17) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.grid = element_blank(),
        axis.text.x = element_text(hjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t=10), face = "bold"),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5, margin = margin(b=20)),
        plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
        legend.position = "bottom",
        legend.box.margin = margin(t = -20, r = 0, b = 0, l = 0))











