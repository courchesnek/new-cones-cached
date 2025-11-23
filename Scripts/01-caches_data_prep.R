#load packages
source("Scripts/00-packages.R")

#connection to KRSP databases
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

con_suppl <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                           dbname ="krsp_suppl",
                           username = Sys.getenv("krsp_user"),
                           password = Sys.getenv("krsp_password"))

selected_grids <- c("CH", "KL", "SU")

#pull in tables
historic_ids<- tbl(con, "historic_squirrel_ids") %>% 
  dplyr::select(old_squirrel_id, new_squirrel_id) %>% 
  collect()

flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  #exclusions
  filter(gr %in% c("SU", "KL", "CH")) %>% 
  dplyr::select(squirrel_id, sex, byear) %>% 
  collect()

middencones <-tbl(con_suppl, "midden_cones") %>% 
  filter(squirrel_id !="UTS",
         grid %in% c("SU", "KL", "CH")) %>% #remove UTS squirrels from data
  collect() %>% 
  left_join(historic_ids, by=c("squirrel_id" = "old_squirrel_id")) %>% 
  mutate(squirrel_id = ifelse(is.na(new_squirrel_id), squirrel_id, new_squirrel_id),
         date = as.POSIXct(Date, format = '%Y-%m-%d %H:%M:%S'))

# fix squirrel IDs --------------------------------------------------------
middencones <- middencones %>% 
  mutate(squirrel_id = ifelse(squirrel_id == 19851, 19537, squirrel_id),
         squirrel_id = ifelse(squirrel_id == 19911, 11895, squirrel_id)) %>%
  dplyr::select(-id, -locx, -locy, -taglft, -tagrt, -Date, -obs, -experiment, -date, -new_squirrel_id)

# fix missing sex values --------------------------------------------------
middencones <- middencones %>%
  left_join(flastall %>% 
            dplyr::select(squirrel_id, sex), 
            by = "squirrel_id", 
            suffix = c("", "_flast")) %>%
  mutate(sex = coalesce(na_if(sex,""), sex_flast)) %>%
  dplyr::select(-sex_flast) %>%
  filter(!is.na(sex))

# fix no. quads and quad area and remove NAs ---------------------------------------------
middencones <- middencones %>%
  mutate(no_quad = case_when(
    year >= 2007 & year <= 2009 ~ 16,
    year >= 2010 & year <= 2023 ~ 8,
    year == 2024 ~ 4))

middencones <- middencones %>%
  mutate(area_quad = case_when(
    year == 2007 ~ 0.49,
    year >= 2008 & year <= 2010 ~ 0.36,
    year >= 2011 & year <= 2024 ~ 0.09))

#remove NA width/length & remove 2007 data (didn't count old vs new)
middencones <- middencones %>%
  drop_na(width, length) %>%
  filter(year != 2007)

# calculate cache sizes ---------------------------------------------------
midden_cones <- middencones %>%   
  mutate(total_new_2019 = total_newclosed + total_newopen,
         total_new = coalesce(total_new, total_new_2019),
         total_cones = total_old + total_new,
         total_cones2 = total_old + total_newclosed,
         cache_size_total = (total_cones / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_total2 = (total_cones2 / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_new = (total_new / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_new_closed = (total_newclosed / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_new_open = (total_newopen / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_old = (total_old / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         squirrel_id = as.numeric(as.character(squirrel_id))) %>%  #needed to match variable types. the as.character is needed otherwise the squirrel_ids get recoded as 1...n
  group_by(squirrel_id, year) %>% #this line of code and the one below removes cases where a squirrel owns more than one midden
  slice(which.max(cache_size_total)) %>% #keeps the midden with more cones so one midden per squirrel
  dplyr::select(year, grid, midden, sex, squirrel_id, cache_size_total, cache_size_new, cache_size_old) 

# add in cone crop --------------------------------------------------------
tree_cones <-tbl(con, "cones") %>%
  filter(Grid %in% selected_grids, Year>=1988) %>%
  collect()  %>%
  mutate(Year = as.numeric(Year), 
         NumNew = as.numeric(NumNew),
         #convert per-tree new cone counts to total cone production using LaMontagne et al. 2005
         total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01)))

#summaries for each year x grid
yearly_cones <- tree_cones %>%
  group_by(Year, Grid) %>% 
  dplyr::summarize(
            num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE), #average number of new cones per tree for each year and grid
            cone_index = mean(log(NumNew + 1), na.rm = TRUE), #average logarithm of the number of cones per tree for each year and grid
            total_cones = round(mean(total_cones, na.rm = TRUE), 3)) %>%
  rename(year = Year,
         grid = Grid)

#organize cone indices for current year ("t") and previous year ("t-1" = tm1)
yearly_cones <- yearly_cones %>%
  mutate(cone_index_t = ifelse(is.finite(cone_index), cone_index, NA)) %>%
  mutate(year_tp1 = year +1 ) #the following year = ("t+1" = tp1)

#create a temporary table that stores each year's cone index (t) but renames it as (t–1)
#these values will be lagged forward so they attach to the next year
yearly_cones_temp <- yearly_cones %>%
  dplyr::select(year, #current year
                year_tp1, #next year (t+1)
                cone_index_tm1=cone_index_t, # # rename current year's index as "t–1" to join to next year
                grid)

#join previous-year cone index (t–1) onto current year (t)
#for a row with year == 2010, we join cone_index_tm1 from the row where year == 2009.
#this ensures each year contains the previous year's cone index
yearly_cones <- left_join(yearly_cones, yearly_cones_temp, by=c("year" = "year_tp1", "grid"), relationship = "many-to-many") %>%
  dplyr::select(year, grid, num_trees, cone_counts, cone_index_t, cone_index_tm1, total_cones)

#save
write.csv(yearly_cones, "Input/yearly_cones.csv", row.names = FALSE)

##save one that is only total cone indices - use cone_index_t
yearly_cone_index <- yearly_cones %>%
  group_by(year) %>%
  summarize(
    avg_cone_index = mean(cone_index_t, na.rm = TRUE))

#save
write.csv(yearly_cone_index, "Input/yearly_cone_index.csv", row.names = FALSE)

#merge with midden_cones
midden_cones <- midden_cones %>%
  left_join(yearly_cones %>% dplyr::select(year, grid, total_cones_tree = total_cones), by = c("year", "grid"))

# data transformation - since non-normal distribution ---------------------
##log transformations - +1 because data contains zeros and cannot do log(0)
midden_cones$log_cache_size_new <- log(midden_cones$cache_size_new + 1)
midden_cones$log_total_cones_tree <- log(midden_cones$total_cones_tree + 1)

#add year type column -------------------------------------------------------
##define mast years
mast_years <- c(1993, 1998, 2005, 2010, 2014, 2019, 2022)

#add a column for year_type
midden_cones <- midden_cones %>%
  mutate(
    year = as.numeric(year), #extract year from date
    year_type = case_when(
      year %in% mast_years ~ "mast",
      year %in% (mast_years + 1) ~ "post-mast",
      TRUE ~ "non-mast"))

#reorder columns
midden_cones <- midden_cones %>%
  dplyr::select(year, year_type, grid, squirrel_id, sex, cache_size_total, cache_size_new, total_cones_tree, log_cache_size_new, log_total_cones_tree)

#save
write.csv(midden_cones, "Input/midden_cones.csv", row.names = FALSE)

# Males cache how many more new cones than females? --------------------------------------
# Compute yearly male/female mean new cone cached
yearly_means <- midden_cones %>%
  group_by(year, sex) %>%
  summarise(mean_cache_new = mean(cache_size_new, na.rm = TRUE),
            .groups = "drop")

#add yearly_cone_index to yearly_means
yearly_means <- yearly_means %>%
  left_join(yearly_cone_index, by = "year")

#identify failure years -------------------------------------------------------------------
trees_per_territory <- 1033 * 0.34 
trees_per_territory

yearly_cones_territory <- yearly_cones %>%
  group_by(year) %>%
  summarise(
    mean_index   = mean(cone_index_t, na.rm = TRUE),
    sd_index     = sd(cone_index_t, na.rm = TRUE),
    mean_cones_tree = mean(total_cones, na.rm = TRUE),   # per tree estimate
    sd_cones_tree   = sd(total_cones, na.rm = TRUE),
    n_grids      = n(),
    # NEW: crude estimate of cones per territory
    mean_cones_territory = mean(total_cones, na.rm = TRUE) * trees_per_territory,
    sd_cones_territory   = sd(total_cones, na.rm = TRUE) * trees_per_territory) %>%
  arrange(year)

yearly_cones_territory

post_mast <- yearly_cones_territory %>%
  filter(year %in% c(1994, 1999, 2006, 2011, 2015, 2020, 2023))
#-------------------- FINAL DECISION: remove years when cone index < 0.6 --------------------

#remove failure years
failure_years <- yearly_cone_index$year[yearly_cone_index$avg_cone_index < 0.6]
failure_years
#these years will be removed for ratio and mean calculations

yearly_means_filtered <- yearly_means %>%
  filter(!(year %in% failure_years))

# Compute year-weighted absolute means per sex (each year contributes equally)
absolute_means <- yearly_means_filtered %>%
  group_by(sex) %>%
  summarise(
    mean_per_year = mean(mean_cache_new, na.rm = TRUE),
    sd_per_year   = sd(mean_cache_new, na.rm = TRUE),
    n_years       = n(),
    se_per_year   = sd_per_year / sqrt(n_years),
    lower_CI      = mean_per_year - qt(0.975, df = n_years - 1) * se_per_year,
    upper_CI      = mean_per_year + qt(0.975, df = n_years - 1) * se_per_year,
    .groups = "drop")

absolute_means

# Compute year-weighted male:female ratio (one ratio per year → average)
ratio_per_year <- yearly_means_filtered %>%
  pivot_wider(names_from = sex, values_from = mean_cache_new) %>%
  mutate(MF_ratio = M / F)

equal_weight_ratio <- mean(ratio_per_year$MF_ratio, na.rm = TRUE)
equal_weight_ratio

# Energetic conversion of cache difference --------------------------------
#convert per-sex yearly means into energetic equivalents
#based on numbers from Fletcher et al. 2010 and 1kcal = 4.184 kJ
kj_conversion <- 0.65 * 4.184   # 1 cone = 0.65 kcal = 2.7196 kJ

energy_means <- absolute_means %>%
  mutate(kJ_equivalent = mean_per_year * kj_conversion)

energy_means

#difference: male minus female
energy_diff <- energy_means$kJ_equivalent[energy_means$sex == "M"] -
  energy_means$kJ_equivalent[energy_means$sex == "F"]

energy_diff

# what's the average total cache size across all squirrels? ----------
avg_total_cache_size <- midden_cones %>%
  filter(cache_size_total > 1) %>%
  ungroup() %>%
  summarise(
    mean_total_cache = mean(cache_size_total, na.rm = TRUE),
    median_total_cache = median(cache_size_total, na.rm = TRUE),
    sd_total_cache = sd(cache_size_total, na.rm = TRUE),
    min_total_cache = min(cache_size_total, na.rm = TRUE),
    max_total_cache = max(cache_size_total, na.rm = TRUE),
    n_records = n())

avg_total_cache_size
