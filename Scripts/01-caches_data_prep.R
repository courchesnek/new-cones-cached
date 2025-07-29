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

selected_grids <- c("CH", "KL", "SU", "JO", "BT")

#pull in tables
##the supplementary tables are not updated in the annual data cleanup, so squirrel_id values must be updated from the historic_squirrel_ids table
historic_ids<- tbl(con, "historic_squirrel_ids") %>% 
  dplyr::select(old_squirrel_id, new_squirrel_id) %>% 
  collect()

flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  #exclusions
  filter(gr %in% c("SU", "KL", "CH", "JO", "BT")) %>% 
  dplyr::select(squirrel_id, sex, byear) %>% 
  collect()

middencones <-tbl(con_suppl, "midden_cones") %>% 
  filter(squirrel_id !="UTS",
         grid %in% c("SU", "KL", "CH", "JO", "BT")) %>% #remove UTS squirrels from data
  collect() %>% 
  left_join(historic_ids, by=c("squirrel_id" = "old_squirrel_id")) %>% 
  mutate(squirrel_id = ifelse(is.na(new_squirrel_id), squirrel_id, new_squirrel_id),
         date = as.POSIXct(Date, format = '%Y-%m-%d %H:%M:%S'))

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
  slice(which.max(cache_size_total)) %>% #keeps the midden with more cones
  dplyr::select(year, grid, midden, sex, squirrel_id, cache_size_total, cache_size_new, cache_size_old) 

# add in cone crop --------------------------------------------------------
tree_cones <-tbl(con, "cones") %>%
  filter(Grid %in% selected_grids, Year>=1988) %>%
  collect()  %>%
  mutate(Year = as.numeric(Year), 
         NumNew = as.numeric(NumNew),
         total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01))) #according to Krebs et al. 2012

#mean cones per year
yearly_cones <- tree_cones %>%
  group_by(Year, Grid) %>% 
  dplyr::summarize(
            num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE), #average number of new cones per tree for each year and grid
            cone_index = mean(log(NumNew + 1), na.rm = TRUE), #average logarithm of the number of cones per tree for each year and grid
            total_cones = round(mean(total_cones, na.rm = TRUE), 3)) %>%
  rename(year = Year,
         grid = Grid)

yearly_cones <- yearly_cones %>%
  mutate(cone_index_t = ifelse(is.finite(cone_index), cone_index, NA)) %>%
  mutate(year_tp1 = year+1)

yearly_cones_temp <- yearly_cones %>%
  dplyr::select(year, year_tp1, cone_index_tm1=cone_index_t, grid)

yearly_cones <- left_join(yearly_cones, yearly_cones_temp, by=c("year" = "year_tp1", "grid"), relationship = "many-to-many") %>%
  dplyr::select(year, grid, num_trees, cone_counts, cone_index_t, cone_index_tm1, total_cones)

#save
write.csv(yearly_cones, "Input/yearly_cones.csv", row.names = FALSE)

##save on that is only total cone indices - use cone_index_t
yearly_cone_index <- yearly_cones %>%
  group_by(year) %>%
  summarize(
    avg_cone_index = mean(cone_index_t, na.rm = TRUE))

#save
write.csv(yearly_cone_index, "Input/yearly_cone_index.csv", row.names = FALSE)

#merge with midden_cones
midden_cones <- midden_cones %>%
  left_join(yearly_cones %>% dplyr::select(year, grid, total_cones), by = c("year", "grid"))

# data transformation - since non-normal distribution ---------------------
##log transformations - +1 because data contains zeros and cannot do log(0)
midden_cones$log_cache_size_new <- log(midden_cones$cache_size_new + 1)
midden_cones$log_total_cones <- log(midden_cones$total_cones + 1)

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
  dplyr::select(year, year_type, grid, squirrel_id, sex, cache_size_new, total_cones, log_cache_size_new, log_total_cones)

#save
write.csv(midden_cones, "Input/midden_cones.csv", row.names = FALSE)

# males cache exactly how much more? --------------------------------------
#scale the cache size by the total cones produced in each year
midden_cones$scaled_cache_size <- midden_cones$cache_size_new / midden_cones$total_cones

avg_scaled_cache <- midden_cones %>%
  group_by(sex) %>%
  summarise(mean_scaled_cache = mean(scaled_cache_size, na.rm = TRUE))

ratio_males_to_females <- avg_scaled_cache$mean_scaled_cache[avg_scaled_cache$sex == "M"] / 
  avg_scaled_cache$mean_scaled_cache[avg_scaled_cache$sex == "F"]

ratio_males_to_females
#across years (i.e. cone crops) males cache approximately 2.30 times as many new cones as females do. 
