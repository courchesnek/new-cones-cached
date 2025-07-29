#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

con_suppl <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                           dbname ="krsp_suppl",
                           username = Sys.getenv("krsp_user"),
                           password = Sys.getenv("krsp_password"))

# trapping years by grid --------------------------------------------------
# 1) pull in trapping records; dbatrapping = 1984-2012 & trapping = 2013-2023
dbatrapping <- tbl(con,"dbatrapping") %>%
  collect()

trapping <- tbl(con,"trapping") %>%
  collect()

# 2) fix dbatrapping
#remove unnecessary columns and NAs
dbatrapping <- dbatrapping %>%
  dplyr::select(squirrel_id, gr, date, locX, locY, Sex, BrStatus) %>%
  na.omit()

dbatrapping_fixed <- dbatrapping


dbatrapping_fixed <- dbatrapping_fixed %>%
  rename(locx = locX, 
         locy = locY,
         sex = Sex,
         rep_con = BrStatus)

# 3) fix trapping
trapping <- trapping %>%
  dplyr::select(squirrel_id, gr, date, locx, locy, sex, rep_con) %>%
  na.omit()

# 4) merge all years together 
alltrapping <- bind_rows(dbatrapping_fixed, trapping) %>%
  rename(grid = gr)

# 5) oldest and most recent trapping years per grid
alltrapping %>%
  # make sure date is a Date class
  mutate(date = as.Date(date)) %>%  
  filter(grid %in% c("KL", "SU", "CH", "JO", "BT")) %>%
  group_by(grid) %>%
  summarise(
    first_trap   = min(date, na.rm = TRUE),
    last_trap    = max(date, na.rm = TRUE),
    start_year   = year(first_trap),
    end_year     = year(last_trap)) %>%
  arrange(grid)

# midden cone count years by grid --------------------------------------------------
# 1) pull in midden cone count records
midden_cones <- read.csv("Input/midden_cones.csv")

# 2) oldest and most recent count years per grid
midden_cones %>%
  filter(grid %in% c("KL", "SU", "CH", "JO", "BT")) %>%
  group_by(grid) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE)) %>%
  arrange(grid)

# tree cone count years by grid --------------------------------------------------
# 1) pull in tree cone count records
tree_cones <- read.csv("Input/yearly_cones.csv")

# 2) oldest and most recent count years per grid
tree_cones %>%
  filter(grid %in% c("KL", "SU", "CH", "JO", "BT")) %>%
  group_by(grid) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE)) %>%
  arrange(grid)

# census years by grid --------------------------------------------------
# 1) pull in census records
census <- read.csv("Input/census_clean.csv")

# 2) oldest and most recent census years per grid
census %>%
  mutate(census_date = as.Date(census_date)) %>%  
  filter(gr %in% c("KL", "SU", "CH", "JO", "BT")) %>%
  group_by(gr) %>%
  summarise(
    first_year = min(census_date, na.rm = TRUE),
    last_year  = max(census_date, na.rm = TRUE)) %>%
  arrange(gr)