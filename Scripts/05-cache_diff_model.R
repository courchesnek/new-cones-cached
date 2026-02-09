#load packages ------------
source("Scripts/00-packages.R")

#read in data -------------
midden_cones <- read.csv("Input/midden_cones.csv")
yearly_cone_index <- read.csv("Input/yearly_cone_index.csv")

#remove failure years -----------------
failure_years <- yearly_cone_index$year[yearly_cone_index$avg_cone_index < 0.6]
failure_years
#these years will be removed for ratio and mean calculations

midden_cones_filtered <- midden_cones %>%
  filter(!(year %in% failure_years))

#is the difference between males and females in cone caching significant across non-failure years? ---------
#standardize predictors
midden_cones_filtered$total_cones_sc <- as.numeric(scale(midden_cones_filtered$total_cones_tree))

#model - GAMMA
model <- glmmTMB(cache_size_new ~ sex + log_cache_size_old + (1 | year) + (1 | squirrel_id),
                 family = tweedie(link = "log"),  
                 data = midden_cones_filtered)

summary(model)

#residual plots
sim_res <- simulateResiduals(model)
plot(sim_res)

testOutliers(sim_res, type = "bootstrap") #no extreme outliers

#test overdispersion
DHARMa::testDispersion(sim_res, type = "DHARMa")
