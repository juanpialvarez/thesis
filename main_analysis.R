#set working directory
setwd("INSERT_WORKING_DIRECTORY")

#load packages
library(surveillance) #running twinstim
library(SpatialEpi) #convert lat and lon to km grid
library(rgdal) #retrieve EPSG codes
library(tigris) #shapefiles for counties and entire country
library(raster) #functions for processing shapefiles
library(readxl) #importing excel files
library(ggmap) #geocoding cities
library(censusapi) #population data
library(maps) #county processing
library(maptools) #county processing

#retrieve shapefiles of continental USA at the county and national level
usa_counties <- counties(state = state.name[!(state.name %in% c("Alaska", "Hawaii"))], cb = TRUE, resolution = "20m")
usa <- nation(resolution = "20m")

#use extent of usa_counties to crop usa
usa <- crop(usa, extent(usa_counties))

#re-project shapefiles to USA Contiguous Albers Equal Area Conic, or ESRI:102003
esri <- c("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")
usa_counties <- spTransform(usa_counties, CRS(esri))
usa <- spTransform(usa, CRS(esri))

#import PIRUS data
data <- read_xlsx("PIRUS_PUB_2018/PIRUS v3.2_public download.xlsx")

#only after 2005 (cutoff for social media data in PIRUS)
data <-data[which(format(as.Date(data$Date_Exposure), "%Y") >= 2005),]

#only far right
data <- data[which(data$Radicalization_Far_Right == 1),]

#remove entries without city data and in other countries
state_names <- state.name[-which(state.name == "Alaska" | state.name == "Hawaii")]
data <- data[which(data$Loc_Plot_City1 != -99 & grepl(paste(state_names, collapse = "|"), data$Loc_Plot_State1)),]

#store successful attempts
data$success <- rep(0, nrow(data))
data$success[which(data$Extent_Plot == 5)] <- 1

#no group as 0, group as 1
data$Group_Membership[which(data$Group_Membership %in% c(1, 2, 3))] <- 1

#missing data as 0
data$Anticp_Fatals_Targ[which(data$Anticp_Fatals_Targ %in% c(-99, -88))] <- 0

#unknown or missing as 0, playing a role as 1
data$Social_Media[which(data$Social_Media == -99 | data$Social_Media == -88)] <- 0
data$Social_Media[which(data$Social_Media == 1 | data$Social_Media == 2)] <- 1

#store locations as separate vector
locs <- c()
for(i in 1:length(data$Loc_Plot_City1)){
  locs <- c(locs, paste0(data$Loc_Plot_City1[i], ", ", data$Loc_Plot_State1[i], sep = ""))
}

#adjust city locations that throw errors when country boundaries are simplified during modeling (close to the coast)
locs[which(locs == "Quantico, Virginia")] <- c("Manassas, Virginia")
locs[which(locs == "Tampa, Florida")] <- c("Orlando, Florida")
locs[which(locs == "Tampa Bay, Florida")] <- c("Orlando, Florida")

#add locations to dataframe
data$locs <- locs

#geocode locations
register_google(key = "INSERT_KEY_HERE")
geo <- geocode(data$locs, output = "all")

#store geocoded locations as lat and lon coordinates
lat <- c()
lon <- c()
for(i in 1:length(geo)){
  lat <- c(lat, geo[[i]]$results[[1]]$geometry$location$lat)
  lon <- c(lon, geo[[i]]$results[[1]]$geometry$location$lng)
}
data$lon <- lon
data$lat <- lat

#prepare coordinates and dates for conversion
data$Date_Exposure <- as.numeric(as.Date(data$Date_Exposure)-min(as.Date("2005-01-01")))+1 #process data data so it is the number of days from the first date in the dataset
coords <- data.frame(data$lon, data$lat)
data_sp <- data.frame(data$Date_Exposure)
colnames(data_sp) <- c("time")

#convert points and predictors to spdf object
points_spdf <- SpatialPointsDataFrame(coords = coords, data = data_sp, proj4string = CRS("+proj=longlat +datum=WGS84"))
points_spdf <- spTransform(points_spdf, CRS(esri))
points_spdf$tile <- over(points_spdf, usa_counties)$GEOID
points_spdf$type <- rep(0, length(points_spdf))
points_spdf$eps.t <- rep(Inf, nrow(data))
points_spdf$eps.s <- rep(Inf, nrow(data))
points_spdf$group <- data$Group_Membership
points_spdf$fatal <- data$Anticp_Fatals_Targ
points_spdf$social_media <- data$Social_Media
points_spdf$internet <- data$Internet_Radicalization
points_spdf$success <- data$success

#load stgrid (contains dynamic endemic variables)
load("stgrid.RData")

#create epidata object
epidata <- as.epidataCS(events = points_spdf, W = usa, stgrid = stgrid, nCircle2Poly = 16)

#breaking location and time ties randomly by half of the minimum distance
max_temporal_untie <- min(dist(epidata$events$time)[dist(epidata$events$time) > 0])/2
max_spatial_untie <- min(dist(coordinates(epidata$events))[dist(coordinates(epidata$events)) > 0])/2

#store new epidata object with ties broken
epidata_untied <- untie(epidata, amount = list(t = max_temporal_untie, s = max_spatial_untie))



# PERMUTATION TEST --------------------------------------------------------

#basic permutation test
perm_test_model <- twinstim(endemic = ~ offset(log(popdensity)) + I(scale(start/365, scale = FALSE)) + poverty + vote + nonwhite + hs_ed + unempl + hate + crime, epidemic = ~ 1, siaf = siaf.step(knots = c(100, 200, 300), maxRange = 400), tiaf = tiaf.step(knots = c((1:3)*(365/4)), maxRange = 365), data = epidata_untied, cores = parallel::detectCores()-1, model = TRUE)
perm_test_results <- epitest(perm_test_model, data = epidata_untied, B = 1000)

#likelihood ratio test
epitest(perm_test_model, data = epidata_untied, method = "LRT")

#knox test
knox(dt = dist(epidata_untied$events$time), ds = dist(coordinates(epidata_untied$events)), eps.t = 365/2, eps.s = 100, simulate.p.value = FALSE)



# MODELING ----------------------------------------------------------------

#construct endemic formula list (for looping through every possible combination of predictors)
end_regressors <- c("poverty", "inequality", "gunown", "vote", "nonwhite", "hs_ed", "unempl", "hate", "crime")
regMat <- expand.grid(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE))
regMat <- regMat[-nrow(regMat),] #remove row with all falses
end_formula_list <- list()
for(i in 1:nrow(regMat)){
  end_formula_list[[i]] <- as.formula(paste("~ offset(log(popdensity)) + I(scale(start/365, scale = FALSE)) + ", paste(end_regressors[unlist(regMat[i,])], collapse = "+")))
}
end_formula_list[[i+1]] <- as.formula("~ offset(log(popdensity)) + I(scale(start/365, scale = FALSE))") #add row with only base model

#construct epidemic formula list (for looping through every possible combination of predictors)
epi_regressors <- c("success", "group", "fatal", "social_media")
regMat <- expand.grid(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE))
regMat <- regMat[-nrow(regMat),] #remove row with all falses
epi_formula_list <- list()
for(i in 1:nrow(regMat)){
  epi_formula_list[[i]] <- as.formula(paste("~", paste(epi_regressors[unlist(regMat[i,])], collapse = "+")))
}
epi_formula_list[[i+1]] <- as.formula("~0") #add row with only base model

#DETERMINE BEST ENDEMIC PREDICTORS

#construct base model with basic endemic terms and constant epidemic term
base_model <- twinstim(endemic = ~ offset(log(popdensity)) + I(scale(start/365, scale = FALSE)), epidemic = ~ 1, data = epidata_untied, siaf = siaf.step(knots = c(100, 200, 300), maxRange = 400), tiaf = tiaf.step(knots = c((1:3)*(180)), maxRange = 730), cores = parallel::detectCores()-1, model = FALSE)
model_results <- list()
aic <- list()

#store model and aic for every combination of endemic terms
for(i in 1:length(end_formula_list)){
  model_results[[i]] <- update(base_model, endemic = end_formula_list[[i]])
  aic[[i]] <- summary(model_results[[i]])$aic
  gc()
}

#combined data
end_output <- list(model_results, aic)

#check variables included in best fitting endemic model
summary(end_output[[1]][[which(unlist(end_output[[2]]) == min(unlist(end_output[[2]])))]])

#DETERMINE BEST EPIDEMIC PREDICTORS

#construct base model with best endemic terms and constant epidemic term
base_model <- twinstim(endemic = ~ offset(log(popdensity)) + I(scale(start/365, scale = FALSE)) + poverty + vote + nonwhite + hs_ed + unempl + hate + crime, epidemic = ~ 1, data = epidata_untied, siaf = siaf.step(knots = c(100, 200, 300), maxRange = 400), tiaf = tiaf.step(knots = c((1:3)*(180)), maxRange = 730), cores = parallel::detectCores()-1, model = FALSE)
model_results <- list()
aic <- list()

#store model and aic for every combination of endemic terms
for(i in 1:length(epi_formula_list)){
  model_results[[i]] <- update(base_model, epidemic = epi_formula_list[[i]])
  aic[[i]] <- summary(model_results[[i]])$aic
  gc()
}

#combine data
epi_output <- list(model_results, aic)
end_output <- list(model_results, aic)

#check results of best fitting model
summary(twinstim_output[[1]][[which(unlist(twinstim_output[[2]]) == min(unlist(twinstim_output[[2]])))]])



# SIMULATION (last six months) --------------------------------------------

#add row names to counties data (required for simulations to run)
row.names(usa_counties) <- usa_counties$GEOID

#set start time, end time, and number of simulations
start_time <- (as.numeric(as.POSIXct("2017-06-01"))-as.numeric(as.POSIXct("2005-01-01")))/(24*60*60)
end_time <- (as.numeric(as.POSIXct("2017-12-31"))-as.numeric(as.POSIXct("2005-01-01")))/(24*60*60)
nsim <- 1000

#silence warnings that would interrupt the loop
oldw <- getOption("warn")
options(warn = -1)

#add simulations to best_sim until the number of successful simulations is equal to nsim
best_sim <- list()
i <- 1
while(length(which(!is.na(best_sim))) < nsim){
  best_sim[[i]] <- tryCatch(simulate(best, t0 = start_time, T = end_time, data = epidata_untied, tiles = usa_counties, simplify = TRUE), error = function(e) NA)
  i <- i + 1
}

#reset warnings default
options(warn = oldw)


