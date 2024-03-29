#This file contains functions to estimate and map density of bird observation on the ACP.
# It is generalized from those used on the YKD. 
# The main difference is that for the YKD, I used the design transect as a measure of effort. 
# Here I use the bird observations to reconstruct effort of the flight path. 
# This is needed because the plane often wonders from the design transect and 
#  the track files of the plane have been lost or are otherwise not available. 
# The main inputs are: (1) point location of bird observations and 
#   (2) linestrings that connect the birds observation. 
# These input are produce elsewhere in the code file ACPmapping2023.R, where 
# extensive QC takes place. 
#
#The main tasks are to (1) read in survey area polygon, track lines and bird obs, 
#  (2) apply a grid over the survey area, and segment the track lines based on this grid, 
#  (3) associate the observations with each segment and sum up as a response in a GLMM, 
#  (4) estimate a density surface using a GAM or other model. 
#
#hope to make this a function, but for now just try sketch out code
#Code is based on that for the YKD in ~/YKD_Coastal_mapping/mapDensity.R
#  In that file there are 2 main functions: get_data() and map_aerial(), and wrapper 
#  that applies them (and a third that is used to select sub areas to model)
#however, those functions were not generalized to other survey data sets and 
# they work on the design transect not the flight path
#
################################################################################
#functions:
##first write a function to return selected area
select_area <- function(area = NA, select = "all"){
  if ( length(select) > 1 & length(select) > length(row.names(area)) ){
    stop("Length of selected strata is > number of strata.")}
  if ( length(select) == 1 ) {
    if ( select == "all" | is.na(select) ){
      area <- area %>%
        st_union() %>%
        st_as_sf()} 
    if (is.numeric(select)) {
      area <- area[select,] %>%
        st_union() %>%
        st_as_sf()
    }
  } else {
    if( is.numeric(select)) {
      area <- area[select,] %>%
        st_union() %>%
        st_as_sf()
    }
  }
  return(area)
}
#####
#function to associate birds observation to transect segments
# then adds effort and covariate data if it exists
# center points of the segments is used as the spatial location
get_data <- function(x = NA, y = NA, area = NA, Spp = NA, grid = NA, 
                     buff = 1000, covs = NULL){
  #x is df of bird observation, geographic coordinates must be named "Lon" and "Lat"
  #  and be in geographic coordinates: crs = 4326
  #y is the transect line segmented using make_segments function, 
  #  must have an id for each segment named "Sample.Label"
  #area is polygon of surveyed area
  #Spp is Species to filter and associate
  #grid is the survey area grid
  #buff is a distance to buffer the survey area and clip observation outside this distance
  #covs is an optional df of covariates for each segment, not implemented
  #Must only supply one year at a time
  #
  #Returns a data frame not sf object
  
  library(dplyr)
  library(sf)

  Year <- unique(x$Year)
  area <- st_transform(area, crs = st_crs(grid))
  #find center points of the segments
  cen <- y %>%
    st_transform(crs = st_crs(grid)) %>%
    st_centroid() %>%
    #st_join(area, join=st_nearest_feature) %>%
    select(Sample.Label, Segment.Label, Length, Area)
  
  #Need to duplicate segments for each observer in x
  Observer <- unique(x$Observer)
  cen2 <- mutate(cen, Observer = Observer[1])
  if( length(Observer) > 1){
    for(i in 2:length(Observer)){
      cen2 <- mutate(cen, Observer = Observer[i]) %>%
        rbind(cen2)
    }
  }
  
  obs <- x %>%
    filter(Species == Spp) %>%
    st_as_sf(coords = c("Lon", "Lat"), crs=st_crs(4326)) %>%
    st_transform(crs=st_crs(grid)) %>%
    st_filter(st_buffer(area, dist = buff)) %>% #filter out observations out of (buffered) area
    st_join(y, join=st_nearest_feature) %>% # join segments and obs
    select(Year, Month, Day, Time, Observer, Num, Obs_Type, Sample.Label, 
           Segment.Label, Length, Area) %>%
    #mutate(Count = ifelse(Obs_Type == "pair", 2*Num, Num)) %>% #transform to "total birds"
    filter(Obs_Type %in% c("single", "pair")) %>% #retain just the singles ("indicated pairs") and observed pairs
    group_by(Segment.Label, Observer) %>%
    #summarize(Count = sum(Count)) %>% #if type is "total birds", condition on type not implemented
    summarize(Count = sum(Num)) %>% 
    right_join(st_drop_geometry(y)) %>% #join with effort data, right join to retain zeros
    #mutate(Year = Year, Observer = Observer) %>%
    st_drop_geometry() %>%
    right_join(cen2) %>% #add center points of segments, retain Observers
    mutate(Count = replace(Count, is.na(Count), 0),  #replace NAs with zeros
           Year = Year) %>%
    arrange(Segment.Label) %>% 
    relocate(Year) %>% 
    relocate(Segment.Label, .after=Count) %>%
    ungroup() %>%
    st_as_sf(sf_column_name = "x", crs = st_crs(y)) %>%
    mutate(Length = units::drop_units(Length), Area = units::drop_units(Area)) %>%
    mutate(Length = Length/1000, Area = Area/(1000*1000), 
           logArea = log(Area)) #change units and add logArea offset

  return( cbind( st_drop_geometry(obs), st_coordinates(obs) ) )
}
#####
#predict and plot from GAM fit object
map_GAM <- function(gamfit = NA, grid = NA, design = NA, Obs = "HMW", Year = 2023,
                    exclude.term = "s(Observer)", cv = FALSE, Spp = NULL){
  library(mgcv)
  library(sf)
  library(tidyverse)
  # grid_position <- st_centroid(grid) %>%
  #   st_join(design, join=st_nearest_feature) %>%
  #   st_drop_geometry() %>%
  #   select(Sample.Label, STRATNAME)
  newdat <- st_centroid(grid) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    mutate(Observer = Obs, logArea = 0, Year = Year) 
  # %>%
  #   cbind(grid_position)
  preds <- predict(gamfit, newdata = newdat, type = "response", se.fit = TRUE,
                   exclude = exclude.term, newdata.guaranteed = TRUE)
  
  plotdat <- cbind(preds, grid)
  p <- ggplot(data = plotdat) + geom_sf(aes(fill=fit), col = NA) +
    #geom_sf(data = design[-trim,], fill = NA, alpha = 1, col = "white")  +
    scale_fill_viridis_c(name = "Expected \n density") +
    #breaks = my_breaks, labels = my_breaks) +
    labs(title = paste0(Spp, ", ", Year))
  
  #print(p)
  #CV
  plotdat <- mutate(plotdat, CV = se.fit/fit)
  if(cv == TRUE){
  p2 <- ggplot(data = plotdat) + geom_sf(aes(fill=CV), col = NA) +
    scale_fill_viridis_c(name = "CV") + labs(title = paste0(Spp, ", ", Year))
  }
  #print(cv)
  if(cv == FALSE){
  p2 <- ggplot(data = plotdat) + geom_sf(aes(fill=se.fit), col = NA) +
    scale_fill_viridis_c(name = "SE") + labs(title = paste0(Spp, ", ", Year))
  }
  #print(se)
  return(list(plots = list(p, p2), preds = plotdat))
}
#####
##function to apply GAM
#Not implemented
# map_aerial <- function(obs = NA, lines = NA, SPP = NA, Nback = 10, area = NA, 
#                        select = NA, cellsize = 2000, w = 200){
#   library(tidyverse)
#   library(sf)
#   library(mgcv)
#   tdesign <- select_area(area, select = select)
#   #ggplot(tdesign) + geom_sf()
#   grid <- st_intersection(tdesign, st_make_grid(x=tdesign, cellsize = cellsize)) %>%
#     mutate(Sample.Label = row.names(.), Area = st_area(.))
#   
#   #create a vector of Years in reverse order
#   years <- rev(unique(obs$Year))
#   #filter out years
#   obs <- filter(obs, Year >= max(years) - Nback)
#   years <- rev(unique(obs$Year))
#   df <- c()
#   for(i in 1:length(years)){ # loop through selected years
#     ## make a year-specific transect segement file
#     dat <- filter(obs, Year == years[1])
#     trans <- filter(lines, Year == years[i]) %>%
#       st_transform(crs=st_crs(grid))
#     seg <- st_intersection(grid, trans) %>%
#       mutate(LENGTH = st_length(.)) %>%
#       group_by(Sample.Label) %>%
#       summarise(LENGTH = sum(LENGTH)) %>%
#       mutate(Area = w*LENGTH)
#     #make covariates
#     covs <- seg %>% st_centroid() %>%
#       st_join(area, join=st_nearest_feature) %>%
#       select(Sample.Label, STRATNAME)
#     #combine and filter species 
#     df[[i]] <- get_data(obs = dat, SPP = SPP, grid = grid, 
#                         design = tdesign, seg = seg, covs = covs)
#   }
#   #format for mgcv
#   df <- map_df(df, bind_cols) %>% 
#     cbind(as.data.frame(st_drop_geometry(obs)), st_coordinates(obs)) %>%
#     mutate(logArea = log(Area/(1000*1000)), Observer = factor(Observer),
#            STRATNAME = factor(STRATNAME), fYear = factor(Year))
#   
#   fit0 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)),
#               offset = logArea, family = tw, method = "REML", data = df)
#   fit1 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5))+s(Observer, bs="re"),
#               offset = logArea, family = tw, method = "REML", data = df)
#   fit2 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5))+s(Year, k = length(unique(df$Year)) - 1),
#               offset = logArea, family = tw, method = "REML", data = df)
#   fit3 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5))+s(Year, k = length(unique(df$Year)) - 1) + 
#                 s(Observer, bs="re"),
#               offset = logArea, family = tw, method = "REML", data = df)
#   # fit4 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5))+s(fYear, bs = "re", k = length(unique(allobs$Year)) - 1) + 
#   #               s(Observer, bs="re"),
#   #             offset = logArea, family = tw, method = "REML", data = allobs)
#   # fit4 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5))+s(Year, k = length(unique(allobs$Year)) - 1) + 
#   #               s(Observer, bs="re"), select = TRUE, 
#   #             offset = logArea, family = tw, method = "REML", data = allobs)
#   # fit4 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 10) + Observer + 
#   #               ti(X, Y, Year, bs = "ds", k = c(20, 20, 10), m=c(1,.5)),
#   #             offset = logArea, family = tw, method = "REML", data = allobs)
#   aic <- AIC(fit0, fit1, fit2, fit3)
#   # aic
#   fit <- get(rownames(aic)[which(aic$AIC == min(aic$AIC))])
#   # summary(fit)
#   # gam.check(fit)
#   #Predict and plot
#   map <- map_GAM(fit = fit, grid = grid, design = area, SPP = SPP,
#                  exclude.term = c("s(Year, k = length(unique(allobs$Year)) - 1)", 
#                                   "s(Observer, bs='re')"))
#   return(list(data = df, aic = aic, fit = fit, map = map$plot, mapdata = map$preds))
# }
#####
# write a function to calculate population total and SE over the modeled area
map_total <- function(gamfit = NA, grid = NA, Obs = "HMW", Year = 2023, 
                      Nsamples = 1000, exclude.term = "s(Observer)"){
  library(tidyverse)
  library(mgcv)
  library(sf)
  
  newdata <- st_centroid(grid) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    mutate(Observer = Obs, Area = units::drop_units(grid$Grid.Area), Year = Year) 
  
  post <- matrix(0, Nsamples, length(unique(newdata$Year)))
  
  Xp <- predict(gamfit, type="lpmatrix", newdata=newdata, exclude = exclude.term, 
                newdata.guaranteed=TRUE)
  #sample from parameter posterior
  b <- rmvn(Nsamples, coef(gamfit), vcov(gamfit))
  for(j in 1:Nsamples ){
    p <- exp(Xp%*%b[j,])*as.vector(newdata$Area) #replicate of prediction at all points
    post[j,] <- sum(p)
      #multiple years and observers not impememnted now, uncomment to implement
      # cbind(newdata, p) %>% 
      # group_by(Year, Observer) %>% 
      # summarize( Total = sum(p) ) %>%
      # ungroup() %>%
      # group_by(Year) %>%
      # summarise(Total = mean(Total)) %>%
      # select(Total) %>% unlist()
  }
  return(post)
}
# write a function to calculate population total and SE over the modeled area 
#  for each year in input data
#newdata must be a grid over which prediction are computed and 
#  must contain a variable named "Area" that gives the area of each grid cell in km^2
#  the prediction grid need not be the same (cell size) as used to fit model
#  newdata must also have at least one name observer, even if the term is excluded. 
map_total2 <- function(gamfit = NA, newdata = NULL, 
                      Nsamples = 1000, exclude.term = "s(Observer)"){
  library(tidyverse)
  library(mgcv)
  library(sf)
  
  # newdata <- st_centroid(grid) %>%
  #   st_coordinates() %>%
  #   as.data.frame() %>%
  #   mutate(Observer = Obs, Area = units::drop_units(grid$Grid.Area), Year = Year) 
  
  post <- matrix(0, Nsamples, length(unique(newdata$Year)) * length(unique(newdata$Observer)))
  
  Xp <- predict(gamfit, type="lpmatrix", newdata=newdata, exclude = exclude.term, 
                newdata.guaranteed=TRUE)
  #sample from parameter posterior
  b <- rmvn(Nsamples, coef(gamfit), vcov(gamfit))
  for(j in 1:Nsamples ){
    p <- exp(Xp%*%b[j,]) * as.vector(newdata$Area) #replicate of prediction at all points
    post[j,] <- cbind(newdata, p) %>% 
      group_by(Year, Observer) %>% 
      summarize( Total = sum(p) ) %>%
      ungroup() %>%
      select(Total) %>% 
      unlist()
  }
  return(post)
}
##modify map_total2 so that it has the correct number of year-observer combos
map_total3 <- function(gamfit = NA, newdata = NULL, yearobs = 1, 
                       Nsamples = 1000, exclude.term = NULL){
  library(tidyverse)
  library(mgcv)
  library(sf)
  
  # newdata <- st_centroid(grid) %>%
  #   st_coordinates() %>%
  #   as.data.frame() %>%
  #   mutate(Observer = Obs, Area = units::drop_units(grid$Grid.Area), Year = Year) 
  
  post <- matrix(0, Nsamples, yearobs)
  
  Xp <- predict(gamfit, type="lpmatrix", newdata=newdata, exclude = exclude.term, 
                newdata.guaranteed=TRUE)
  #sample from parameter posterior
  b <- rmvn(Nsamples, coef(gamfit), vcov(gamfit))
  for(j in 1:Nsamples ){
    p <- exp(Xp%*%b[j,]) * as.vector(newdata$Area) #replicate of prediction at all points
    post[j,] <- cbind(newdata, p) %>% 
      group_by(Year, Observer) %>% 
      summarize( Total = sum(p) ) %>%
      ungroup() %>%
      select(Total) %>% 
      unlist()
  }
  return(post)
}
#####
# write a function to accept lines and make segments from a defined grid
#works on only one year at a time
make_segments <- function(x = NA, y = NA, w = 200){
  library(dplyr)
  library(sf)
  # x is a sf polygon grid, y is an sf linestring line transect; w is transect width
  y <- st_transform(y, crs = st_crs(x))
  st_intersection(x, y) %>%
    #grids with > 1 transect path through them become mutlilinestrings with the same ID.
    #need to cast to linestrings and give unique IDs.
    #seg2 <- st_cast(seg, "LINESTRING") -- only keeps the first linestring! 
    #I don't understand why below works but see
    #https://gis.stackexchange.com/questions/280771/r-sfst-castlinestring-keeping-first-linestring-only-warning
    
    st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
    mutate(Length = st_length(.), Area = w*Length) %>%
    group_by(Sample.Label) %>%
    mutate(id = factor(row_number())) %>%
    mutate(Segment.Label = paste0(Sample.Label, ".", id)) %>%
    select(Sample.Label, Segment.Label, Length, Area)
}
#test it
#make_segments(x= testgrid, y = filter(lines, Year == 2023))
# seems to work
################################################################################
# ## Test and debug functions
# library(tidyverse)
# library(sf)
# #read in lines and birds data
# lines <- st_read(dsn = "Data/ACP_2023/analysis_output/Lines_Obs_2.gpkg")
# birds <- read_csv(file = "Data/ACP_2023/analysis_output/Bird_QC_Obs.seat.stratum.csv", 
#                   col_types = "iiidcddcccdccdd") %>%
#   # dplyr::mutate(
#   #   # replace text and parenthesis
#   #   geometry = stringr::str_replace(geometry, 'c\\(', ''),
#   #   geometry = stringr::str_replace(geometry, '\\)', '')
#   # ) %>%
#   # # separate into lat and lon columns
#   # tidyr::separate(geometry, into=c('Lon', 'Lat'), sep=', ') %>%
#   st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
# 
# acp <- st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#   
# 
# ggplot(data = acp) + geom_sf() + geom_sf_text(aes(label = Stratum))
# test<- select_area(area = acp, select = "all")
# plot(st_geometry(test))
# #works!
# #####

# speed <- function(x){
#  ##function to calculate speed based on position and time as recorded in data
#  # used as a QC step to identify problematic points
#  # according the HMW plausible speeds are 45 - 150 mph
#  #x is a sf data frame with point geometry and a time variable for the time of recording for each point
#  df <- x %>% group_by(Transect, Day, Observer) %>%
#    slice(-n())
#  df2 <- x %>% group_by(Transect, Day, Observer) %>%
#    slice(-1)
# 
#  df3 <- st_distance(x=df, y=df2, by_element = TRUE)
#  dTime <- df2$Time - df$Time
#  return( as.vector( (60*60*df3) / (dTime*1000) ) )
# }
# #apply to all data for one year, 2017
# x <- data.frame(Speed = as.vector(speed(x = bdf)/1.61) ) %>% #miles per hour
#   drop_na()
# hist(x$Speed[x$Speed < 300], xlim=c(-200, 300), breaks=100)
# summary(x$Speed[x$Speed < Inf])
