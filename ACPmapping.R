# This code is meant to explore the ACP data visually to discover QC issues that need correction.
# The goal here is to produce lines from the observed bird point locations to reconstruct transect effort
# These lines can then be 'segmented' to produce spatially explicit small-scale sample units for modeling.
# This code is largely developmental and exploratory
# written by Erik Osnas, May, 2022
#load and map ACP
library(sf)
library(tidyverse)

#read and plot basic spatial layers
#design strata
acp <- st_read(dsn="Design_Strata/ACP_2007to2018_DesignStrata.shp") %>%
  st_transform(crs=3338)
plot(st_geometry(acp), col=factor(acp$STRATNAME))
#study area (not sure what this is or how different than above)
acp2 <- st_read(dsn="Design_Strata/ACP_2007to2019_StudyArea.shp") %>%
  st_transform(crs=3338)
plot(st_geometry(acp2)) #seem to be outer perimeter of design strata

sum(st_area(acp))
#58201582314 [m^2]
st_area(acp2)
#58201582314 [m^2]
# stupid! we should delete the 'study area' file; why do we keep these separate?
acp3 <- acp %>% st_union() %>% st_make_valid()
plot(st_geometry(acp3))
st_area(acp3) 
#58201582314 [m^2]
#acp2 is unecessary, delete
rm(acp2)
#All of above are originally in EPSG:3338, if left in 3338, 
# then area is the same for all of the above
# on reprojection, error in area calculations arise

#make bounding box sf object
bbox <- st_sf(st_as_sfc(st_bbox(acp)))

#read and plot transects, here just 2019 for example reference
trans1 <- st_read(dsn="Design_Transects/ACP_2019_Transects.shp") %>% 
#this is in EPGS:4269 = lat, long, but the length attributes 
#  seem to be calculated in 3338 --> transform
 st_transform(crs=3338)

ggplot(data=acp) + 
  geom_sf(aes(fill=STRATNAME)) + 
  geom_sf(data=trans1) +
  guides(fill=guide_legend(title="Strata")) + 
  labs(title=2019) + 
  coord_sf(datum = st_crs(3338))

ggplot(data=st_transform(acp, crs=st_crs(4326))) + 
  geom_sf(aes(fill=STRATNAME)) + 
  geom_sf(data=st_transform(trans1, crs=st_crs(4326))) +
  guides(fill=guide_legend(title="Strata")) + 
  labs(title=2019)

#add a leaflet plot
library(tmap)
tmap_mode("view")
tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
  tm_shape(trans1) + tm_lines() + 
  tm_basemap(server = "Esri.WorldImagery")
  
#add bird observation
csv_files = list.files(path = 'Data/', pattern = "csv$", full.names = TRUE)
# Read each csv file into a list
birds <- map(csv_files, read_csv, col_types=cols(Transect=col_double())) %>% 
  map_dfr(select, "Year", "Month", "Day", "Time", "Transect", 
          "Observer", "Species", "Num", "Obs_Type", "Code", "Lat", "Lon") %>%
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) %>%
  st_transform(crs=3338) %>%
  st_filter(bbox) %>% #get rid of the birds in Nigeria (lat/long = 0), only 14 observations
  filter(Code == 1) #remove any non-survey or special observations 

# ibp <- birds %>% filter(Obs_Type != "flkdrake", Num > 0,
#                         Obs_Type %in% c("single", "pair") | (Obs_Type == "open" & Num < 3))
# 
# flocks <- birds %>% filter(Obs_Type == "flkdrake" | (Obs_Type == "open" & Num >= 3))
# 
# Spp <- c("ARTE", "BRAN", "CCGO", "COEI", "GLGU", "GWFG", "JAEG", "KIEI", "LTDU", 
#          "NOPI", "PALO", "RTLO", "SAGU", "SNGO", "SPEI", "STEI", "SWAN", "YBLO") 
# lo <- c("PALO", "RTLO", "YBLO")
# ei <- c("COEI", "KIEI", "SPEI", "STEI")
# gu <- c("ARTE", "GLGU", "JAEG", "SAGU")
# ge <- c("BRAN", "CCGO", "GWFG", "SNGO", "SWAN")
# du <- c("AMWI", "LTDU", "NOPI", "RBME")
# 
#singles and pairs == indicated breeding birds
# tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
#   tm_shape(trans1) + tm_lines() +
#   tm_basemap(server = "Esri.WorldGrayCanvas")
# for(i in Spp){
# sp <- filter(ibp, Species %in% i) %>% st_as_sf(coords=c("Lon", "Lat"), crs=4326)
# tm <- tm + tm_shape(sp, name=i) + 
#   tm_dots(col=terrain.colors(length(Spp))[which(factor(Spp)==i)])
# }
# tm 
# 
#flocks?

#map year-specific observations and make track lines
Y = 2019
df <- filter(birds, Year==Y) %>%
  group_by(Transect)
ggplot(data=trans1, group=TRANSID) +
  geom_sf(col=trans1$TRANSID) +
  geom_sf(data=df, size=0.2)
# #leaflet
# tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
#   tm_shape(trans1, name="Design Trans.") + tm_lines() +
#   tm_text("TRANSID") +
#   tm_basemap(server = "Esri.WorldGrayCanvas") + 
#   tm_scale_bar()
# 
# for(Y in 2007:2019){
# sp <- filter(birds, Year == Y) %>% st_as_sf(coords=c("Lon", "Lat"), crs=4326)
# tm <- tm + tm_shape(sp, name=as.character(Y)) + 
#     tm_dots(col=terrain.colors(13)[which(2007:2019==Y)])
# }
# tm 
###################################################################
###################################################################
#make a linestring for each transect
#first test on 2016 data
# test0 <- filter(birds, Year==2016) %>% 
#   cbind(st_coordinates(.)) %>%
#   #distinct(X, Y) %>%
#   group_by(Transect) %>%
#   arrange(X, .by_group=TRUE) 
# 
# #make a linestring from points, first remove geometry, then rebuild
# test <- as.data.frame(test0) %>% 
#   select(-geometry) %>%
#   filter(Transect == 39) %>%
#   arrange(X, Y) %>%
#   select(X, Y) %>%
#   as.matrix() %>%
#   st_linestring() %>%
#   st_sfc(crs=3338) %>%
#   st_sf(geometry=.) %>%
#   mutate(Transect = 39)
# plot(st_geometry(test))
#that worked, now turn into function and apply across all transects

points2line <- function(x, Year=unique(x$Year), Transect=unique(x$Transect), crs=4326){
  #this function accepts sf object of points and returns an sf linestring
  # sf object define ONE linestring, not more than one!
  #accepted sf object should have an attribute for named Year and Transect, 
  # if not, supply as a parameter value
  linestring <- x %>% cbind(st_coordinates(.)) %>%
    as.data.frame() %>% 
    select(-geometry) %>%
    arrange(X, Y) %>%
    select(X, Y) %>%
    as.matrix() %>%
    st_linestring() %>%
    st_sfc(crs=4326) %>%
    st_sf(geometry=.) %>%
    mutate(Year = Year, Transect=Transect)
  return(linestring)
}

#test
x = birds %>% st_transform(crs=4326) %>%
  filter(Year == 2016, Transect == 39)
points2line(x=x)

#Now apply function across all transects
lines <- birds %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#plot them, filter through years and plot each year
filter(lines, Year==2019) %>%
  st_geometry() %>%
  plot()
# found some QC issues, plot 2010, 2013, 2014, 2015, 2016 2017 2018 2019 data and others
################################################################################
## Observation quality control process starts here
#cycle through years to discover QC issues and propose solutions, see below  
#plot in leaflet
Y = 2013
# Tran = 1453
# df <- filter(lines, Year==Y, Transect==Tran)
# bdf <- filter(birds, Year==Y, Transect==Tran) %>% mutate(Day=as.character(Day))
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% mutate(Day=as.character(Day))
tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() + 
  tm_text("Transect", size=2) + 
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") + 
  tm_basemap(server = "Esri.WorldGrayCanvas") + 
  tm_scale_bar() 
tm

#loop through years
# tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5)
# for(Y in 2007:2019){
#   df <- filter(lines, Year==Y)
#   bdf <- filter(birds, Year==Y)
#   tm <- tm + tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() + 
#     tm_text("Transect") + 
#   tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots() + 
#   tm_basemap(server = "Esri.WorldGrayCanvas") + 
#   tm_scale_bar() 
# }
# tm 
#hide layers
# %>% 
#   tmap_leaflet() %>%
#   leaflet::hideGroup(c(paste(2008:2019, "Flown Track"), paste(2008:2019, "Bird Obs")))

#QC issues with transect numbering, try sorting by time?


speed <- function(x){
 ##function to calculate speed based on position and time as recorded in data
 # used as a QC step to identify problematic points
 # according the HMW plausible speeds are 45 - 150 mph
 #x is a sf data frame with point geometry and a time variable for the time of recording for each point
 df <- x %>% group_by(Transect, Day, Observer) %>%
   slice(-n())
 df2 <- x %>% group_by(Transect, Day, Observer) %>%
   slice(-1)

 df3 <- st_distance(x=df, y=df2, by_element = TRUE)
 dTime <- df2$Time - df$Time
 return( as.vector( (60*60*df3) / (dTime*1000) ) )
}
# #apply to all data for one year, 2017
# x <- data.frame(Speed = as.vector(speed(x = bdf)/1.61) ) %>% #miles per hour
#   drop_na()
# hist(x$Speed[x$Speed < 300], xlim=c(-200, 300), breaks=100)
# summary(x$Speed[x$Speed < Inf])

################################
##QC birds data set
# structure of below:
## QC find: Year = XXXX, Transect = XX, Description = short English description. 
## Solution: English description of solution
## then tidyverse (or general R) code
birdsQC <- birds %>% cbind(st_coordinates(.)) %>%
  ## QC find: Year = 2010, Transect = 1453, Description = small number of observations (5) attributed to transect 1453 have lat/longs associated with transect 363. 363 seem to have been done later on 2010-19.
  ## Solution: delete five observations. These obs are very far west on 363, so delete based on transect number and long < 0.
  filter(!(Year == 2010 & Transect == 1453 & X < 0) ) %>%
  select(!c("X", "Y"))
## QC find: Year = 2013, Transect = 38 and 45, Description = 
#
#   trans <- st_read(dsn="Design_Transects/ACP_2013_Transects.shp") %>%
#   st_transform(crs=3338)
# plot(st_geometry(acp3))
# plot(st_geometry(filter(trans, TRANSID == 45)), add = TRUE)
# plot(st_geometry(filter(birds, Year == 2013 & Day == 10 & Observer == "HMW" & Transect == 45)),
#      add = TRUE)
# plot(st_geometry(filter(birds, Year == 2013 & Day == 10 & Observer == "WWL" & Transect == 45)), 
#      add = TRUE)
# plot(st_geometry(filter(trans, TRANSID == 38)), col="red", add = TRUE)
# 
# plot(st_geometry(acp3))
# plot(st_geometry(filter(trans, TRANSID == 38)), add = TRUE)
# plot(st_geometry(filter(birds, Year == 2013 & Day == 10 & Observer == "HMW" & Transect == 38)),
#      add = TRUE)
# plot(st_geometry(filter(birds, Year == 2013 & Day == 10 & Observer == "WWL" & Transect == 38)), 
#      add = TRUE)
# plot(st_geometry(filter(trans, TRANSID == 45)), col="red", add = TRUE)
# HMW seems to have misslabled transect 38 as 45 on day 10 and
# HMW misslabled transect 45 as 38 on day 10
## Solution: reverse transect 38 and 45 for HMW on day 10; need to create temporary df to keep results
df <- birdsQC %>% filter(Year == 2013 & Observer == "HMW" & Day == 10 & Transect == 38) %>% 
  mutate(Transect = 45)
df2 <- birdsQC %>% filter(Year == 2013 & Observer == "HMW" & Day == 10 & Transect == 45) %>% 
  mutate(Transect = 38)
birdsQC <- birdsQC %>% filter(!(Year == 2013 & Observer == "HMW" & Day == 10 & Transect %in%c(38, 45))) %>%
  bind_rows(df, df2)
          
#Did it work?
trans <- st_read(dsn="Design_Transects/ACP_2013_Transects.shp") %>%
  st_transform(crs=3338)
plot(st_geometry(acp3))
plot(st_geometry(filter(trans, TRANSID == 45)), add = TRUE)
df <- birdsQC
plot(st_geometry(filter(df, Year == 2013 & Day == 10 & Observer == "HMW" & Transect == 45)),
     add = TRUE)
plot(st_geometry(filter(df, Year == 2013 & Day == 10 & Observer == "WWL" & Transect == 45)),
     add = TRUE)
plot(st_geometry(filter(trans, TRANSID == 38)), col="red", add = TRUE)

plot(st_geometry(filter(df, Year == 2013 & Day == 10 & Observer == "HMW" & Transect == 38)),
     col="red", add = TRUE)
plot(st_geometry(filter(df, Year == 2013 & Day == 10 & Observer == "WWL" & Transect == 38)),
     col="red", add = TRUE)
# Yes!
# recompute lines
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#Plot seem to suggest it worked
# continue QC
#plots for QC
Y <- 2014
df <- filter(lines, Year==Y)
plot(st_geometry(df))
bdf <- birdsQC %>% filter(Year == Y) %>% mutate(Day=as.character(Day))
tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() + 
  tm_text("Transect", size=2) + 
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") + 
  #tm_basemap(server = "Esri.WorldImagery") + 
  tm_basemap(server = "Esri.WorldGrayCanvas")
tm
# QC find:  Year = 2014, Transect = 43, Description = 
#  It appears a section of transect 43 was sampled twice, once on day ==11 and again on day ==12.
#  It seems implied that search effort was stopped before "end transect" there was recorded or computer/GPS failure 
#  bdf <- filter(birdsQC, Year == 2014 & Transect ==43)
# Solution: renumber transects so that each segment is a unique number
birdsQC <- birdsQC %>% 
#  transect 43 on Day = 12 becomes 43.3
#  transect 43 on Day = 11 and time < 73597 (west side) becomes 43.1
#  transect 43 on Day = 11 and time >74000 (east side) becomes 43.2
  mutate(Transect = if_else(Year == 2014 & Transect == 43 & Day == "12", 43.3, Transect)) %>% 
  mutate(Transect = if_else(Year == 2014 & Transect == 43 & Day == "11" & Time < 73597, 43.1, Transect)) %>%
  mutate(Transect = if_else(Year == 2014 & Transect == 43 & Day == "11" & Time > 74000, 43.2, Transect))
# recompute lines
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#Plot seem to suggest it worked
# continue QC
#plots for QC
Y <- 2014
df <- filter(lines, Year==Y)
# #does calculating speed and displaying color of dots based on speed help QC?
# bdf <- birdsQC %>% filter(Year == Y) %>% mutate(Day=as.character(Day))
# x <- data.frame(Speed=speed(bdf)/1.61)
# sdf <- bdf %>% group_by(Transect, Day, Observer) %>%
#   slice(-n()) %>% bind_cols(x) %>% 
#   mutate("Speed Bin" = ifelse(Speed < 45, "too slow", ifelse(Speed > 150, "too fast","just right"))) %>%
#   filter(Speed < 45 | Speed > 150)
# tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
#   tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() + 
#   tm_text("Transect", size=2) + 
#   tm_shape(sdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Speed Bin", size = 0.1) + 
#   #tm_basemap(server = "Esri.WorldImagery") + 
#   tm_basemap(server = "Esri.WorldGrayCanvas")
# tm
# #Maybe, mostly IDs points of start or end with same location as birds obs. 
# QC find: Transect 35, Year = 2014, Descrption = Transect ended at ~65980, then started again
# QC soluton: assign segements
birdsQC <- birdsQC %>% 
  mutate(Transect = if_else(Year == 2014 & Transect == 35 & Time < 65990, 35.1, Transect)) %>%
  mutate(Transect = if_else(Year == 2014 & Transect == 35 & Time > 65990, 35.2, Transect))
# recompute lines
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#continue mapping transects
Y <- 2015
df <- filter(lines, Year==Y)
bdf <- birdsQC %>% filter(Year == Y) %>% mutate(Day=as.character(Day))
tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() + 
  tm_text("Transect", size=2) + 
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") + 
  #tm_basemap(server = "Esri.WorldImagery") + 
  tm_basemap(server = "Esri.WorldGrayCanvas")
tm

#QC find: Year 2015, Transect 24 and 39
#  Transect 14 has start stop within same day, multiple occasions; also transect 13


#segmenting transects
# source("trans2seg.R")
# #need to remove duplicate points (when multiple birds are seen at the same point)
# test2 <- as.data.frame(test0) %>% 
#   select(-geometry) %>%
#   #filter(Transect == 39) %>%
#   arrange(X, Y) %>%
#   select(X, Y) %>%
#   distinct() %>% #this removes duplicate points
#   as.matrix() %>%
#   st_linestring() %>%
#   st_sfc(crs=4326) %>%
#   st_sf(geometry=.) 
# #%>%
# #  mutate(Transect = 39)
# trans.seg <- trans2seg(test2, utm=6332)
# #NOTE: after/during segmentization need to id original vs. segmentized points and remove original points. 
# #hummm, this is too hard. how st_segmetize just adds points tot he linestring. I need to use
# # the observed point to re-create the track, then chuck it up. 