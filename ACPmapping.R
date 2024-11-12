# This code is meant to explore the ACP data visually to discover QC issues that 
#   need correction.
# The goal here is to produce lines from the observed bird point locations to 
#   reconstruct transect effort.
# These lines can then be 'segmented' to produce spatially explicit small-scale 
#   sample units for modeling.
# This code produces a Quality-control version of the ACP design strata and transects, 
#   bird observations, and flight path.
# Also added to the bird observations is the design transect, seat, and stratum so 
#   that design-based estimates can be produced directly from the birds observations, 
#   with sample effort (transect length) obtained from the design files. 
# written by Erik Osnas, May, 2022
# modified October 2023 to use 2023 data from RDR. Some major data updates occurred in 22 and 23
#finalized approximately in November 2023. 
# This code below produces the data input files for spatial mapping of bird densities, 
#   in map_density.R
#
# The code below should not be run, it is only used to produce a usable QC data set.
#   Many interactive leaflet maps are produced to explore data issues. If you do run it, 
#   Clear viewer occasionally.
# Code is only provided to document QC process and provide a repeatable source for 
#   the produced data sets.  
#
# Update February 2024, Emily Silverman found a transect numbering problem in 2018
#   Many observations have incorrect Transect numbers, Code, and Stratum. 
#   These are all in the 10-02 area and associated with Heather. 
#   Code section added to fix this lines 1034 on
#
#  Also in Feb. 2024, found error in Code and Month for WWL in 2016, changes, see below
#
#  March 2024: found typo in observer initials while writing metadata data dictionary.
#    For 2007 to 2009, RMD and RDM is one observer. Should change all to RDM for 
#    Robert D. MacDonald. This will affect the bird observation data file but not the 
#    flight lines. I will re-create all data assets, however, to reflect the commit 
#    version associated with this revision. All models and model output (Maps) will 
#    need to be redone. This should have a small effect on the map and trends. Note 
#    that all results presented at ABC, SeaDuck, and Ducks9 are affected. As well 
#    as the first version of the SPEI map distributed to USFWS ES in February 2024.
#    Change code at end of file, no need to run all. 
#
#  November 2024: Add 2024 data to output data sets birds, lines
#
#load and map ACP
library(sf)
library(tidyverse)
library(tmap)

tmap_mode("view")

#read and plot basic spatial layers
#design strata
acp <- st_read(dsn="Data/ACP_2023/source_data/ACP_DesignStrata.gpkg") %>%
  st_transform(crs=3338) 
#note that in above ACP polygons, area attribute only match if calculation is 
#done in 3338, not original crs, which appears to be 4269
plot(st_geometry(acp), col=factor(acp$STRATNAME))

#make bounding box sf object
bbox <- st_sf(st_as_sfc(st_bbox(acp)))

#read and plot transects, here just 2019 for example reference
tran_layers <- st_layers(dsn = "Data/ACP_2023/source_data/ACP_DesignTrans.gpkg")
trans <- list()
for(i in 1:length(tran_layers$name)){
  trans[[i]] <- st_read(dsn="Data/ACP_2023/source_data/ACP_DesignTrans.gpkg", 
                        layer = tran_layers$name[i]) 
}
names(trans) <- tran_layers$name
lapply(trans, st_crs)
# #looks like they are all in 3338
# a <- lapply(trans, function(x){sum(st_length(x))})
# b <- lapply(trans, function(x){sum(x$LENGTH)})
# df <- data.frame(a = unlist(a), b = unlist(b))
#lengths are not all equal but above code does seem to work?

ggplot(data=acp) + 
  geom_sf(aes(fill=STRATNAME)) + 
  geom_sf(data=trans[["ACP_2023_Transects"]]) +
  guides(fill=guide_legend(title="Strata")) + 
  labs(title=2023) + 
  coord_sf(datum = st_crs(3338))

plot(st_geometry(st_cast(trans[["ACP_2023_Transects"]], "POINT")), pch = ".")
#zoom in 
# tmap_mode("view")
# tm_shape(acp) + 
#   tm_polygons(alpha = 0.5, col = "STRATNAME", lwd = 4,  border.col = "darkgray") +
#   tm_shape(trans[["ACP_2023_Transects"]]) + tm_lines(lwd = 2) + 
#   tm_basemap(server = "Esri.WorldImagery")

#add bird observation
# Needed to delete ...left and right seat files for 2010. Chuck wrote these to RDR and are redundant;
# I just deleted them from my copy of the data from RDR
#
csv_files = list.files(path = "Data/ACP_2023/final_data", pattern = "csv$", full.names = TRUE)
birds <- map(csv_files, read_csv, col_types = cols(Year = "i", Month = "i",
            Day = "i", Transect = "i", Lat = "d", Lon = "d", Time = "d", 
            Num = "i", Code = "c", Seat = "c", Observer = "c", Flight_Dir = "c",
            Filename = "_", Species = "c", Obs_Type = "c", Stratum = "c",
            Segment = "_", A_G_Name = "_", Wind_Dir = "_", Wind_Vel = "_",
            Sky = "_", Delay = "_", Behavior = "_", Distance = "_", Notes = "c"))

# map(birds, problems)
# #birds[[17]] has problems, starting at row 1155
# View(birds[[17]])

birds17 <- read_csv(file = csv_files[[17]], col_select = c("Transect"), 
                    col_types = c("c")) %>%
  mutate(Transect = as.numeric(str_sub(Transect, 1, 2)))
birds[[17]]$Transect <- birds17$Transect #replace transect names
birds2 <- map_dfr(birds, select, "Year", "Month", "Day", "Time", "Transect", 
                  "Observer", "Seat", "Species", "Num", "Obs_Type", "Code", 
                  "Lat", "Lon") %>%
  filter(Lat != 0) %>% #only 13 observations
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) 
# %>%
# actually, I think we should retain dead birds for the line building, 
#  and remove them later before modelling
  # filter(Code != "DEAD") #remove any dead bird, but retaining all other obs types;
  #                        #non-survey or special observations
birds <- birds2
rm(birds2)

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
###################################################################
###################################################################
source("points2line.R")
# #make a linestring for each transect
# points2line <- function(x, Year=unique(x$Year), Transect=unique(x$Transect), crs=4326){
#   #this function accepts sf object of points and returns an sf linestring
#   # sf object define ONE linestring, not more than one!
#   #accepted sf object should have an attribute for named Year and Transect, 
#   # if not, supply as a parameter value
#   linestring <- x %>% cbind(st_coordinates(.)) %>%
#     as.data.frame() %>% 
#     select(-geometry) %>%
#     arrange(X, Y) %>%
#     select(X, Y) %>%
#     as.matrix() %>%
#     st_linestring() %>%
#     st_sfc(crs=4326) %>%
#     st_sf(geometry=.) %>%
#     mutate(Year = Year, Transect = Transect)
#   return(linestring)
# }

#test
# x = birds %>% st_transform(crs=4326) %>%
#   filter(Year == 2016, Transect == 39)
# points2line(x=x)

#Now apply function across all transects
lines <- birds %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
################################################################################
## Observation quality control process starts here
##QC birds data set
# structure of below:
## QC find: Year = XXXX, Transect = XX, Description = short English description. 
## Solution: English description of solution
## then tidyverse (or general R) code
################################
#cycle through years to discover QC issues and propose solutions, see below 
################################
#plot them, filter through years and plot each year
years <- unique(birds$Year)
years <- years[!is.na(years)]
################################
##QC Find: Year = 2010: there is 1 NA in Year for the birds data !
#   Also, see above in the process of finding this, I discovered that there were duplicates in the Obs data
#df <- filter(birds, is.na(Year))
# A tibble: 1 × 11
# Year Month   Day   Time Transect Observer Species   Num Obs_Type Code 
# * <int> <int> <int>  <dbl>    <dbl> <chr>    <chr>   <int> <chr>    <chr>
#   1    NA     6    22 51656.       38 KSB      COEI        4 open     1   
# SOlution: KSB was only in 2010, so Year must = 2010, replace!
birds <- mutate(birds, Year = if_else(is.na(Year), 2010, Year))
################################
# Always plot your data!
#plot all years, visually inspect lines:
for(i in 1:length(trans)){
filter(lines, Year==years[i]) %>%
  st_geometry() %>%
  plot(main = years[i])
}
#try tmap to zoom into issues:
# y = 2010
# df <- filter(lines, Year==y)
# df2 <- filter(birds, Year==y)
# tm_shape(acp) + 
#   tm_polygons(alpha = 0.5, col = "STRATNAME", lwd = 4,  border.col = "darkgray") +
#   tm_shape(df) + tm_lines(lwd = 2, col = "red") + 
#   tm_shape(df2) + tm_dots() + 
#   tm_basemap(server = "Esri.WorldImagery")
####
# found some QC issues in plots: 
# 2010--line extents from west of Tesh, to 1002 area.
# 2013--transect mismatch
# 2014--GPS error/typo in coordinate
# 2015--transect mismatch
# 2016--one extra transect in west
# 2017--hard to tell, need to zoom in but looks like extra transects or transect labeling issues. just east of tesh.
# 2019--transect label issue in west, looks like just one point.
####
## Now work to fix
####
################################################################################
# QC Find: Year 2013, transects 38 and 45 are mixed up. I plotted, 
#  Solution: found that HWM swapped transects and then fixed it
#plot in leaflet
Y = 2013
Tran = c(38, 45)
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y, Transect %in% Tran) %>% mutate(Day=as.character(Day))
#bdf <- filter(birds, Year==Y) %>% mutate(Day=as.character(Day))
tm <- tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
tm
tm_shape(filter(bdf, Observer == "HMW" & Day ==10), name=paste(Y,"Bird Obs")) + 
  tm_dots(col="Transect")
tm_shape(filter(bdf, Observer == "WWL" & Day ==10), name=paste(Y,"Bird Obs")) + 
  tm_dots(col="Transect")
#HMW is writing transect 38 on 45 and 45 on 38 on Day 10
birdsQC <- birds %>% mutate(Transect2 = if_else(Day == 10 & Observer == "HMW" & Transect == 38,45,Transect), 
                            Transect2 = if_else(Day == 10 & Observer == "HMW" & Transect == 45,38, Transect2))
tm_shape(filter(birdsQC, Observer %in% c("HMW", "WWL") & Day ==10 & Transect2 %in% c(38,45)), 
         name=paste(Y,"Bird Obs")) + 
  tm_dots(col="Transect2")
#success!
birds <- mutate(birdsQC, Transect = Transect2) %>% select(-Transect2)
rm(birdsQC)
#Now apply function across all transects
lines <- birds %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
Y = 2013
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% mutate(Day=as.character(Day))
tm <- tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
tm
## 2013 done!
################################################################################
# QC Find: Year = 2019, transect mislabeled on west side, maybe just one point
Y = 2019
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% mutate(Day=as.character(Day))
tm <- tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
tm
#appears to be west end of transect 38 to 33, start end issue?
Trans <- c(33)
df <- filter(lines, Year==Y, Transect %in% Trans)
bdf <- filter(birds, Year==Y, Transect %in% Trans) %>% mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Species") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#day == 11; HMW mislabeled START on transect 38 as 33; 
# there seem to be duplicate observations for HMW on Day = 11, trans = 33
# are there others?
dbirds <- distinct(birds)
dim(birds)[1] - dim(dbirds)[1]
#[1] 5221, there are >5k duplicate obs in the data set. 
# many of these must be justified when observers record the same species and 
# group size in the same WAV file.
df <- birds %>% group_by_all() %>% summarise(n = n()) %>% filter(n > 1)
unique(df$Year)
#How many have two start or end point recorded?
df <- birds %>% filter(Species == "START") %>% group_by_all() %>% 
  summarise(n = n()) %>% filter(n > 1)
df
#there are three occasions for this:
#  2007, RMD, transect 420 (maybe only 6 observations); 
#  2015, HMW, transect 3; (about 34 observations)
#  2019, HMW, transect 33 (found above)
df <- filter(lines, Transect %in% c(33, 38) & Year == 2019)
bdf <- birds %>%
  filter(Transect == 33 & Year == 2019 & Species == "START", Day == 12, Observer == "HMW")
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(trans[["ACP_2019_Transects"]]) + tm_lines() + 
  # tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("TRANSID", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Species") +
  tm_basemap(server = "Esri.WorldGrayCanvas")
#that START point is really on transect 38, 
# actually there already is a start for 38, so this must be a duplicate
#Need to delete this one and add one for 33 with the correct location, copy from Dave's start
# just need to swap out coordinates
birdsQC <- cbind(st_drop_geometry(birds), st_coordinates(birds)) %>%
  filter(Transect == 33 & Year == 2019 & Species == "START" & Day == 12 & 
           Observer == "DES")
newX <- birdsQC$X
newY <- birdsQC$Y
birdsQC <- cbind(st_drop_geometry(birds), st_coordinates(birds)) %>% 
  mutate(X = replace(X, Transect == 33 & Year == 2019 & Species == "START" & 
                    Day == 12 & Observer == "HMW", newX),
         Y = replace(Y, Transect == 33 & Year == 2019 & Species == "START" & 
                       Day == 12 & Observer == "HMW", newY)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)
birds <- birdsQC
rm(birdsQC)
#redo lines and plot
lines <- birds %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#plot
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y, Day == 12, Transect == 32) %>% mutate(Day=as.character(Day))
tm <- tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
tm
##Worked!
################################################################################
###############
##QC Find: east end of transect 34, tried three times. On the 15th, wondered over to transect 32.
##  maybe delete whole code 3 obs?
## QCFind, Obs on Transect 32 mislabel as 34
##QC find: on transect 53, spike in latitude of obs, not real. How to fix?
Y = 2017
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y, Day == 15,Time < 66555.2 &  Time >= 61853.3, Transect == 34) %>% 
  mutate(Day=as.character(Day))
tm <- tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
tm
###################
## QC Find: transect 32 mislabel as 34 on day = 15, Time >= 61853.3
birdsQC <- birds %>% mutate(Transect = replace(Transect, Year == 2017 & 
                                                 Day == 15 & Code == 3 & 
                                                 Time >= 61853.3 & Time < 66555.2, 32)) %>% #rename transet
  filter(!(Year == 2017 & Day == 15 & Code == 3 & Transect == 34)) %>% #remove code 3 from day 15
  filter(!(Year == 2017 & Day == 15 & Code == 1 & Transect == 34 & Time > 66000 & Time < 67000)) #remove group of code = 1 obs from 34 that were done between 66000 and 67000
bdf <- filter(birdsQC, Year==Y, Transect == 34) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#redo line to test
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#test
df <- filter(lines, Year==Y)
bdf <- filter(birdsQC, Year==Y, Transect == 34) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#Good!
# Now do spike on transect 53
df <- filter(lines, Year==Y)
bdf <- filter(birdsQC, Year==Y, Transect == 53, Day == 18) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#It a lone code = 3 START point, note that the time is earliest for transect and day
# Delete
birdsQC <- filter(birdsQC, !(Year==Y & Transect == 53 & Day == 18 & Code == 3))
#redo line to test
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#test
df <- filter(lines, Year==Y)
bdf <- filter(birdsQC, Year==Y) %>% 
  mutate(Day=as.character(Day))
tdf <- filter(trans[["ACP_2017_Transects"]])
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(tdf) + tm_lines(col = "red") + 
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
birds <- birdsQC
rm(birdsQC)
## End 2017 QC
################################################################################
#Start 2016
Y = 2016
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#QC issues:
# (1) in east, observer look like they record different days on the same transect
#   transect 16 to 28 east side of Tesh. 
# (2) extra transect effort in west while transiting between transect 64 and 50, labeled 5064
# fix (2) first
birdsQC <- filter(birds, !(Year == Y & Transect == 5064))
#fix (1):
#looking at mapped effort by day, looks like WWL has wrong label, change his day to 11
#confirmed this by looking at Heather notes on the "T" drive 
birdsQC <- mutate(birdsQC, Day = replace(Day, Year == Y & 
                                           Observer == "WWL" & 
                                           Day == 10 & 
                                           Transect %in% c(16, 18, 21, 23, 26, 28), 
                                         11))
#check
bdf <- filter(birdsQC, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#works, re do lines
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#check again
df <- filter(lines, Year==Y)
bdf <- filter(birdsQC, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
birds <- birdsQC
rm(birdsQC)
# End 2016
################################################################################
#QC 2015
Y = 2015
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#QC Finds: (1) looks like transects 24 and 38 are mismatched, who did it?
bdf <- filter(birds, Year==Y & Transect %in% c(24, 38) & Time > 69000) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#WWL was on 24 but transcribed 38, use Time as above to replace:
birdsQC <- mutate(birds, Transect = replace(Transect, Year == Y & Observer == "WWL" &
                                       Transect == 38 & Time > 69000, 24))
#check it
bdf <- filter(birdsQC, Year==Y & Transect %in% c(24, 38)) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Transect") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#worked! Re do lines
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#check again
df <- filter(lines, Year==Y)
bdf <- filter(birdsQC, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
birds <- birdsQC
rm(birdsQC)
#Continue 2015:
Y = 2015
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#QC Find: (1) Transect 16 and 18 one coordinate mismatch; 
#             it is a swan and swan nest that is on 18 according to time and 
#             latitude but was labeled as 16
#         (2) Transect 26 looks to have a strange point, investigate
#             Seems like Heather started too soon, delete her start use WWL's
#             Both started the transect too far north, then came down on on it
#         (3) Transect 30 someone has wrong Day in transcribed data,
#             Only on eastern end; also has different time range than other observer
#             On Day 10 both flew it early (Time < 63000), WWL Code = 3, but seems fine
#             On Day 9 both flew and record (Time < 63000), WWL Code = 3 but seems fine
#             On Day 9 they flew the east end again (Time > 63000), but WWL gave code = 3 again, seems fine
#             We don't have a method for recording replicates on the same day, so for now, delete
#QC Fix (1): Replace transect number
birdsQC <- mutate(birds, Transect = replace(Transect, Year == Y & Day == 13 & 
                                              Observer == "HMW" & 
                                              Transect == 16 & 
                                              Time == 37475.59, 18))
#QC Fix (2): delete HMW's Start, replace with Bill but subtract one second to get the GWFG;
#  Not sure what is best clocks are off and position and times don't really match up here
# subtract 20s from WWL's start ime
df2<- filter(birdsQC, Year == Y & Transect == 26 & Observer == "WWL" & 
                          Species == "START") %>%
  mutate(Observer = "HMW", Time = Time - 20)

birdsQC <- filter(birdsQC, !(Year == Y & Transect == 26 & Observer == "HMW" & 
                               Species == "START")) %>%
  rbind(df2) %>%
  arrange(Year, Transect, Time)
rm(df2)
#QC Fix (3): delete data from Day 9, Transect 30, Time > 63000
birdsQC <- filter(birdsQC, !(Year == Y & Transect == 30 & Day == 9 & 
                               Time > 63000))
#check it
bdf <- filter(birdsQC, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#re do lines
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#check again
df <- filter(lines, Year==Y)
bdf <- filter(birdsQC, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
birds <- birdsQC
rm(birdsQC)
#End 2015
################################################################################
#QC 2014
map_birds <- function(Y = NA, data = NA){
  df <- filter(lines, Year==Y)
  bdf <- filter(data, Year==Y) %>% 
    mutate(Day=as.character(Day))
  tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
    tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
    tm_text("Transect", size=2) +
    tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
    tm_basemap(server = "Esri.WorldGrayCanvas") +
    tm_scalebar()
}
map_birds(Y = 2014, data = birds)
#QC Find: (1) transect 40 end point way off to east;
#             negative sign is missing from longitude
#         (2) transect 40, hole or gap in center, investigate
#         (3) transect 43, multiple days, east end
#         (4) transect 35, seem to have started then stopped in same day
Y = 2014
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y, Transect == 40, Day == 12) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#QC Fix (1): add negative sign to longitude
birdsQC <- cbind(st_drop_geometry(birds), st_coordinates(birds)) %>%
  mutate(X = if_else(X > 0, -1*X, X)) %>%
  st_as_sf(coords=c("X", "Y"), crs = 4326)
#plot
map_birds(Y = 2014, data = birdsQC)
#worked
#next
#QC Fix (2): gap in 40 center
bdf <- filter(birdsQC, Year==Y, Transect == 40) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#Doesn't seem like a problem. For whatever reason they re-sampled that section 
#  where they "wondered off"
#QC Fix (3): Year = 2014, Transect = 43, Description = 
#  It appears a section of transect 43 was sampled twice, once on day ==11 and again on day ==12.
#  It seems implied that search effort was stopped before "end transect" there was recorded or computer/GPS failure 
#
# Solution: renumber transects so that each segment is a unique number
df <- filter(lines, Year==Y)
bdf <- filter(birdsQC, Year==Y, Transect == 43) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()

birdsQC <- birdsQC %>% 
  #  transect 43 on Day = 12 becomes 43.3
  #  transect 43 on Day = 11 and time < 73597 (west side) becomes 43.1
  #  transect 43 on Day = 11 and time >74000 (east side) becomes 43.2
  mutate(Transect = if_else(Year == 2014 & Transect == 43 & Day == "12", 43.3, Transect)) %>% 
  mutate(Transect = if_else(Year == 2014 & Transect == 43 & Day == "11" & Time < 73597, 43.1, Transect)) %>%
  mutate(Transect = if_else(Year == 2014 & Transect == 43 & Day == "11" & Time > 74000, 43.2, Transect))
# QC Fix (4): Transect 35, Year = 2014, 
#             Description = Transect ended at ~65980, then started again
bdf <- filter(birdsQC, Year==Y, Transect == 35) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
# QC solution: assign segments
birdsQC <- birdsQC %>% 
  mutate(Transect = if_else(Year == 2014 & Transect == 35 & Time < 65990, 35.1, Transect)) %>%
  mutate(Transect = if_else(Year == 2014 & Transect == 35 & Time > 65990, 35.2, Transect))
#Re do lines
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#check again
df <- filter(lines, Year==Y)
bdf <- filter(birdsQC, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
birds <- birdsQC
rm(birdsQC)
#End 2014
################################################################################
# 2013 done above
################################################################################
# QC 2010
## QC find: Year = 2010, Transect = 1453, Description = small number of observations (5) 
# attributed to transect 1453 have lat/longs associated with transect 363. 
# 363 seems to have been done later on day 19.
## Solution: delete five observations. These obs are very far west on 363, so delete based on transect number and long < 0.
Y = 2010
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#check transect 363
bdf <- filter(birds, Year==Y, Transect %in% c(363, 1453)) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Time") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#The times for the 5 1453 obs on transect 363 fit in perfectly with the time of the 
# obs label as transect 363 on day 19.
# Solution: assume the day and transect for these five obs are wrong and change transect and day
#  to 363 and day = 19
birdsQC <- mutate(birds, 
                  Transect = replace(Transect, Year == Y & Transect == 1453 & 
                                       Time > 56000, 363), 
                  Day = replace(Day, Year == Y & Transect == 1453 & 
                                  Time > 56000, 19))
#check
bdf <- filter(birdsQC, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#re do lines
lines <- birdsQC %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#check again
df <- filter(lines, Year==Y)
bdf <- filter(birdsQC, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
birds <- birdsQC
rm(birdsQC)
################################################################################
#map other year 2007 - 2009 to look for QC issues
Y = 2009
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
Y = 2008
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
Y = 2007
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y) %>% 
  mutate(Day=as.character(Day))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() +
  tm_text("Transect", size=2) +
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
################################################################################
#Somehow, as of 20231120 the lines sf object now has one observation with an NA for Year
#  and a linestring with only one point that causes an error. 
# Solution: This seems to have been caused by the new tmap 3.99.9999 = 4.0, which
#  doesn't really make sense but when I changed code to update for 4.0, the lines 
#  object is made correctly. Strange. Will need to add Seat to birds and see if this happens again 
# After running with Seat included in birds df, all seems fine. 
#QC is done! (yeah right, at least for now)
#transform the geometry column to a data column of Lon and Lat
coords <- st_coordinates(birds)
birds <- st_drop_geometry(birds) %>% cbind(coords) %>%
  rename(Lon = X, Lat = Y)
#Clean up ACP geometry data:
# (1) remove LENGTH and AREA data, they are calculated under an unknown crs or are wrong
# (2) rename the Prudhoe Bay "Nonhabitat" polygon to "Not Sampled"
# (3) remove "STRAT"
# (4) rename "STRATNAME"
nonhab_id <- c(2,4,5,15)
#tm_shape(acp) +
#tm_shape(filter(acp, STRATNAME == "Nonhabitat")) + 
tm_shape(acp[15,]) + 
  #tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) + 
  tm_polygons(fill = "red") + 
  tm_shape(lines) + tm_lines() + 
  tm_basemap(server = "Esri.WorldImagery")
#from above and iterating through the four 'nonhabitat' areas, it appears that in order they are:
# row 2 is northern one up by Admiralty Bay
# row 4 is Teshekpuk
# row 5 is also Teshekpuk, a sub-part. Why is that here? Slop?
# row 15 is the Prudhoe Bay area -- keep this one DELETE THE REST
acp2 <- select(acp, -AREA, -LENGTH, -STRAT) %>%
  slice(-c(2,4,5)) %>%
  rename(Stratum = STRATNAME) %>%
  mutate(Stratum = replace(Stratum, Stratum == "Nonhabitat", "Not Sampled")) %>%
  st_transform(crs=4326)
tm_shape(acp2) + 
  tm_polygons(fill = "Stratum", fill_alpha = 0.5) + 
  tm_shape(lines) + tm_lines() + 
  tm_basemap(server = "Esri.WorldImagery")
#looks better
#add stratum to lines
#use largest overlap of strat file to assign transects to strata to lines
#this causes a lot of NAs for Stratum!
lines2 <- st_join(lines, acp2, join = st_overlaps, largest = TRUE)
df <- filter(lines2, is.na(Stratum))
tm_shape(acp2) + 
  tm_polygons(fill = "Stratum") + 
  tm_shape(df) + tm_lines() 
#NAs are due to samples outside the traditional ACP survey area in the 10-02 area
#add Stratum to birds, first have to cast back into a sf object
birds2 <- st_as_sf(birds, coords = c("Lon", "Lat"), crs = 4326) %>%
  st_join(lines2, join = st_nearest_feature) %>%
  mutate(Stratum = replace(Stratum, Code == 4, "10-02 Area")) %>%
  select(-Year.y, -Transect.y) %>%
  rename(Year = Year.x, Transect = Transect.x) %>%
  relocate(Stratum, .after = Time)
df <- filter(birds2, is.na(Stratum))
tm_shape(acp2) + 
  tm_polygons() + 
  #tm_shape(lines) + tm_lines(col = "red") + 
  tm_shape(df) + tm_dots()
#there are 24 observation with Code = 1, Stratum = NA and all seem to be in 10-02 area,
# All are Observer = "HMW"
#QC find!: Change Stratum to "10-02 Area" and Code to 4
birds2 <- mutate(birds2, Stratum = replace(Stratum, is.na(Stratum), "10-02 Area"), 
                 Code = replace(Code, is.na(Stratum), 4))
df <- st_coordinates(birds2)
birds2 <- cbind(st_drop_geometry(birds2), df)
birds2 <- rename(birds2, Lon = X, Lat = Y)
###############
#need to get design transect lengths
#Oh, God! What a mess!
trans2 <- purrr::map(trans, select, ORIGID, OBJECTID)
st_rename <- function(x){st_geometry(x) <- "geometry"; return(x)}
trans2 <- purrr::map(trans2, st_rename)
tran.names <- names(trans2) %>% str_sub(5, 8) 
names(trans2) <- tran.names
trans2 <- trans2[sort(tran.names)]
trans2 <- purrr::map_dfr(trans2, rbind, .id = "Year")
#Do the transect numbers match?
dftrans <- trans2 %>% filter(Year %in% c(2007,2023), OBJECTID %in% 1:8) %>% 
  st_transform(crs = 4326)
dfObs <- birds2 %>% filter(Year %in% c(2007,2023), Transect %in% 1:8) %>%
  st_as_sf(coords=c("Lon", "Lat"), crs=4326)
tm_shape(acp2) + tm_polygons() + 
  tm_shape(dftrans) + tm_lines(col = "red") + 
  tm_shape(dfObs) + tm_dots()
#doesn't match for OBJECTID, we need ORIGID
#looks like in year = 2007, transect = 8 does not match effort, should extend to coast
#wait, ORIGID gives segment, we need OBJECTID that gives transect
#and we need to change the Transects in the bird obs dataframe to match!
# trans3 <- mutate(trans2, Year = as.numeric(Year)) %>%
#   st_transform(crs=4326) %>%
#   st_intersection(acp2)
# trans3 <- mutate(trans3, Length = units::drop_units(st_length(trans3)))
# trans3 <- filter(trans3, Length > 10)
# birds3 <-  right_join(birds2, st_drop_geometry(trans2), by = c("Year", "Transect" = "ORIGID"))
#screw it! I can't figure it out right now. 

#long break...

#still not working, need to split it up by year. 
years <- unique(birds2$Year)
birds3 <- data.frame(NULL)
for(i in years){
  dfObs <- filter(birds2, Year == i)
  dftrans <- filter(trans2, Year == i)
  df <- dfObs %>%  st_as_sf(coords=c("Lon", "Lat"), crs=4326) %>% 
    st_transform(crs=3338) %>%
    st_join(dftrans, join = st_nearest_feature)
  birds3 <- rbind(birds3, df)
}
birds3 <- birds3 %>% select(-Year.y, -ORIGID) %>%
  rename(Year = Year.x, NavTransect = Transect, Transect = OBJECTID) %>%
  relocate(Transect, .after = Stratum)
#plot to check
dftrans <- trans2 %>% filter(Year %in% c(2007,2023), OBJECTID %in% 1:8) %>% 
  st_transform(crs = 4326)
dfObs <- birds3 %>% filter(Year %in% c(2007,2023), Transect %in% 1:8) %>%
  st_as_sf(coords=c("Lon", "Lat"), crs=4326)
tm_shape(acp2) + tm_polygons() + 
  tm_shape(dftrans) + tm_lines(col = "red") + 
  tm_shape(dfObs) + tm_dots()
#I think that worked!
## Can you make a design based estimate?
estimate <- filter(birds3, Code == 1, Species == "NOPI", Stratum != "10-02 Area", 
                   Obs_Type %in% c("single", "pair", "open", "flkdrake")) %>% 
  st_drop_geometry() %>%
  group_by(Year, Stratum, Transect) %>%
  mutate(AdjustNum = if_else(Obs_Type == "pair", Num*2, Num)) %>%
  summarise(total = sum(AdjustNum)) %>% ungroup()
trans3 <- select(trans2, Year, OBJECTID) %>%
  rename(Transect = OBJECTID)
  #need to add stratum to transect data frame
trans3 <- st_join(trans3, st_transform(acp2, crs = 3338), join = st_nearest_feature)
#plot and check
tm_shape(acp2) + tm_polygons() + 
  tm_shape(trans3) + tm_lines(col = "Stratum")
#didn't work, try an intersection
trans3 <- select(trans2, Year, OBJECTID) %>%
  rename(Transect = OBJECTID)
df <- st_intersection(trans3, st_transform(acp2, crs = 3338))
tm_shape(acp2) + tm_polygons() + 
  tm_shape(df) + tm_lines(col = "Stratum")
  #tm_shape(filter(df, Stratum == "Not Sampled")) + tm_lines(col = "Stratum", lwd = 2) #"nibblet"
#group line by year, stratum and transect number into one multilinestring object
df <- df %>% group_by(Year, Stratum, Transect) %>% summarise() %>% ungroup()
trans3 <- df
#now plot to check
tm_shape(acp2) + tm_polygons() + 
  tm_shape(df) + tm_lines(col = "Stratum")
#looks good. 
#add zeros and transect lengths to estimates
df <- df %>% mutate(Length = units::set_units(st_length(df), "km")) %>%
  st_drop_geometry() %>%
  mutate(Year = as.numeric(Year))
estimate <- right_join(estimate, df)
estimate <- mutate(estimate, total = replace(total, is.na(total), 0))
density <- estimate %>% filter(Stratum != "Not Sampled") %>% 
  group_by(Year, Stratum) %>% 
  summarise(n = n(), stotal = sum(total), sLength = sum(Length),
            SEDensity = sd(total/Length)/sqrt(n)) %>%
  ungroup() %>%
  mutate(Density = if_else(Year == 2018, stotal/(sLength*units::set_units(0.2, "km")),
                           stotal/(sLength*units::set_units(0.4, "km"))))
#find area of Strata
acp <- acp2 %>% filter(Stratum != "Not Sampled") %>% 
  st_transform(crs = 3338) %>%
  mutate(Area = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(Stratum) %>%
  summarise(Area = sum(Area)) %>% 
  mutate(Area = units::set_units(Area, "km^2")) %>% ungroup()
#add area to estimates
density <- left_join(density, acp)
Pop <- density %>% mutate(Num = Density*Area, vNum = Area^2 * SEDensity^2) %>%
  group_by(Year) %>%
  summarise(Num = sum(Num), vNum = sum(vNum), sdNum = sqrt(vNum)) %>% ungroup() %>%
  mutate(Num = units::drop_units(Num), vNum = units::drop_units(vNum), 
         sdNum = units::drop_units(sdNum)) %>%
  select(-vNum) %>% mutate(Type = "Direct")
ggplot(data = Pop) +
  geom_pointrange(aes(x = Year, y = Num, ymin = Num - 2*sdNum, ymax = Num + 2*sdNum)) 
#compare to AKaerial
df <- AKaerial::ACPHistoric$combined %>% filter(Species == "NOPI") %>%
  mutate(Num = total, sdNum = total.se) %>% mutate(Type = "AKaerial") %>%
  select(Year, Num, sdNum, Type) %>%
  rbind(Pop)
ggplot(data = df, aes(color = Type)) + 
  geom_pointrange(aes(x = Year, y = Num, ymin = Num - 2*sdNum, ymax = Num + 2*sdNum), 
                  position = position_dodge(width = 0.3)) +
  labs(title = "total birds")
#that seems to be close!
#####################################
## Why are there multiple "MULTIPOLYGON" objects within strata?
# 2 High multipolygons and 7 Low !
acp3 <- mutate(acp2, Number = row.names(acp2))
ggplot(data=acp3) + 
  geom_sf(aes(fill=Number)) + 
  guides(fill=guide_legend(title="Row Objects"))
tm_shape(acp3[6,]) + tm_polygons() #One tiny strip of land on a barrier island!
tm_shape(acp3[11,]) + tm_polygons() 
tm_shape(acp3[c(6,5, 11),]) + tm_polygons() 
#group them together
acp3 <- group_by(acp2, Stratum) %>% summarise()

#End design transect QC
#There are four things to write: acp strata polygons, transects, 
#  bird obs with correct strata and transects number, and flight lines with 
#  correct strata and transects
st_write(acp3, dsn = "Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#write transects
trans3 <- st_transform(trans3, crs = 4326)
st_write(trans3, dsn = "Data/ACP_2023/analysis_output/ACP_DesignTrans_QC.gpkg") 
#birds3 has correct transect numbers
df <- st_transform(birds3, crs = 4326) %>% st_coordinates() 
birds3 <- st_drop_geometry(birds3) %>% cbind(df) %>% rename(Lon = X, Lat = Y)
write_csv(birds3, file = "Data/ACP_2023/analysis_output/Bird_QC_Obs.seat.stratum.csv")
#flight lines, no transect numbers as the flight line might deviate
#re do lines
lines <- birds3 %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  select(-Transect) %>% rename(Transect = NavTransect) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#check lines
plot(st_geometry(lines))
lines <- rename(lines, NavTransect = Transect)
st_write(lines, dsn = "Data/ACP_2023/analysis_output/Lines_Obs.2.gpkg")
################################################################################
## Fix 2018 10-02 issue found by Emily Silverman, Feb. 2024
## Many 10-02 observation are labeled as "low" stratum and have incorrect Transect number
##  fix this by assigning NavTransect to Transect and "10-02" as stratum for 2018
#  I am also changing the Code == "DEAD" to Code == 5 so that there are not read errors
dat <- read_csv(file = "Data/ACP_2023/analysis_output/Bird_QC_Obs.seat.stratum.csv", 
                col_types = cols(Code = "c")) |>
  mutate(Transect = if_else(Year >= 2018 & NavTransect > 200, NavTransect, Transect), 
         Stratum =  if_else(Year >= 2018 & NavTransect > 200, "10-02 Area", Stratum), 
         Code = if_else(Code == "DEAD", "5", Code)) |>
  mutate(Code = as.integer(Code)) 
#replaced Code == DEAD to Code == 5
#check data
df <- filter(dat, Stratum == "10-02 Area") |>
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) 
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name="Bird Obs") + tm_dots(col="Transect") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
df <- filter(dat, Stratum == "Low" & Year >= 2018) |>
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) 
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name="Bird Obs") + tm_dots(col="Transect") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#make lines
source("points2line.R")
lines <- dat %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  select(-Transect) %>% rename(Transect = NavTransect) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#check lines
plot(st_geometry(lines[lines$Year == 2018,]))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(lines[lines$Year == 2018,]) + tm_lines() +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
lines <- rename(lines, NavTransect = Transect)
#write data
write_csv(dat, file = "Data/ACP_2023/analysis_output/Bird_QC_Obs.seat.stratum.2024.csv")
st_write(lines, dsn = "Data/ACP_2023/analysis_output/Lines_Obs.2024.gpkg")
################################################################################
## Fix 2016 issue found by Emily Silverman on 20230214: 
#    (1) WWL Code = 2 should be 1 for transects 19 and 24
#    (2) Month should be 6 for WWL in 2016 (found by Emily and me simultaneously)
dat <- read_csv(file = "Data/ACP_2023/analysis_output/Bird_QC_Obs.seat.stratum.2024.csv")
dat <- mutate(dat , 
              Code = if_else(Year == 2016 & Observer == "WWL" & Seat == "RF", 1, Code), 
              Month = 6)
#make lines
source("points2line.R")
lines <- dat %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  select(-Transect) %>% rename(Transect = NavTransect) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#check lines
plot(st_geometry(lines[lines$Year == 2016,]))
acp <- st_read(dsn="Data/ACP_2023/source_data/ACP_DesignStrata.gpkg") %>%
  st_transform(crs=4326) 
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(lines[lines$Year == 2016,]) + tm_lines() +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#looks good
lines <- rename(lines, NavTransect = Transect)
#write data, use timestamp in file name
write_csv(dat, file = paste0("Data/ACP_2023/analysis_output/Bird_QC_Obs",Sys.Date(),".csv"))
st_write(lines, dsn = paste0("Data/ACP_2023/analysis_output/Lines_Obs",Sys.Date(), ".gpkg"))
################################################################################
# Implement fix of Observer typo found in March 2024:
#   Change all observer initials RMD to RDM in 2007 to 2009
# Also add commit version info to data assets
library(tidyverse)
birds <- read_csv(file = "Data/ACP_2023/analysis_output/Bird_QC_Obs2024-02-15.csv")
birds <- birds |>
  mutate(Observer = replace(Observer, Year %in% 2007:2008 & Observer == "RMD", "RDM"))
#check
table(birds$Year, birds$Observer)
#looks good
#write data, use timestamp in file name
write_csv(birds, file = paste0("Data/ACP_2023/analysis_output/Bird-QC-Obs-",Sys.Date(),".csv"))
#commit changes
git2r::commit(all=TRUE, message="fix observer typo")
################################################################################
# Add 2024 data
# need to turn this into a function
library(tidyverse)
library(sf)
source("points2line.R")
#Sciencebase item number https://www.sciencebase.gov/catalog/item/65419bbdd34ee4b6e05bd005
library(sbtools)
tmp <- item_file_download("65419bbdd34ee4b6e05bd005", 
                   names = "ACP_2022-2024_QCObs_name.zip", 
                  destinations=file.path(tempdir(), "temp"))
unzip(zipfile = tmp, exdir = file.path(tempdir(), "temp2"))
files <- list.files(file.path(tempdir(), "temp2"), full.names = TRUE)
files
dat1 <- read_csv(file = files[5])
dat2 <- read_csv(file = files[6])
birds24 <- rbind(dat1, dat2) |> 
  select("Year", "Month", "Day", "Time", "Transect", "Observer", "Seat", 
         "Species", "Num", "Obs_Type", "Code", "Lat", "Lon") |> 
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) 
#makes lines
lines24 <- birds24 %>% st_transform(crs=4326) %>%
  group_split(Year, Transect, Day) %>%
  map(points2line) %>%
  map_dfr(rbind)
#plot
plot(st_geometry(lines24), main = 2024)
#try tmap to zoom in 
library(tmap)
tmap_mode("view")
acp <- st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg") %>%
  st_transform(crs=4326) 
tm_shape(acp) +
  tm_polygons(fill = "STRATNAME", fill_alpha = 0.5, lwd = 4,  col = "darkgray") +
  tm_shape(lines24) + tm_lines(lwd = 2, col = "red") +
  tm_shape(birds24) + tm_dots() +
  tm_basemap(server = "Esri.WorldImagery")
#looks good
lines24 <- rename(lines24, NavTransect = Transect)
#read in lines data, append, and write to new file
lines <- st_read(dsn = "Data/ACP_2023/analysis_output/Lines-Obs-2024-02-15.gpkg")
st_geometry(lines) <- "geometry"
lines <- rbind(lines, lines24)
st_write(lines, dsn = paste0("Data/ACP_2023/analysis_output/Lines-Obs-",Sys.Date(), ".gpkg"))
#now birds
#need transects for 2024
tmp <- item_file_download("6477f116d34eac007b50c5cd", 
                          names = "ACP_DesignTrans.gpkg", 
                          destinations=file.path(tempdir(), "temp3"))
st_layers(dsn = file.path(tempdir(), "temp3"))
#no 2024 transects!
#try RDR, need to be on VPN
path <- "//ifw7ro-file.fws.doi.net/datamgt/mbm/mbmwa_008_ACP_Aerial_Survey/data/design_files/ACP_DesignTrans.gpkg"
trans <- st_layers(dsn = path)
trans
#they appear to be on RDR, but 2024 are different CRS than other years
trans24 <- st_read(dsn = path, layer = "ACP_2024_Transects") |>
  st_transform(crs = 4326)
plot(st_geometry(trans24))
trans <- st_read(dsn = "Data/ACP_2023/analysis_output/ACP_DesignTrans_QC.gpkg")
filter(trans, Year == 2018) |>
  st_geometry() |>
  plot(col = "red", add=TRUE)
#interesting, transect just south of tesh in 2018 was not in design for 2024
plot(st_geometry(trans24))
filter(trans, Year == 2014) |>
  st_geometry() |>
  plot(col = "red", add=TRUE)
#but it was in 2014!
#what about birds?
plot(st_geometry(trans24))
filter(lines, Year == 2018) |>
  st_geometry() |>
  plot(col = "red", add=TRUE)
#it is in the lines/birds data
#what about the original data source
trans18 <- st_read(dsn = path, layer = "ACP_2018_Transects") |>
  st_transform(crs = 4326)
plot(st_geometry(trans24))
plot(trans18, col = "red", add=TRUE)
#also missing in original
####################################
trans3 <- mutate(trans24, Year = 2024) |>
  select(Year, OBJECTID) |>
  rename(Transect = OBJECTID) |>
  st_intersection(acp) |>
  group_by(Year, Stratum, Transect) |> 
  summarise() |> 
  ungroup()
tm_shape(acp) + tm_polygons() + 
  tm_shape(trans3) + tm_lines(col = "Stratum")
#write to file
st_geometry(trans3) <- "geometry"
trans3 <- st_cast(trans3, "MULTILINESTRING")
st_geometry(trans) <- "geometry"
trans <- rbind(trans, trans3)
st_write(trans, dsn = paste0("Data/ACP_2023/analysis_output/ACP_DesignTrans_QC_",Sys.Date(), ".gpkg"))
#####################################
birds3 <- st_join(birds24, trans3, join = st_nearest_feature)
birds <- read_csv(file = "Data/ACP_2023/analysis_output/Bird-QC-Obs-2024-03-21.csv")
birds3 <- birds3 %>% select(-Year.y) %>%
  rename(Year = Year.x, NavTransect = Transect.x, Transect = Transect.y) %>%
  relocate(Stratum, .after = Time) |>
  relocate(Transect, .after = Stratum)
coords <- st_coordinates(birds3)
birds3 <- st_drop_geometry(birds3) |>
  cbind(coords) |>
  rename(Lon = X, Lat = Y)
#append and write
birds <- rbind(birds, birds3)
write_csv(birds, file = paste0("Data/ACP_2023/analysis_output/Bird-QC-Obs-", Sys.Date(),".csv"))
