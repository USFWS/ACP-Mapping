#load and map ACP
library(sf)
library(tidyverse)

acp <- st_read(dsn="Design_Strata/ACP_2007to2018_DesignStrata.shp") %>%
  st_transform(crs=4326)
plot(st_geometry(acp), col=factor(acp$STRATNAME))
acp2 <- st_read(dsn="Design_Strata/ACP_2007to2019_StudyArea.shp") %>%
  st_transform(crs=4326)
plot(st_geometry(acp2))

sum(st_area(acp))
st_area(acp2)
# stupid! we should delete the 'study area' file; why do we keep these separate?
acp3 <- acp %>% st_union() %>% st_make_valid()
plot(st_geometry(acp3))
st_area(acp3) #humm, error in area calculations, keep acp2?
#acp strata overlap so the sum of strata areas should not add up
#then why does area of acp == acp2?

#make bounding box sf object
bbox <- st_sf(st_as_sfc(st_bbox(acp)))

#read and plot transects
trans1 <- st_read(dsn="Design_Transects/ACP_2019_Transects.shp") %>%
  st_transform(crs=4326)

ggplot(data=acp) + 
  geom_sf(aes(fill=STRATNAME)) + 
  geom_sf(data=trans1) +
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
          "Observer", "Species", "Num", "Obs_Type", "Lat", "Lon") %>%
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) %>%
  st_filter(bbox) #get rid of the birds in Nigeria (lat/long = 0)

ibp <- birds %>% filter(Obs_Type != "flkdrake", Num > 0,
                        Obs_Type %in% c("single", "pair") | (Obs_Type == "open" & Num < 3))

flocks <- birds %>% filter(Obs_Type == "flkdrake" | (Obs_Type == "open" & Num >= 3))

Spp <- c("ARTE", "BRAN", "CCGO", "COEI", "GLGU", "GWFG", "JAEG", "KIEI", "LTDU", 
         "NOPI", "PALO", "RTLO", "SAGU", "SNGO", "SPEI", "STEI", "SWAN", "YBLO") 
# lo <- c("PALO", "RTLO", "YBLO")
# ei <- c("COEI", "KIEI", "SPEI", "STEI")
# gu <- c("ARTE", "GLGU", "JAEG", "SAGU")
# ge <- c("BRAN", "CCGO", "GWFG", "SNGO", "SWAN")
# du <- c("AMWI", "LTDU", "NOPI", "RBME")

#singles and pairs == indicated breeding birds
tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
  tm_shape(trans1) + tm_lines() +
  tm_basemap(server = "Esri.WorldGrayCanvas")
for(i in Spp){
sp <- filter(ibp, Species %in% i) %>% st_as_sf(coords=c("Lon", "Lat"), crs=4326)
tm <- tm + tm_shape(sp, name=i) + 
  tm_dots(col=terrain.colors(length(Spp))[which(factor(Spp)==i)])
}
tm 

#flocks?


#map year-specific observations and make track lines
Y = 2019
df <- filter(birds, Year==Y) %>%
  group_by(Transect) 
ggplot(data=trans1, group=TRANSID) + 
  geom_sf(col=trans1$TRANSID) + 
  geom_sf(data=df, size=0.2)
#leaflet
tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
  tm_shape(trans1, name="Design Trans.") + tm_lines() +
  tm_text("TRANSID") +
  tm_basemap(server = "Esri.WorldGrayCanvas") + 
  tm_scale_bar()

for(Y in 2007:2019){
sp <- filter(birds, Year == Y) %>% st_as_sf(coords=c("Lon", "Lat"), crs=4326)
tm <- tm + tm_shape(sp, name=as.character(Y)) + 
    tm_dots(col=terrain.colors(13)[which(2007:2019==Y)])
}
tm 

#make a linestring for each transect
#first test on 2016 data
test0 <- filter(birds, Year==2016) %>% 
  cbind(st_coordinates(.)) %>%
  #distinct(X, Y) %>%
  group_by(Transect) %>%
  arrange(X, .by_group=TRUE) 

#make a linestring from points, first remove geometry, then rebuild
test <- as.data.frame(test0) %>% 
  select(-geometry) %>%
  filter(Transect == 39) %>%
  arrange(X, Y) %>%
  select(X, Y) %>%
  as.matrix() %>%
  st_linestring() %>%
  st_sfc(crs=4326) %>%
  st_sf(geometry=.) %>%
  mutate(Transect = 39)
plot(st_geometry(test))
#that worked, now turn into function and apply across all transects

points2line <- function(x, Year=unique(x$Year), Transect=unique(x$Transect), crs=4326){
  #this function accepts sf object of points and returns an sf linestring
  #also will accept an ID variable and return that as an attribute of the linestring, 
  # otherwise returns an ID == NA.
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
x = filter(birds, Year == 2016, Transect == 39)
points2line(x=x)

#Now apply function across all transects
lines <- birds %>% group_split(Year, Transect) %>%
  map(points2line) %>%
  map_dfr(rbind)
#plot them
filter(lines, Year==2010) %>%
  st_geometry() %>%
  plot()
# found some QC issues, plot 2010, 2013, 2015 2016 2017 2019 data and others
  
#plot in leaflet
Y = 2007
df <- filter(lines, Year==Y)
bdf <- filter(birds, Year==Y)
tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
  tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() + 
  tm_text("Transect") + 
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots() + 
  tm_basemap(server = "Esri.WorldGrayCanvas") + 
  tm_scale_bar() 

#loop through years
tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5)
for(Y in 2007:2019){
  df <- filter(lines, Year==Y)
  bdf <- filter(birds, Year==Y)
  tm <- tm + tm_shape(df, name=paste(Y, "Flown Track")) + tm_lines() + 
    tm_text("Transect") + 
  tm_shape(bdf, name=paste(Y,"Bird Obs")) + tm_dots() + 
  tm_basemap(server = "Esri.WorldGrayCanvas") + 
  tm_scale_bar() 
}
tm 
#hide layers
# %>% 
#   tmap_leaflet() %>%
#   leaflet::hideGroup(c(paste(2008:2019, "Flown Track"), paste(2008:2019, "Bird Obs")))

#QC issues with transect numbering, try sorting by time?



#segmentize transects
source("trans2seg.R")
trans.seg <- trans2seg(trans1, utm=6332)
