#load and map ACP
library(sf)
library(tidyverse)

acp <- st_read(dsn="Design_Strata/ACP_2007to2018_DesignStrata.shp") %>%
  st_transform(crs=4326)
plot(st_geometry(acp))
acp2 <- st_read(dsn="Design_Strata/ACP_2007to2019_StudyArea.shp") %>%
  st_transform(crs=4326)
plot(st_geometry(acp2))

sum(st_area(acp))
st_area(acp2)
#stupid! we should delete the 'study area' file
acp3 <- st_union(acp)
plot(st_geometry(acp3))

#make bounding box
bbox <- st_sf(st_as_sfc(st_bbox(acp)))

trans1 <- st_read(dsn="Design_Transects/ACP_2019_Transects.shp") %>%
  st_transform(crs=4326)

ggplot(data=acp) + 
  geom_sf(aes(fill=STRATNAME)) + 
  geom_sf(data=trans1) +
  guides(fill=guide_legend(title="Strata"))

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
  map_dfr(select, "Species", "Year", "Lat", "Lon", "Num", "Obs_Type", "Transect") %>%
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) %>%
  st_filter(bbox) #get rid of the birds in Nigeria (lat/long = 0)


Spp <- c("ARTE", "BRAN", "CCGO", "COEI", "GLGU", "GWFG", "JAEG", "KIEI", "LTDU", 
         "NOPI", "PALO", "RTLO", "SAGU", "SNGO", "SPEI", "STEI", "SWAN", "YBLO") 
# lo <- c("PALO", "RTLO", "YBLO")
# ei <- c("COEI", "KIEI", "SPEI", "STEI")
# gu <- c("ARTE", "GLGU", "JAEG", "SAGU")
# ge <- c("BRAN", "CCGO", "GWFG", "SNGO", "SWAN")
# du <- c("AMWI", "LTDU", "NOPI", "RBME")

tm <- tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) + 
  tm_shape(trans1) + 
  tm_basemap(server = "Esri.WorldImagery")

for(i in Spp){
sp <- filter(birds, Species %in% i) %>% st_as_sf(coords=c("Lon", "Lat"), crs=4326)
tm <- tm + tm_shape(sp, name=i) + 
  tm_dots(col=terrain.colors(length(Spp))[which(factor(Spp)==i)])
}
tm 

#map year-specific observations and make track lines
Y = 2019
df <- filter(birds, Year==Y) %>%
  group_by(Transect) 
ggplot(data=trans1, group=TRANSID) + 
  geom_sf(col=trans1$TRANSID) + 
  geom_sf(data=df, size=0.2)

#try Y = XXXX
Y = 2007
trans1 <- st_read(dsn=paste("Design_Transects/ACP_",Y,"_Transects.shp", sep="")) %>%
  st_transform(crs=4326) 
df <- filter(birds, Year==Y) %>%
  group_by(Transect) 
ggplot(data=trans1, group=TRANSID) + 
  geom_sf(col=trans1$TRANSID) + 
  geom_sf_text(aes(label =TRANSID),size=3,family="sans") +
  geom_sf(data=df, size=0.2) + 
  labs(title=Y)

#segmentize transects
source("trans2seg.R")
trans.seg <- trans2seg(trans1, utm=6332)
