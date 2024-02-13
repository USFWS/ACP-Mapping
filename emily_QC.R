#QC 2018 data check
#Emily Silverman found a QC issue on Feb, 2024
library(tidyverse)
library(sf)
#read QC data
dat <- read_csv(file = "Data/ACP_2023/analysis_output/Bird_QC_Obs.seat.stratum.csv") 
dat18 <- filter(dat, Year == 2018) %>%
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) 
  
#plot
library(tmap)
acp <- st_read(dsn="Data/ACP_2023/source_data/ACP_DesignStrata.gpkg") %>%
  st_transform(crs=4326) 
tran_layers <- st_layers(dsn = "Data/ACP_2023/source_data/ACP_DesignTrans.gpkg")
trans <- st_read(dsn="Data/ACP_2023/source_data/ACP_DesignTrans.gpkg", 
                        layer = "ACP_2018_Transects") %>%
  st_transform(crs=4326) 
tm <- tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(dat18, name="Bird Obs") + tm_dots(col="Day") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
tm
#Now look just at transect 33
df <- dat18 |> filter(Transect == 33) |> mutate(Stratum = factor(Stratum))
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name="Bird Obs") + tm_dots(col="NavTransect") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#look at transect 33 and 10-02 area
df1002 <- filter(df, NavTransect > 200)
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df1002, name="Bird Obs") + tm_dots(col="NavTransect") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#Stratum is labled as Low in many of these, Transect is 33 but should be = NavTransect in many cases.
#for HMW code = 1 for 10-02 area. Change this to 4 before assigning stratum. 
