#map raw track files
library(tidyverse)
library(sf)
library(tmap)

#read in an example track
path <- read_csv(file = "Data/ACP_2023/raw_data/ACP_20220621_Track_HWilson.txt", 
                 col_names = c("Lat", "Lon", "Time"), col_types = "ddd") %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

df <- st_read(dsn = "Data/ACP_2023/analysis_output/Lines_Obs.gpkg") %>%
  filter(Year == 2022)

tm_shape(acp) + tm_polygons(col = "STRATNAME", alpha = 0.5) +
  tm_shape(path, name=paste(Y, "Flown Track")) + tm_dots(col = "Time") +
  tm_shape(df, name=paste(Y,"Design Transect")) + tm_lines(col="red") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scale_bar()
