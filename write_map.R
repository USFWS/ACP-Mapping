#write spatial predictions to database
#for USFWS ES, Section 7 consultations March 15, 2024
#   sent to Dash Feierabend and Pauline Hope
#   March 2024--requested smaller grid of 750m
library(tidyverse)
library(sf)
load("SPEI_figs.RData")
#make new grid
test <- select_area(area = acp, select = "all") %>%
  st_transform(crs=3338)
testgrid <- st_intersection(test, st_make_grid(x=test, cellsize = 750)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
plot(st_geometry(testgrid))
#need to change unit of input grid to km^2
testgrid <- mutate(testgrid, Grid.Area = units::set_units(Grid.Area, km^2)) 

map <- map_GAM(gamfit = fit2.o, grid = testgrid, Spp = "SPEI", Year = 2023)
map[[1]][[1]]

#seems to work
#iterate through years and write to geopackage
for(i in 2007:2023){
  map <- map_GAM(gamfit = fit2.o, grid = testgrid, Spp = "SPEI", Year = i)
  st_geometry(map$preds) <- "geometry"
  map <- map$preds |> select(-CV) |>
    mutate(Grid.Area = units::drop_units(Grid.Area))
  st_write(map, dsn="Data/ACP_2023/analysis_output/ACP_SPEI.gpkg", 
           layer = paste0(i), )
}
#try to read it
test <- st_read(dsn="Data/ACP_2023/analysis_output/ACP_SPEI.gpkg", layer = "2007")
