#write spatial predictions to database
#for USFWS ES, Section 7 consultations March 21, 2024
#   March 2024--requested smaller grid of 750m
#             --fix typo in observer initials, re-do model fit, write map
library(tidyverse)
library(sf)
source("map_density_functions.R")
spp = "STEI"
fit <- readRDS(paste0("Data/ACP_2023/analysis_output/gam/", spp, "_fit.RDS"))
#make new grid
acp <- st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#make grid
acp <- select_area(area = acp, select = "all") %>%
  st_transform(crs=3338)
grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = 750)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
#need to change unit of input grid to km^2
grid <- mutate(grid, Grid.Area = units::set_units(Grid.Area, km^2)) 
#test it
map <- map_GAM(gamfit = fit, grid = grid, Spp = spp, Year = 2020)
map[[1]][[1]]
#seems to work
#iterate through years and write to geopackage
for(i in 2007:2023){
  map <- map_GAM(gamfit = fit2.o, grid = grid, Spp = spp, Year = i)
  st_geometry(map$preds) <- "geometry"
  map <- map$preds |> select(-CV) |>
    mutate(Grid.Area = units::drop_units(Grid.Area))
  st_write(map, dsn=paste0("Data/ACP_2023/analysis_output/ACP-", spp, "-", 
                           Sys.Date(),".gpkg"), 
           layer = paste0(i))
}
#try to read it
test <- st_read(dsn=paste0("Data/ACP_2023/analysis_output/ACP-", spp, "-", 
                           Sys.Date(),".gpkg"),
                layer = "2007")
#add metadata as layer table
#commit changes
git2r::commit(all=TRUE, message=paste0("Wrote geopackage for ", spp))
commit <- as.data.frame(git2r::revparse_single(revision = "HEAD"))
st_write(commit, dsn=paste0("Data/ACP_2023/analysis_output/ACP-", spp, "-", 
                            Sys.Date(),".gpkg"), 
         layer = "Version")
