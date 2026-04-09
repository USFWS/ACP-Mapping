#write spatial predictions to database
#for USFWS ES, Section 7 consultations March 21, 2024
#   March 2024--requested smaller grid of 750m
#             --fix typo in observer initials, re-do model fit, write map
#March 2026 modified to loop over species and write rasters to file
#  for including density map in ACP report and to share rasters outside of MBM
#  - will use terra package and write to multiple layer (band) geotiff file
library(tidyverse)
library(sf)
library(terra)
source("map_density_functions.R")
# spp = "GWFG"
# fit <- readRDS(paste0("Data/ACP_2025/gam/", spp, "nb_fit.RDS"))
#make new grid
acp <- st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#make grid
acp <- select_area(area = acp, select = "all") %>%
  st_transform(crs=3338)
grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = 1000)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
#need to change unit of input grid to km^2
grid <- mutate(grid, Grid.Area = units::set_units(Grid.Area, km^2)) 
#test it
# map <- map_GAM(gamfit = fit, grid = grid, Spp = spp, Year = 2023)
#map[[1]][[1]]
#seems to work
#write survey area polygon to file
sysdate <- Sys.Date()
st_write(acp, dsn=paste0("Data/ACP_2025/gam/ACP-Survey-Area.gpkg"), 
         layer = "ACP Survey Area")
#iterate through species and years, write to tif
# make template raster
template = rast(grid, res = 1000, crs = "EPSG:3338")
#list to hold results
raster_list <- list()
#species list of GAM objects
spplist <- list.files("Data/ACP_2025/gam", pattern = ".RDS") |>
  stringr::str_sub(1, 4)
# make table of model formula by species
df <- data.frame()
years <- c(2007, 2025) #c(2007:2019, 2022:2025)
for(spp in spplist){
  print(paste0("Species ", spp))
  fit <- readRDS(paste0("Data/ACP_2025/gam/", 
                        spp, "nb_fit.RDS"))
  df <- rbind(df, c(spp, as.character(fit$formula)[3]))
  for(i in years){
    print(paste0("Predicting year ", i))
    map <- map_GAM(gamfit = fit, grid = grid, Spp = spp, Year = i)
    st_geometry(map$preds) <- "geometry"
    raster_list[[i - 2007 + 1]] <- rasterize(vect(map$preds), template, 
                                            field = "fit")
  }
  #find average
  print(paste0("Predicting average"))
  map <- map_GAM(gamfit = fit, grid = grid, Spp = spp, Year = 2025,
                 exclude.term = c("s(Observer)", "s(fYear)", "s(Year)",
                                  "ti(X,Y,Year)"))
  st_geometry(map$preds) <- "geometry"
  raster_list[[length(years) + 1]] <- rasterize(vect(map$preds), template, 
                                           field = "fit")
  #write to disk
  stack <- do.call("c", raster_list)
  names(stack) <- c(years, "Average")
  writeRaster(stack,
              filename = paste0("Data/ACP_2025/gam/ACP-", spp, "-",
                                sysdate,".tif"),
              overwrite = TRUE)
}
names(df) <- c("Species", "Formula")
# 
# #read raster
# test <- rast(x= paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#               sysdate,".tif"))
# plot(test[[1]])
# #read with stars
# test <- read_stars(paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                              sysdate,".tif"))
# test
# 
# plot(test[[1]])
# #########################################################################
# #commit changes
# git2r::commit(all=TRUE, message=paste0("Wrote geopackage for ", spp))
# commit <- as.data.frame(git2r::revparse_single(revision = "HEAD"))
# st_write(commit, dsn=paste0("Data/ACP_2023/analysis_output/ACP-", spp, "-", 
#                             Sys.Date(),".gpkg"), 
#          layer = "Version")
# #########################################################################
# ## test making  raster of the density predictions
# library(stars)
# template = st_as_stars(st_bbox(map$preds), dx = 1000, dy= 1000, values = NA_real_)
# star_raster = st_rasterize(map$preds[,"fit"], template)
# plot(star_raster, main="")
# write_stars(star_raster, 
#             dsn=paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                        sysdate,".tif"))
# write_stars(star_raster, 
#             dsn=paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                        sysdate,".tif"), append=TRUE, along = "band")
# tmp <- read_stars(paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                          sysdate,".tif"))
# tmp
# #terra
# library(terra)
# template = rast(vect(map$preds), res = 1000)
# terra_raster1 <- rasterize(vect(map$preds), template, field = "fit")
# terra_raster2 <- rasterize(vect(map$preds), template, field = "se.fit")
# plot(terra_raster1)
# stack <- c(terra_raster1, terra_raster2)
# writeRaster(stack[[1]], 
#             filename = paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                                      sysdate,".tif"), 
#             overwrite = TRUE)
# gwfg <- rast(x= paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                        sysdate,".tif"))
# plot(gwfg[[2]]) #tif works
# #try geopackage
# # st_write(acp, dsn=paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
# #                          sysdate,".gpkg"), layer = "ACP Survey Area")
# writeRaster(
#   stack[[1]],
#   paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#          sysdate,".gpkg"),
#   filetype = "GPKG",
#   #gdal = c("APPEND_SUBDATASET=YES", paste0("RASTER_TABLE=", "fit")),
#   overwrite = TRUE # Use overwrite=TRUE if the file or layer might exist
# )
# st_layers(dsn=paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                      sysdate,".gpkg"))
# gwfg <- rast(x= paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                        sysdate,".gpkg"))
# plot(gwfg) #worked!
# # which is is smaller geotif or geopackage?
# file.size(paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                  sysdate,".tif"))
# file.size(paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                  sysdate,".gpkg"))
# #tif is smaller
# #Is raster smaller than sf polygon? (should be)
# st_write(select(map$preds, fit), 
#          dsn=paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                          sysdate,".gpkg"))
# file.size(paste0("Data/ACP_2025/gam/ACP-", spp, "-", 
#                  sysdate,".gpkg"))
# #geotiff is WAY smaller!