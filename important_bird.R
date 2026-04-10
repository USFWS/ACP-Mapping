# read rasters and make a raster of "important bird areas" as in Amundson et al. 2019
#  This is based on deviations from the spatial partial effect or the spatial
#    average effects. Could be produced from the GAM model output and predicting 
#    the spatial partial effect or from the average raster. I will use the 
#    average raster.
# Amundson used three discrete categories to classify importance, 1, 0 , -1 for 
#  each species, where the 0 category was based on posterior overlap of the cell
#  to the grand mean. Instead, I used a continuous score standardized to the 
#  range across the map for each species, see below
library(tidyverse)
library(terra)

#need species list
spplist <- list.files("Data/ACP_2025/gam", pattern = ".RDS") |>
  stringr::str_sub(1, 4)
spplist <- spplist[ !spplist %in% c("AMWI", "BLSC", "COEI", "CORA", "EMGO", 
                                    "GOEA", "GWTE", "MALL", "NSHO", "RNGR", 
                                    "SACR", "SEOW", "SNOW", "SUSC") ]
##############################################
# standardize to max
#make template raster
iba <- rast(x=paste0("Data/ACP_2025/gam/ACP-", spplist[1], "-2026-04-06.tif"), 
            lyrs = 3)
plot(iba, main = paste0("Relative importance for ", spplist[1]), axes = FALSE)
x <- global(iba, "min", na.rm=TRUE)[1,1]
y <- global(iba, "range", na.rm = TRUE)
y <- (y[1,2] - y[1,1])
iba <- (iba - x)/y
#png(filename = paste0("Data/ACP_2025/Rel_import_", spplist[1], ".png"))
plot(iba, main = paste0("Relative importance for ", spplist[1]), axes = FALSE)
#dev.off()
#loop over species
for(i in 2:length(spplist)){
  r <- rast(x=paste0("Data/ACP_2025/gam/ACP-", spplist[i], "-2026-04-06.tif"), 
            lyrs = 3)
  x <- global(r, "min", na.rm=TRUE)[1,1]
  y <- global(r, "range", na.rm = TRUE)
  y <- (y[1,2] - y[1,1])
  r <- (r - x)/y
  iba <- iba + r
}
# plot(iba)
# x <- global(iba, "min", na.rm=TRUE)[1,1]
# y <- global(iba, "range", na.rm = TRUE)
# y <- (y[1,2] - y[1,1])
# iba <- (iba - x)/y
png(filename = "Data/ACP_2025/plots/Rel_import.png", height = 200)
plot(iba, main = "Relative importance to waterbirds", axes = FALSE)
dev.off()
writeRaster(iba, filename = "Data/ACP_2025/plots/Rel_import.tif")
# iba2 <- ifel(iba > 0.6, 1, 0)
# plot(iba2, main = "Relative importance to waterbirds")
# ##############################################
# standardize to mean, 3 discrete levels relative to mean
# #make template raster
# iba <- rast(x=paste0("Data/ACP_2025/gam/ACP-", spplist[1], "-2026-04-06.tif"), 
#             lyrs = 3)
# plot(iba)
# x <- global(iba, "mean", na.rm=TRUE)[1,1]
# iba <- (iba - x)/x
# plot(iba)
# y <- global(iba, "range", na.rm = TRUE)
# y <- (y[1,2] - y[1,1])/3
# iba <- ifel(iba > y, 1, ifel(iba < 0, -1, 0))
# #iba <- ifel(iba > 0, 1, ifel(iba < 0, -1, 0))
# plot(iba)
# 
# #loop over species
# for(i in 2:length(spplist)){
#   r <- rast(x=paste0("Data/ACP_2025/gam/ACP-", spplist[i], "-2026-04-06.tif"), 
#               lyrs = 3)
#   x <- global(r, "mean", na.rm=TRUE)[1,1]
#   r <- (r - x)/x
#   y <- global(r, "range", na.rm = TRUE)
#   y <- (y[1,2] - y[1,1])/3
#   r <- ifel(r > y, 1, ifel(r < 0, -1, 0))
#   #r <- ifel(r > 0, 1, ifel(r < 0, -1, 0))
#   iba <- iba + r
# }
# plot(iba)
# ############################################################
# # standardized to mean, then to range, leave continuous
# iba <- rast(x=paste0("Data/ACP_2025/gam/ACP-", spplist[1], "-2026-04-06.tif"),
#             lyrs = 3)
# plot(iba)
# x <- global(iba, "mean", na.rm=TRUE)[1,1]
# iba <- (iba - x)/x
# plot(iba)
# x <- global(iba, "min", na.rm=TRUE)[1,1]
# y <- global(iba, "range", na.rm = TRUE)
# y <- (y[1,2] - y[1,1])
# iba <- (iba - x)/y
# plot(iba)
# 
# #loop over species
# for(i in 2:length(spplist)){
#   r <- rast(x=paste0("Data/ACP_2025/gam/ACP-", spplist[i], "-2026-04-06.tif"),
#               lyrs = 3)
#   x <- global(r, "mean", na.rm=TRUE)[1,1]
#   r <- (r - x)/x
#   x <- global(r, "min", na.rm=TRUE)[1,1]
#   y <- global(r, "range", na.rm = TRUE)
#   y <- (y[1,2] - y[1,1])
#   r <- (r - x)/y
#   iba <- iba + r
# }
# plot(iba)
