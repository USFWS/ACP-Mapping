#QC 2018 data check
#Emily Silverman found a QC issue in Feb, 2024
library(tidyverse)
library(sf)
library(tmap)
tmap_mode(mode = "view")
#read QC data
dat <- read_csv(file = "Data/ACP_2023/analysis_output/Bird_QC_Obs.seat.stratum.csv") 
dat18 <- filter(dat, Year == 2018) %>%
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) 
  
#plot
acp <- st_read(dsn="Data/ACP_2023/source_data/ACP_DesignStrata.gpkg") %>%
  st_transform(crs=4326) 
tran_layers <- st_layers(dsn = "Data/ACP_2023/source_data/ACP_DesignTrans.gpkg")
trans <- st_read(dsn="Data/ACP_2023/source_data/ACP_DesignTrans.gpkg", 
                        layer = "ACP_2018_Transects") %>%
  st_transform(crs=4326) 
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(dat18, name="Bird Obs") + tm_dots(col="Transect") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()

#Now look just at transect 32 and 33
df <- dat18 |> filter(Transect %in% 32:33) |> mutate(Stratum = factor(Stratum))
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

df <- filter(dat18, Year == 2018, Stratum %in% c("10-02 Area", "Low")) 
table(df$Year, df$NavTransect)
table(df$Year, df$Transect)
tm_shape(acp) + tm_polygons(fill = "STRATNAME", fill_alpha = 0.5) +
  tm_shape(df, name="Bird Obs") + tm_dots(col="NavTransect") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()


library(tidyverse)
m <- 0.85
n <- 10000
se <- rep(c(0.01, 0.02, 0.04, 0.06), each = n)
Pop <- rnorm(n*4, 10000, 500)

df <- data.frame( Y = Pop*rnorm(4*n, m, se), ncol = 1)
df$Rep <- factor(rep(1:4, each = n))

ggplot(df, aes(x = Rep, y = Y, group = Rep)) + geom_violin()

df |> group_by(Rep) |> summarize(m = mean(Y), se = sd(Y))


dat <- read_csv(file = "Data/ACP_2023/analysis_output/Bird_QC_Obs.seat.stratum.2024.csv")

library(tmap)
library(sf)
acp <- st_read(dsn="Data/ACP_2023/source_data/ACP_DesignStrata.gpkg") %>%
  st_transform(crs=3338) 
df <- filter(dat, Year == 2016)
table(df$Observer, df$Seat)
table(df$Observer, df$Seat, df$Code)
ddf <- filter(df, Seat == "RF", NavTransect %in% c(19, 24)) |>
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) |>
  mutate(Code = factor(Code))
tm_shape(acp) + tm_polygons() +
  tm_shape(ddf) + tm_dots(fill = "Code") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#just code = 1
tm_shape(acp) + tm_polygons() +
  tm_shape(filter(ddf, Code == 1)) + tm_dots(fill = "Code") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
#plot
tmap_mode(mode = "view")
ddf <- filter(df, Observer == "WWL", NavTransect %in% c(11:12, 19, 24)) |>
  st_as_sf(coords=c("Lon", "Lat"), crs=4326) |>
  mutate(Code = factor(Code))
tm_shape(acp) + tm_polygons() +
  tm_shape(ddf) + tm_dots(fill = "Code") +
  tm_basemap(server = "Esri.WorldGrayCanvas") +
  tm_scalebar()
