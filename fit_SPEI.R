#based off of code in map_eiders.R
#this script produces species-specific model fitting, checking and selection 
#  used for mapping and trend estimation
#needs bird observations, flight lines, survey area grid 
#writes a mgcv::gam fit object to be used for mapping

#should try to make this a function...

library(tidyverse)
library(sf)
library(mgcv)
source("map_density_functions.R")
#read in lines and birds data
lines <- st_read(dsn = "Data/ACP_2023/analysis_output/Lines-Obs-2024-02-15.gpkg")
birds <- read_csv(file = "Data/ACP_2023/analysis_output/Bird-QC-Obs-2024-03-21.csv") %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
acp <- st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#make grid
acp <- select_area(area = acp, select = "all") %>%
  st_transform(crs=3338)
grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = 6000)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
#loop through all years and fit models
#Do this for SPEI
df <- data.frame(NULL)
years <- unique(birds$Year)
for(i in years){
  trans <- filter(lines, Year == i) %>%
    st_transform(crs=st_crs(3338))
  seg <- make_segments(x = grid, y = trans)
  dat <- filter(birds, Year == i)
  dfy <- get_data(x = dat, y = seg, area = acp, Spp = "SPEI", grid = grid)
  df <- rbind(df, dfy)
}
#fit GAM
fit0 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)),
            offset = logArea, family = nb, method = "REML", data = df)
fit1 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 10),
            offset = logArea, family = nb, method = "REML", data = df)
fit2 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 10) + 
              ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("ds", "cr"), 
                 m=list(c(1,.5),rep(0,0))),
            offset = logArea, family = nb, method = "REML", data = df)
#fit models with Observer covariate
df$Observer <- factor(df$Observer)
fit0.o <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re"),
              offset = logArea, family = nb, method = "REML", data = df)
fit1.o <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re") + 
                s(Year, k = 10),
              offset = logArea, family = nb, method = "REML", data = df)
fit2.o <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re") + 
                s(Year, k = 10) + 
                ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("ds", "cr"), 
                   m=list(c(1,.5),rep(0,0))),
              offset = logArea, family = nb, method = "REML", data = df)
AIC(fit0, fit0.o, fit1, fit1.o, fit2, fit2.o)
# df      AIC
# fit0    85.77876 11492.83
# fit0.o  90.77274 11398.49
# fit1    89.94515 11370.37
# fit1.o  94.94982 11358.08
# fit2   112.29740 11350.49
# fit2.o 117.36230 11337.94
#check fit
summary(fit2.o)
gam.check(fit2.o)
#############################################
# DHARMa package for residual plots
library(DHARMa)
library(mgcViz)
testDispersion(fit2.o)
simulationOutput <- simulateResiduals(fittedModel = fit2.o, n = 500)
plot(simulationOutput)
plotResiduals(simulationOutput)
testZeroInflation(simulationOutput)
#looks good!
resid <- recalculateResiduals(simulationOutput, group = df$Year)
#testSpatialAutocorrelation(resid, x=df$X, y = df$Y) #see error message, need to recalculate resid and arrage by group
# can't get Spatial Autocorrelation to work, might be some issue with fit
library(gratia)
appraise(fit2.o)
draw(fit2.o, select = 1, dist = 0.02, rug = FALSE)
draw(fit2.o, select = 3, rug = FALSE)
#try 2D check
b <- getViz(fit2.o, nsim = 50)
ck1 <- check2D(b, x1 = "X", x2 = "Y")
ck1 + l_gridCheck2D(gridFun = mean)
#looks OK
################################################
saveRDS(fit2.o, file = "SPEI_fit.RDS")
