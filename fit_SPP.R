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
#select species
spp <- "STEI"
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
  dfy <- get_data(x = dat, y = seg, area = acp, Spp = spp, grid = grid)
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
#add random effect of year
df$fYear <- factor(df$Year)
fit0.re <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + 
                 s(fYear, bs = "re"),
               offset = logArea, family = nb, method = "REML", data = df)
fit1.re <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 10) + 
                 s(fYear, bs = "re"),
            offset = logArea, family = nb, method = "REML", data = df)
fit2.re <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 10) + 
                 s(fYear, bs = "re") + 
                 ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("ds", "cr"), 
                 m=list(c(1,.5),rep(0,0))),
            offset = logArea, family = nb, method = "REML", data = df)
fit0.o.re <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re") + 
                s(fYear, bs = "re"),
              offset = logArea, family = nb, method = "REML", data = df)
fit1.o.re <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re") + 
                s(fYear, bs = "re") + 
                s(Year, k = 10),
              offset = logArea, family = nb, method = "REML", data = df)
fit2.o.re <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re") + 
                s(fYear, bs = "re") + 
                s(Year, k = 10) + 
                ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("ds", "cr"), 
                   m=list(c(1,.5),rep(0,0))),
              offset = logArea, family = nb, method = "REML", data = df)
aic <- AIC(fit0, fit0.o, fit1, fit1.o, fit2, fit2.o, fit0.re, fit1.re, fit2.re,
           fit0.o.re, fit1.o.re, fit2.o.re)
print(aic)
# df      AIC
# fit0   22.48402 465.9433
# fit0.o 22.48523 465.9455
# fit1   29.49496 460.2039
# fit1.o 29.49591 460.2058
# fit2   29.49633 460.2062
# fit2.o 29.49611 460.2059
best <- get(row.names(aic)[which(aic$AIC == min(aic$AIC))])
################################################
saveRDS(best, file = paste0(spp, "_fit.RDS"))

# #check_fit <- function(best = NULL){
#   library(mgcv)
#   library(DHARMa)
#   library(mgcViz)
#   library(gratia)
# #check fit
# summary(best)
# gam.check(best)
# qq.gam(best, rep = 100)
# #############################################
# # DHARMa package for residual plots
# 
# testDispersion(best)
# simulationOutput <- simulateResiduals(fittedModel = best, n = 500)
# plot(simulationOutput)
# plotResiduals(simulationOutput)
# testZeroInflation(simulationOutput)
# #looks good!
# #resid <- recalculateResiduals(simulationOutput, group = df$Year)
# #testSpatialAutocorrelation(resid, x=df$X, y = df$Y) #see error message, need to recalculate resid and arrage by group
# # can't get Spatial Autocorrelation to work, might be some issue with fit
# 
# appraise(best)
# draw(best, select = 1, dist = 0.02, rug = FALSE)
# draw(best, select = 2, rug = FALSE)
# #try 2D check
# b <- getViz(best, nsim = 50)
# ck1 <- check2D(b, x1 = "X", x2 = "Y")
# ck1 + l_gridCheck2D(gridFun = mean)
# #looks OK

library(quarto)
# save.image("temp.RData")
# quarto_render("model_checking.qmd", output_format = "html")

results = list(spp = spp, aic = aic, best = best)
saveRDS(results, file = "temp.RDS")
# quarto_render("model_checking.qmd", output_format = "html")
quarto_render("model_checking.qmd", output_format = "html",
              output_file = paste0(spp, "_model_checking.html"))
file.rename(from = paste0(spp, "_model_checking.html"), 
            to = paste0("Data/ACP_2023/analysis_output/gam/", spp, 
                        "_model_checking.html"))
