#code to fit GAMs to SPEI and STEI, check model fit,
#  explore models and select best one for mapping/prediction
#  Also produces figure use in ABC and Ducks9 presentation 2024
#  model fit objects and figures are writen to .RData/RDS objects. 
#code to produce density data sets for USFWS are generated and written in the file 
#  write_map.R, which reads in the fit objects produced here
library(tidyverse)
library(sf)
#read in lines and birds data
lines <- st_read(dsn = "Data/ACP_2023/analysis_output/Lines_Obs_2.gpkg")
birds <- read_csv(file = "Data/ACP_2023/analysis_output/Bird_QC_Obs.seat.stratum.csv", 
                  col_types = "iiidcddcccdccdd") %>%
  # dplyr::mutate(
  #   # replace text and parenthesis
  #   geometry = stringr::str_replace(geometry, 'c\\(', ''),
  #   geometry = stringr::str_replace(geometry, '\\)', '')
  # ) %>%
  # # separate into lat and lon columns
  # tidyr::separate(geometry, into=c('Lon', 'Lat'), sep=', ') %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

acp <- st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")


ggplot(data = acp) + geom_sf() + geom_sf_text(aes(label = Stratum))
test<- select_area(area = acp, select = "all")
plot(st_geometry(test))

test <- select_area(area = acp, select = "all") %>%
  st_transform(crs=3338)
testgrid <- st_intersection(test, st_make_grid(x=test, cellsize = 6000)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
plot(st_geometry(testgrid))
#filter lines to one year and make segments
trans <- filter(lines, Year == 2023) %>%
  st_transform(crs=st_crs(3338))
seg <- make_segments(x = testgrid, y = trans)
#filter bird obs and associate to segments
dat <- filter(birds, Year == 2023)
df <- get_data(x = dat, y = seg, area = test, Spp = "GWFG", grid = testgrid)
#works!
#fit a GAM based on data frame
library(mgcv)
fit0 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)),
            offset = logArea, family = tw, method = "REML", data = df)
#worked!
#Now loop through year and combine to fit multi-year model with time trend
#pick year range
df <- data.frame(NULL)
years <- c(2017:2019, 2022, 2023)
for(i in years){
  trans <- filter(lines, Year == i) %>%
    st_transform(crs=st_crs(3338))
  seg <- make_segments(x = testgrid, y = trans)
  dat <- filter(birds, Year == i)
  dfy <- get_data(x = dat, y = seg, area = test, Spp = "GWFG", grid = testgrid)
  df <- rbind(df, dfy)
}
#fit GAM
fit0 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)),
            offset = logArea, family = nb, method = "REML", data = df)
#nb model seem slightly better based on qq plot, but nb and tw give very similar predictions
#use nb for GWFG for now
#try a model with time smooth
fit1 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 3),
            offset = logArea, family = nb, method = "REML", data = df)

#add all years and fit above plus st interaction
#Do this for SPEI
df <- data.frame(NULL)
years <- unique(birds$Year)
for(i in years){
  trans <- filter(lines, Year == i) %>%
    st_transform(crs=st_crs(3338))
  seg <- make_segments(x = testgrid, y = trans)
  dat <- filter(birds, Year == i)
  dfy <- get_data(x = dat, y = seg, area = test, Spp = "SPEI", grid = testgrid)
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
#AIC(fit0, fit1, fit2)
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
# > AIC(fit0, fit0.o, fit1, fit1.o, fit2, fit2.o)
# df      AIC
# fit0    85.77876 11492.83
# fit0.o  91.62503 11400.35
# fit1    89.94515 11370.37
# fit1.o  95.34782 11359.40
# fit2   112.29740 11350.49
# fit2.o 117.76715 11339.26
# Fit model with year random effects
df.re <- mutate(df, fYear = factor(Year))
fit2.re.o <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re") + 
                   s(fYear, bs = "re") + 
                   ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("ds", "cr"), 
                      m=list(c(1,.5),rep(0,0))),
                 offset = logArea, family = nb, method = "REML", data = df.re)
#seems to be working well
save.image(file = "SPEI_fit.RData")
##############################################
#Now make predictions and plot a map
map23 <- map_GAM(gamfit = fit2.o, grid = testgrid, Spp = "SPEI", Year = 2023)
print(map23[[1]][[1]])
print(map23[[1]][[2]])
map13 <- map_GAM(gamfit = fit2.o, grid = testgrid, Spp = "SPEI", Year = 2013)
print(map13[[1]][[1]])
print(map13[[1]][[2]])
#works!
#Now test population estimation function:
tot <- map_total(gamfit = fit2.o, grid = testgrid)
mean(tot)
#[1] 1110449855
#Not working!
#need to change unit of input grid to km^2
testgrid <- mutate(testgrid, Grid.Area = units::set_units(Grid.Area, km^2)) 
tot <- map_total(gamfit = fit2.o, grid = testgrid, Year = 2023)
mean(tot) 
# [1] 1118.486
#seems low
# filter(AKaerial::ACPHistoric$combined, Species == "SPEI")
# exp(coef(fit1))[1]*units::set_units(st_area(test), km^2)
# but about right!
sd(tot)
#try map_total2: predictions for each year 2007 to 2023
newdf <- st_centroid(testgrid) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(Observer = "HMW", Area = units::drop_units(testgrid$Grid.Area))
years <- 2007:2023
df <- data.frame(NULL)
for(i in years){
  df <- rbind(df, mutate(newdf, Year = i))
}
tot <- map_total2(gamfit = fit2.o, newdata = df)
#plot posterior
ddf <- data.frame(Year = years, IBP = apply(tot, 2, mean), 
                  upper=apply(tot, 2, quantile, probs = 0.975), 
                  lower = apply(tot, 2, quantile, probs=0.025), type = "Model")
design.est <- #AKaerial::ACPHistoric$output.table %>%
  read_csv(file = "Data/ACP_2023/analysis_output/ACP2007to2023Output.csv") %>%
  filter(Species == "SPEI") %>%
  rename(ibb = ibbtotal.est, ibb.var = var.Nib) %>%
  mutate(IBP = ibb/2, upper = ibb/2 + 2*sqrt(ibb.var/4), lower = ibb/2 - 2*sqrt(ibb.var/4), 
         type = "Design") %>%
  select(Year, Observer, IBP, upper, lower, type)
ggplot(data = ddf) + 
  geom_ribbon(aes(x=Year, ymin=lower, ymax=upper), fill = "orange") +
  geom_line(aes(x=Year, y=IBP)) + 
  geom_pointrange(data = design.est, aes(x=Year, y=IBP, ymin=lower, ymax=upper, 
                                         color = Observer),
                  position = position_jitter(width = 0.1, height = 0)) +
  
  labs(title = "SPEI")
#add posterior lines to background
ddf2 <- as.data.frame(tot) %>% rename_with(~as.character(years)) %>%
  slice_sample(n = 500) %>%
  mutate(Sample = row_number()) %>%
  pivot_longer(cols = "2007":"2023", names_to = "Year", values_to = "IBP") %>%
  mutate(Type = "Posterior", Year = as.numeric(Year))

ggplot() +
  #geom_line(data = ddf2, aes(x = Year, y = IBP, group = Sample), col = "black", alpha = 0.25) + 
  #geom_point(data = ddf2, aes(x = Year, y = IBP, group = Sample), col = "black", alpha = 0.25) +
  #geom_point(data = design.est, aes(x = Year, y = IBP, group = Observer), col = "black", alpha = 0.25) +
  geom_ribbon(data = ddf, aes(x=Year, ymin=lower, ymax=upper), fill = "orange", alpha = 0.25) +
  geom_line(data = ddf, aes(x=Year, y=IBP), col = "white", lwd = 1) + 
  labs(title = "SPEI on ACP")
################################################################################
#Plot model with random year and observer effect, fit2.re.o
newdf <- st_centroid(testgrid) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(Area = units::drop_units(testgrid$Grid.Area))
yobsdf <- df.re %>% select(Year, fYear, Observer) %>%
  distinct()
df <- data.frame(NULL)
for(i in 1:dim(yobsdf)[1]){
  df <- rbind(df, mutate(newdf, Year = yobsdf$Year[i], fYear = yobsdf$fYear[i], 
                         Observer = yobsdf$Observer[i]))
}
tot <- map_total3(gamfit = fit2.re.o, newdata = df, yearobs = dim(yobsdf)[1])
ddf.re <- mutate(yobsdf, IBP = apply(tot, 2, mean), 
                 upper=apply(tot, 2, quantile, probs = 0.975), 
                 lower = apply(tot, 2, quantile, probs=0.025), type = "Model")
ggplot() +
  geom_ribbon(data = ddf, aes(x=Year, ymin=lower, ymax=upper), fill = "orange", alpha = 0.25) +
  geom_pointrange(data = ddf.re, aes(x = Year, y = IBP, ymin = lower, ymax = upper, group = Observer),
                  col = "black", lwd = 1, position = position_dodge(width = 0.25)) +
  # geom_pointrange(data = ddf.re, aes(x = Year, y = IBP, ymin = lower, ymax = upper, col = Observer), 
  #                 lwd = 1, position = position_dodge(width = 0.25)) +
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 8000, by = 2000)) + 
  scale_x_continuous(breaks = seq(2007, 2023, by = 2)) +
  labs(title = "SPEI on ACP")
save.image("SPEI_figs.RData")
################################################################################
#Plot observer effects
pick <- stringr::str_detect(names(coef(fit2.o)), "Observer")
obsdf <- data.frame(Observer = levels(factor(birds$Observer)), 
                    Effect = coef(fit2.o)[pick], 
                    SE.Effect = NA)
ggplot(data = obsdf) +
  geom_point(aes(x = Observer, y = Effect))
################################################################################
## Plot space-time change, wrap into function?
diff1 = log(map23$preds$fit) - log(map13$preds$fit)
diff2 = map23$preds$fit / map13$preds$fit
stmapdf <- cbind(diff1, diff2, testgrid)
ggplot(data = stmapdf) + geom_sf(aes(fill=diff1)) +
  scale_fill_viridis_c(name = "log(Change)") +
  labs(title = "2023 - 2013")
ggplot(data = stmapdf) + geom_sf(aes(fill=diff2)) +
  scale_fill_viridis_c(name = "% Change") +
  labs(title = "2023 - 2013")
##############################################
#what about Zero inflated models
fit.zero <- gam(list(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re") + 
                       s(Year, k = 10), ~s(X, Y, bs="ds", k = 200, m=c(1,.5))),
                offset = logArea, family = ziplss(), method = "REML", data = df)
summary(fit.zero)

#make covariates
#not implemented
# covs <- seg %>% st_centroid() %>%
#   st_join(st_transform(acp, crs=3338), join=st_nearest_feature) %>%
#   select(Sample.Label, STRATNAME)
# test <- get_data(obs = dat, design = testarea, SPP = "SPEI", 
#                  grid = testgrid, buff = 1000, seg = seg, covs = covs, select = "all")

###############################################
## test out DHARMa package for residual plots
# library(DHARMa)
# library(mgcViz)
# testDispersion(fit2.o)
# simulationOutput <- simulateResiduals(fittedModel = fit2.o, n = 500)
# plot(simulationOutput)
# plotResiduals(simulationOutput)
# testZeroInflation(simulationOutput)
# #resid <- recalculateResiduals(simulationOutput, group = df$Sample.Label)
# #testSpatialAutocorrelation(resid, x=$X, y = df$Y) #see error message, need to recalculate resid and arrage by group
# #looks good!
# library(gratia)
# appraise(fit2.o)
# draw(fit2.o, select = 1, dist = 0.02, rug = FALSE)
# draw(fit2.o, select = 3, rug = FALSE)
# #try 2D check
# b <- getViz(fit2.o, nsim = 50)
# ck1 <- check2D(b, x1 = "X", x2 = "Y")
# ck1 + l_gridCheck2D(gridFun = mean)
# ################################################
# #now try fit.zero
# testDispersion(fit.zero)
# simulationOutput <- simulateResiduals(fittedModel = fit.zero)
# plot(simulationOutput)
# testZeroInflation(simulationOutput)
# #also looks good
# #################################################
# # Can we do predictions at a finer scale? try 1 km^2
# testgrid <- st_intersection(test, st_make_grid(x=test, cellsize = 1000)) %>%
#   mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
# testgrid <- mutate(testgrid, Grid.Area = units::set_units(Grid.Area, km^2))
# map23.1m <- map_GAM(gamfit = fit2.o, grid = testgrid, Spp = "SPEI", Year = 2023)
# print(map23.1m[[1]][[1]])
# print(map23.1m[[1]][[2]])
################################################################################
## Steller's eider
df <- data.frame(NULL)
years <- unique(birds$Year)
for(i in years){
  trans <- filter(lines, Year == i) %>%
    st_transform(crs=st_crs(3338))
  seg <- make_segments(x = testgrid, y = trans)
  dat <- filter(birds, Year == i)
  dfy <- get_data(x = dat, y = seg, area = test, Spp = "STEI", grid = testgrid)
  df <- rbind(df, dfy)
}
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
fit0 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)),
            offset = logArea, family = nb, method = "REML", data = df)
fit1 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 10),
            offset = logArea, family = nb, method = "REML", data = df)
fit2 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 10) + 
              ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("ds", "cr"), 
                 m=list(c(1,.5),rep(0,0))),
            offset = logArea, family = nb, method = "REML", data = df)
AIC(fit0, fit0.o, fit1, fit1.o, fit2, fit2.o)
# df      AIC
# fit0   22.48402 465.9433
# fit0.o 22.48619 465.9471
# fit1   29.49496 460.2039
# fit1.o 29.49566 460.2053
# fit2   29.49633 460.2062
# fit2.o 29.49674 460.2070
save.image("STEI_fit.RData")
#load and print map
#load("STEI_fit.RData")
#plot map
map23 <- map_GAM(gamfit = fit1, grid = testgrid, Spp = "STEI", Year = 2023)
print(map23[[1]][[1]])
print(map23[[1]][[2]])
#plot trend
newdf <- st_centroid(testgrid) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(Observer = "HMW", Area = units::drop_units(testgrid$Grid.Area))
years <- 2007:2023
df <- data.frame(NULL)
for(i in years){
  df <- rbind(df, mutate(newdf, Year = i))
}
tot <- map_total2(gamfit = fit1, newdata = df)
#plot posterior
ddf <- data.frame(Year = years, IBP = apply(tot, 2, mean), 
                  upper=apply(tot, 2, quantile, probs = 0.975), 
                  lower = apply(tot, 2, quantile, probs=0.025), type = "Model")
design.est <- #AKaerial::ACPHistoric$output.table %>%
  read_csv(file = "Data/ACP_2023/analysis_output/ACP2007to2023Output.csv") %>%
  filter(Species == "STEI") %>%
  rename(ibb = ibbtotal.est, ibb.var = var.Nib) %>%
  mutate(IBP = ibb/2, upper = ibb/2 + 2*sqrt(ibb.var/4), lower = ibb/2 - 2*sqrt(ibb.var/4), 
         type = "Design") %>%
  select(Year, Observer, IBP, upper, lower, type)
ggplot(data = ddf) + 
  geom_ribbon(aes(x=Year, ymin=lower, ymax=upper), fill = "orange") +
  geom_line(aes(x=Year, y=IBP)) + 
  geom_pointrange(data = design.est, aes(x=Year, y=IBP, ymin=lower, ymax=upper, 
                                         color = Observer),
                  position = position_jitter(width = 0.1, height = 0)) +
  
  labs(title = "STEI")
#add posterior lines
ddf2 <- as.data.frame(tot) %>% rename_with(~as.character(years)) %>%
  slice_sample(n = 100) %>%
  mutate(Sample = row_number()) %>%
  pivot_longer(cols = "2007":"2023", names_to = "Year", values_to = "IBP") %>%
  mutate(Type = "Posterior", Year = as.numeric(Year))

#cols <- c("Sample"="lightgray","Mean"="white","95% CI"="orange")
ggplot() +
  geom_line(data = ddf2, aes(x = Year, y = IBP, group = Sample), col = "black", alpha = 0.25) + 
  #geom_point(data = ddf2, aes(x = Year, y = IBP, group = Sample), col = "black", alpha = 0.25) +
  #geom_point(data = design.est, aes(x = Year, y = IBP, group = Observer), col = "black", alpha = 0.25) +
  geom_ribbon(data = ddf, aes(x=Year, ymin=lower, ymax=upper), fill = "orange", alpha = 0.25) +
  geom_line(data = ddf, aes(x=Year, y=IBP), col = "white", lwd = 1) +
  coord_cartesian(ylim=c(0, 1500)) +
  #scale_colour_manual(name="Type",values=cols) + #not working??
  #scale_fill_manual(name="95% CI",values=cols) +
  labs(title = "STEI on ACP")
