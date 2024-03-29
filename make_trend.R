#write predict population total and calculate time-specific trend
#  predictions to output files/database
#should make this a function but for now, with only one species, will wait
make_trend <- function(fit = NA, area = NA, cs = 1000, n.samples = 500){
library(tidyverse)
library(sf)
source("map_density_functions.R")
#fit <- readRDS("SPEI_fit.RDS")
#make new grid
#acp <- st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#make grid
area <- select_area(area = area, select = "all") %>%
  st_transform(crs=3338)
grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = cs)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
#need to change unit of input grid to km^2
grid <- mutate(grid, Grid.Area = units::set_units(Grid.Area, km^2)) 
#make new data frame for predictions
newdf <- st_centroid(grid) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(Observer = "HMW", Area = units::drop_units(grid$Grid.Area))
df <- data.frame(NULL)
for(i in min(fit$model$Year):max(fit$model$Year)){
  df <- rbind(df, mutate(newdf, Year = i))
}
#simulation posterior
post <- map_total2(gamfit = fit, Nsamples = n.samples, newdata = df)
#summarize
df <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
                 Mean = apply(post, 2, mean), 
                 sd = apply(post, 2, sd), 
                 median = apply(post, 2, median), 
                 upper = apply(post, 2, quantile, probs = 0.975),
                 lower = apply(post, 2, quantile, probs = 0.025))
#plot
gg <- ggplot(data = df) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  scale_x_continuous(breaks = seq(2007, 2025, by = 2)) + 
  ylab("Estimate")
  
#print(gg)
#cycle through lags and compute trend
#Now make a data frame of the posterior "trend" for each lag time period
#here x-year trend is measured as [N_a/N_b]^(1/(a - b)), where a is the current year
#  and b is any other year, b < a
trendDF <- data.frame(NULL) 
repDF <- as.data.frame(post)
names(repDF) <- as.character(min(fit$model$Year):(min(fit$model$Year) + dim(post)[2] - 1))
repDF <-  repDF |> 
  pivot_longer(cols = 1:dim(post)[2], names_to = "Year", values_to = "Estimate") |>
  arrange(Year) |>
  group_by(Year) |>
  mutate(Rep = row_number(Year)) |>
  ungroup()
for(i in 1:(dim(post)[2] - 1)){
  temp <- repDF |> arrange(Rep, Year) |> group_by(Rep) |>
    mutate(Lag = i, 
           temp = lag(Estimate, i, order_by = Year), 
           Trend = (Estimate/temp)^(1/Lag)) |>
    filter(Year == max(repDF$Year)) |>
    ungroup() |>
    select(Year, Lag, Trend)
  trendDF <- rbind(trendDF, temp)
}
#hist(filter(trendDF, Lag == 13)$Trend)
#summarize and plot
sumTrend <- trendDF |> group_by(Lag) |>
  summarise(mean = mean(Trend), 
            median = quantile(Trend, probs = 0.5), 
            lower = quantile(Trend, probs = 0.025), 
            upper = quantile(Trend, probs = 0.975))
#write_csv(sumTrend, file =  paste0(path, "plot_trends/data/", spp, "_trend.csv"))
p2 <- ggplot(data = sumTrend) + 
  geom_ribbon(aes(x=Lag, ymin=lower, ymax = upper), fill = "orange") + 
  geom_line(aes(x = Lag, y = median), col = "darkgray", lwd = 1) + 
  geom_line(aes(x = Lag, y = mean), col = "red", lwd = 1) + 
  geom_hline(yintercept = 1) + 
  labs(y = "Growth rate")

return(list(pop.data = df, pop.plot = gg, 
            trend.data = sumTrend, trend.plot = p2))
}
#test it
# test <- make_trend(fit = readRDS("SPEI_fit.RDS"), 
#                     area = st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg"), 
#                     n.samples = 100)
#worked!