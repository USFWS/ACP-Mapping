################################################################################
#This is based off of plot_trend_ducks_dev.R, but for ACP
# include it here just for dev purposes
##Now lets loop through all species and write figures and data to a directory
#will try to make this a function at some point
#write error and messages to log file 
zz <- file("plot_trends/data/log.txt", open="wt")
sink(zz ,type = "output")
sink(zz, type = "message")

path = NULL
exclude.spp = c("CORA", "SWANN", "UNGR", "UNME", "UNEI", "BAEA", "COME")
area = "ACP"
library(AKaerial)
library(tidyverse)
library(mgcv)

if(area == "ACP") data <- ACPHistoric$output.table
if(area == "YKD") data <- YKDHistoric$output.table
df <- data |>
  filter(! Species %in% c("START", "END", exclude.spp)) |>
  mutate(Species = factor(Species), ibbtotal.est = units::drop_units(ibbtotal.est), 
         cv.ibb = SE.ibb/ibbtotal.est) |>
  select(Year, Species, Observer, ibbtotal.est, SE.ibb, cv.ibb) |>
  drop_na() |>
  mutate(upper = ibbtotal.est + 2*SE.ibb, lower = ibbtotal.est - 2*SE.ibb, 
         Observer = factor(Observer))
#start loop
for(spp in levels(df$Species)){
  print(spp)
  #select species and do sims
  #spp <- "SPEI"
  sppdf <- df |> filter(Species == spp)
  yearobs <- length(unique(sppdf$Year))
  NumObs <- length(unique(sppdf$Observer)) 
  kyear <- ifelse(length(sppdf$Year) > yearobs - 1 + NumObs, 
                  yearobs - 1, yearobs - 1 - NumObs)  
  # print(data.frame(Species = spp, yearobs = yearobs, NumObs = NumObs, 
  #                  kyear = kyear, nobs = dim(sppdf)[1]))
  if(kyear <= 0 | NumObs + kyear > dim(sppdf)[1] ){
    message(paste("Not enough data to fit to", spp))
    next()
  }
  if(kyear < yearobs -1) message(paste("Number of temporal knots for", spp,"restricted to", kyear))
  nreps = 200
  results <- w <- vector("list", nreps)
  newdf <- sppdf |> mutate(Observer = "HMW") |> select(Year, Species, Observer) |> 
    distinct()
  for(i in 1:nreps){
    #need to catch NaNs for rare or poorly estimated species
    sppdf$est <- log(rnorm(dim(sppdf)[1], mean = sppdf$ibbtotal.est, sd = sppdf$SE.ibb))
    sppdf$est <- if_else(is.na(sppdf$est), 1e-10, sppdf$est)
    bootfit <- gam(est ~ s(Year, bs = "tp", k = kyear) +
                     s(Observer, bs = "re"),
                   method = "REML", data = sppdf, family = scat)
    #sample from posterior
    b <- rmvn(1, coef(bootfit), vcov(bootfit))
    Xp <- predict(bootfit, type="lpmatrix", newdata=newdf, exclude = "s(Observer)", 
                  newdata.guaranteed=TRUE)
    # w[[i]] <- warnings()
    # print(paste("iteration", i))
    # print(paste("Number of iterations for fit", bootfit$iter))
    # print(warnings())
    results[[i]] <- exp(Xp%*%b)
  }
  names(results) <- paste0(rep("rep", length(results)), 1:nreps)
  repdf <- cbind(newdf, as.data.frame(results)) |>
    pivot_longer(4:(4+nreps-1), names_to = "Rep", values_to = "results") 
  # ggplot(data = repdf) + 
  #   geom_line(aes(x = Year, y = results, group = Rep), alpha = 0.25) + 
  #   geom_pointrange(data = sppdf, aes(x = Year, y = ibbtotal.est, ymin = lower, ymax = upper, 
  #                                     col = Observer))
  #fit at point estimate to compare
  # sppdf$logest <- log(sppdf$ibbtotal.est)
  # fit1 <- gam(logest ~ s(Year, bs = "tp", k = kyear) +
  #               s(Observer, bs = "re"),
  #             method = "REML", data = sppdf, family = scat)
  # preds <- predict(fit1, newdata = newdf, exclude = "s(Observer)", se.fit = TRUE)
  # fitdf <- cbind(newdf, preds) |> mutate(upper = fit + 2*se.fit, lower = fit - 2*se.fit) |>
  #   mutate(fit = exp(fit), upper = exp(upper), lower = exp(lower))
  # ggplot() + 
  #   geom_ribbon(data = fitdf, aes(x=Year, ymin=lower, ymax = upper, fill = "orange")) + 
  #   geom_line(data = repdf, aes(x = Year, y = results, group = Rep), alpha = 0.25) + 
  #   geom_line(data = fitdf, aes(x = Year, y = fit), col = "white", lwd = 1) + 
  #   geom_pointrange(data = sppdf, aes(x = Year, y = ibbtotal.est, ymin = lower, ymax = upper, 
  #                                     col = Observer))
  #Now use results to find mean and SD at each time point
  sumdf <- repdf |> group_by(Year) |>
    summarise(mean = mean(results), sd = sd(results)) |>
    mutate(upper = mean + 2*sd, lower = mean - 2*sd)
  write_csv(sumdf, file =  paste0(path, "plot_trends/data/", spp, "_bootfit.csv"))
  p1 <- ggplot() + 
    geom_ribbon(data = sumdf, aes(x=Year, ymin=lower, ymax = upper, fill = "orange"), 
                show.legend = FALSE) + 
    geom_line(data = repdf, aes(x = Year, y = results, group = Rep), alpha = 0.25) + 
    geom_line(data = sumdf, aes(x = Year, y = mean), col = "white", lwd = 1) + 
    geom_pointrange(data = sppdf, aes(x = Year, y = ibbtotal.est, ymin = lower, ymax = upper, 
                                      col = Observer), show.legend = FALSE) + 
    labs(title = paste(spp, "on", area), y = "IBBtotal")
  png(filename = paste0(path, "plot_trends/figures/", spp, "_index.png"))
  print(p1)
  dev.off()
  #Now make a data frame of the posterior "trend" for each lag time period
  #here x-year trend is measured as [N_a/N_b]^(1/(a - b)), where a is the current year
  #  and b is a any other year, b < a
  trendDF <- data.frame(NULL)
  for(i in 1:(length(unique(repdf$Year)) - 1)){
    temp <- repdf |> arrange(Rep, Year) |> group_by(Rep) |>
      mutate(Lag = i, 
             temp = lag(results, i, order_by = Year), 
             Trend = (results/temp)^(1/Lag)) |>
      filter(Year == max(repdf$Year)) |>
      ungroup() |>
      select(Year, Species, Lag, Trend)
    trendDF <- rbind(trendDF, temp)
  }
  #hist(filter(trendDF, Lag == 35)$Trend)
  #summarize and plot
  sumTrend <- trendDF |> group_by(Lag) |>
    summarise(mean = mean(Trend), 
              median = quantile(Trend, probs = 0.5), 
              lower = quantile(Trend, probs = 0.025), 
              upper = quantile(Trend, probs = 0.975))
  write_csv(sumTrend, file =  paste0(path, "plot_trends/data/", spp, "_trend.csv"))
  p2 <- ggplot(data = sumTrend) + 
    geom_ribbon(aes(x=Lag, ymin=lower, ymax = upper), fill = "orange") + 
    geom_line(aes(x = Lag, y = median), col = "darkgray", lwd = 1) + 
    geom_line(aes(x = Lag, y = mean), col = "red", lwd = 1) + 
    geom_hline(yintercept = 1) + 
    labs(y = "Growth rate", title = paste(spp, "on", area))
  png(filename = paste0(path, "plot_trends/figures/", spp, "_trend.png"))
  print(p2)
  dev.off()
}
sink(type = "output")
sink(type="message")
close(zz)
