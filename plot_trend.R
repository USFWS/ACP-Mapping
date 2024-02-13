library(tidyverse)
library(AKaerial)
library(ggpubr)
library(units)
plot_trend <- function(data = NULL, Spp = "SPEI"){
  library(tidyverse)
  library(AKaerial)
  if(is.null(data)) data <- ACPHistoric$output.table
  ddf <- data %>%
    filter(Species == Spp)
  ggplot(data = ddf) + 
    geom_smooth(aes(x=Year, y=total.est), method = "gam", formula = y ~ s(x, k = 10)) + 
    geom_point(aes(x=Year, y = total.est, col = Observer)) + 
    labs(title = Spp) + 
    ylab("Estimate Total")
}  

species <- unique(ACPHistoric$output.table$Species)  
plots <- list()
for(i in species){
  plots[[i]] <- plot_trend(data = ACPHistoric$output.table, Spp = i)
}
seathings <- c("SPEI", "KIEI", "YBLO", "RTLO", "LTDU", "JAEG")
ggarrange(plotlist = plots[seathings], legend = "top", common.legend = TRUE)
goosethings <- c("GWFG", "CCGO", "BRAN", "SNGO", "TUSW")
ggarrange(plotlist = plots[goosethings], legend = "top", common.legend = TRUE)

Spp = "SPEI"
dddf <- read_csv(file = "Data/ACP_2023/analysis_output/ACP2007to2023Combined.csv") %>%
  filter(Species == Spp & Year >= 2014)
fit <- lm(log(itotal) ~ I(Year - 2014), data = dddf)
summary(fit)
exp(coef(fit))
Spp = "YBLO"
dddf <- read_csv(file = "Data/ACP_2023/analysis_output/ACP2007to2023Combined.csv") %>%
  filter(Species == Spp & Year >= 2014)
fit <- lm(log(itotal) ~ I(Year - 2014), data = dddf)
summary(fit)
exp(coef(fit))

