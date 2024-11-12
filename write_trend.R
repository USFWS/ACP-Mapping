#write results from make_trend to file
library(tidyverse)
source("make_trend.R")
spp = "SNOW"
trend <- make_trend(fit = best, 
                    area = st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg"), 
                    n.samples = 500)
#should make a function for for now, 2024=03-29, just write results for SPEI

write_csv(trend$trend.data, file = paste0("Data/ACP_2023/analysis_output/trends/", spp, 
                                          "-trend-", Sys.Date(), ".csv"))
write_csv(trend$pop.data, file = paste0("Data/ACP_2023/analysis_output/trends/", spp, 
                                        "-pop-", Sys.Date(), ".csv"))
ggsave(plot = trend$trend.plot, file = paste0("Data/ACP_2023/analysis_output/trends/", 
                                              spp, "-trend-", Sys.Date(), ".png"))
ggsave(plot = trend$pop.plot, file = paste0("Data/ACP_2023/analysis_output/trends/", 
                                            spp, "-pop-", Sys.Date(), ".png"))
