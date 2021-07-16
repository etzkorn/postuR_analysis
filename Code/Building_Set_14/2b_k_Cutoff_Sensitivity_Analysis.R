# Perform sensitivity analysis for the types of clustering/parameters
# identified as best in Appendix 1: 1_Clustering_Algorithm_Choice.R.

# Mean Shift Clustering with window of 0.14
# Centroid Heirarchical Clustering cut at 7 clusters
# Ward Heirarchical Clustering cut at 5 clusters

###########################################################
# Check Angular Change in Upright Orientation
rm(list = ls())
raw.file <- dir("Data/Data_SMASH_ZIO/Full_Data_2019-04-26_15:15:35/", full.names = T)
raw.file <- raw.file[grepl("Full_Data", raw.file) & grepl(".rdata", raw.file)]

## Packages
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(movMF)
source("../Zio_Package/postuR/R/calculate_removal_time.R")
source("../Zio_Package/postuR/R/check_nonwear.R")
source("../Zio_Package/postuR/R/find_top.R")
source("../Zio_Package/postuR/R/mad.R")
source("../Zio_Package/postuR/R/euclid_norm.R")
source("../Zio_Package/postuR/R/process_zacl.R")

chord2theta <- function(chord) 2*asin(chord/2)/pi*180

top.data <- tibble()
max.theta <- c()

for(i in raw.file){
      ## Grab one raw data file
      load(i)

      # get centers for each quantile
      for(qj in seq(0.9, 0.99, by = 0.01)){
            top.data.i <- 
            data %>%
            mutate(wear = T) %>%
            calculate.removal.time(q = qj, multiple.removals = F)%>%
            mutate(id = data$id[1],
                   q = qj)
            top.data <- bind_rows(top.data, top.data.i)
      }
}

#############################################################
# Check Changes in ratio of mean resultant length

png(filename = "Figures/2_Appendix_Images/Sensitivity_Percentile_RRatio.png",
    width = 800, height = 600)
top.data %>%
      mutate(p = 100*q,
             change = ifelse(id %in% c("150831_N509676032",
                                "150911_N509606040",
                                "151102_N520912069"),
                             "Yes",
                             "No")) %>%
      ggplot() +
      geom_path(aes(x = p,
                    y = (r.ratio), group = id, color = change)) +
      geom_point(aes(x = p,
                     y = (r.ratio), group = id, color = change)) +
      scale_x_continuous("Percentile",breaks = 90:99,minor_breaks = NULL, expand = c(0.01,0.01)) +
      scale_y_continuous("Mean Resultant Length Ratio", expand = c(0.01,0.01))+
      theme_bw(30) +
      theme(legend.position = "bottom") +
      geom_hline(yintercept = 0.95, linetype = 2) +
      scale_color_manual("Suspected Removal/Replacement", values = c("red", "blue"))
dev.off()
