# Perform sensitivity analysis for the quantile used to estimate 
# the top of the sphere simultaneously with the cutoff value k
# to determine whether someone has removed their device.

# Vary |H_i| from 0.9 T_i ... 0.99 T_i
# check angular change in upright orientation
# check change in classifications

###########################################################
# Check Angular Change in Upright Orientation
rm(list = ls())
data.dir <- file.path("Data/Data_SMASH_ZIO/OneMinute_Data_2021-06-21")
raw.file <- dir(data.dir,full.names = T)

## Packages
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)

chord2theta <- function(chord) 2*asin(chord/2)/pi*180

df <- tibble()

for(i in raw.file){
      ## Grab one raw data file
     min.data <- read.csv(i)
     
     df <-
     min.data %>%
     mutate(down0 = as.numeric(theta > 45)) %>%
     group_by(wear.bout, cluster.meanshift.14) %>%
     mutate(down.meanshift.14 = down0 | (median(theta)>45)) %>%
     ungroup %>%
     group_by(wear.bout, cluster.centroid7) %>%
     mutate(down.centroid7 = down0 | (median(theta)>45)) %>%
     ungroup %>%
     group_by(wear.bout, cluster.ward5) %>%
     mutate(down.ward5 = down0 | (median(theta)>45)) %>%
     ungroup %>%
     summarise(msc = mean(down.meanshift.14, na.rm=T),
               chc = mean(down.centroid7, na.rm=T),
               whc = mean(down.ward5, na.rm=T),
               msc.chc = mean(down.meanshift.14*down.centroid7, na.rm=T) +  
                         mean((1-down.meanshift.14)*(1-down.centroid7), na.rm=T),
               msc.whc = mean(down.meanshift.14*down.ward5, na.rm=T) +  
                         mean((1-down.meanshift.14)*(1-down.ward5), na.rm=T),
               chc.whc = mean(down.centroid7*down.ward5, na.rm=T) +  
                         mean((1-down.centroid7)*(1-down.ward5), na.rm=T)) %>%
     bind_rows(df)
}

summary(df$msc.chc)
summary(df$msc.whc)
summary(df$chc.whc)

#> summary(df$msc.chc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.8691  0.9818  0.9941  0.9821  0.9971  0.9995 
#> summary(df$msc.whc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.8839  0.9949  0.9981  0.9882  0.9995  1.0000 
#> summary(df$chc.whc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.8695  0.9853  0.9934  0.9741  0.9985  0.9996 
