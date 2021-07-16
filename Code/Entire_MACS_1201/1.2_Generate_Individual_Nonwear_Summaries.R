# Go  back to raw data, and find people who had any non-wear. 
# Quantify the original recording period, and the amount of non-wear.

library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(gridExtra)

# Load Some Functions Manually
source("../postuR/postuR/R/check_nonwear.R")

######################################################
#(1) Loop Through All Raw files and non-wear

raw.files <- dir("../../../MACS_Accel/Accel_Results/Full_Data/", full.names = T)

nonwear.data <- tibble()

for(i in raw.files[992:length(raw.files)]){
#for(i in raw.files){
      id <- sub(".rdata","",sub(".*Full_", "", i))
      load(i)
      
      ### Identify non-wear (3 consecutive hours with fewer than 10 changes)
      nonwear.data <- 
            check.nonwear(data, filter = F, mimimum.wear.bout = 0) %>%
            summarise(id = id[1],
                      start.time = min(time),
                      stop.time = max(time),
                      nonwear.prop = 1 - mean(wear),
                      wear.bouts = length(unique(wear.bout))) %>%
            bind_rows(nonwear.data)
      
      save(nonwear.data,file = paste0("Nonwear_Summaries_",today(),".rdata"))
      
      if(which(i == raw.files)%%20==0) cat(which(i == raw.files), "\n")
}

nrow(nonwear.data) #1252

# Some post processing to eliminate duplicate serial numbers
nonwear.data <- nonwear.data %>% 
   mutate(process.date = sub("_.*","",id) %>% as.numeric,
          device.sn = sub(".*_","",id)) %>% 
   group_by(device.sn) %>% 
   filter(n()==1 | process.date == max(process.date)) %>%
   ungroup %>%
   mutate(record.length = difftime(start.time, stop.time, units = "days"))

nrow(nonwear.data) #1250, removed two duplicates
