# Summarise Upright Posture for Entire Cohort

library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(gridExtra)

# Load Some Functions Manually
source("../postuR/postuR/R/check_nonwear.R")
source("../postuR/postuR/R/find_top.R")
source("../postuR/postuR/R/calculate_removal_time.R")
source("../postuR/postuR/R/euclid_norm.R")
source("../postuR/postuR/R/rotate_data.R")

######################################################
#(1) Loop Through All Raw files and grab estimated upright positions
# Determine if devices were removed/replaced

raw.files <- dir("../../../MACS_Accel/Accel_Results/Full_Data/", full.names = T)
minute.directory <- "../../../MACS_Accel/Accel_Results/OneMinute_Data"

round.unit = lubridate::seconds(60)

top.data <- tibble()

for(i in raw.files[1163:length(raw.files)]){
      id <- sub(".rdata","",sub(".*Full_", "", i))
      load(i)

      #### Code selected from process.zacl()

      ### Identify non-wear (3 consecutive hours with fewer than 10 changes)
      data <- check.nonwear(data, filter = T)
      
      if(nrow(data) == 0){
         next
      }
      
      ### FIND REMOVAL TIME POINT
      # if we deem device is device is removed within any wear bout
      # separate wear bout into two wearbout
      data <-
         data %>%
         dplyr::select(-bout.length) %>%
         dplyr::filter(wear.bout != 0) %>%
         tidyr::nest(data = c(time, x, y, z)) %>%
         dplyr::mutate(
            removal = purrr::map(data, calculate.removal.time),
            data = purrr::map2(data, removal,
                               ~ dplyr::mutate(.x, wear.bout2 = (.y$r.ratio < 0.95)* (time > .y$time)))) %>%
         dplyr::select(-removal) %>%
         tidyr::unnest(data) %>%
         dplyr::mutate(wear.bout = wear.bout + 0.5*wear.bout2) %>%
         dplyr::select(-wear.bout2)%>%
         dplyr::group_by(wear.bout)%>%
         dplyr::filter(n() > 94*60*24) %>%
         dplyr::ungroup()
      
      if(nrow(data) == 0){
         next
      }
      
      top.data <- 
      data %>%
      group_by(wear.bout) %>%
      nest() %>%
      mutate(top = purrr::map(data, find.top)) %>%
      transmute(id = id, wear.bout, top) %>%
      bind_rows(top.data)
      
      save(top.data,file = paste0("Upright_Posture_Summaries_",today(),".rdata"))
}
