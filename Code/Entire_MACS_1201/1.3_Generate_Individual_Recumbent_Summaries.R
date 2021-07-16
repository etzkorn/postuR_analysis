
# Take person-level csv files of activity/posture timeseries, 
# generate person-specific and person/day-specific activity summaries.
# 2021-06-25

library(tidyverse)
library(lubridate)

min.files <- dir("../../../MACS_Accel/Accel_Results/OneMinute_Data/", full.names = T)
day.summary.data <- tibble()
summary.data <- tibble()

############################################################
# Person-Level Summaries
for(i in raw.files){
      min.data = read.csv(i)
      id = min.data$id[1]
      
      # Person-Level Summaries
      summary.data <- global.activity.summaries(min.data, epoch.seconds = 60) %>% 
        bind_rows(summary.data)
      cat(id, "processed.\n")
      
      # Person/Day-Level Summaries
      day.summary.data <- 
            min.data %>% 
            mutate(date = floor_date(time, unit = "day")) %>%
            nest(-date) %>%
            mutate(summaries = map(data, ~global.activity.summaries(., epoch.seconds = 60))) %>%
            select(date, summaries) %>%
            unnest %>%
            bind_rows(day.summary.data)
      
      cat("Processed ", n, "\n")
      n <- n+1
      
      #Re-Save Summary Data with Every Iteration
      save(summary.data, file = paste0("Processed_Data/MACS_Summaries",today(),".rdata"))
      save(day.summary.data, file = paste0("Processed_Data/MACS_Day_Summaries",today(),".rdata"))
}

# Some post processing to eliminate duplicate serial numbers
summary.data <- summary.data %>% 
      mutate(process.date = sub("_.*","",id),
             device.sn = sub(".*_","",id)) %>% 
      group_by(device.sn) %>% 
      filter(n()==1 | days == min(days)) %>%
      ungroup

day.summary.data <- day.summary.data %>% 
      mutate(process.date = sub("_.*","",id),
             device.sn = sub(".*_","",id),
             wday = wday(date,label = T)) %>% 
      group_by(device.sn, date) %>% 
      filter(n()==1 | days == min(days)) %>%
      mutate(nday = 1:n()) %>%
      ungroup

save(summary.data, file = paste0("Processed_Data/MACS_Summaries",today(),".rdata"))
save(day.summary.data, file = paste0("Processed_Data/MACS_Day_Summaries",today(),".rdata"))

write.csv(summary.data, file = paste0("Processed_Data/MACS_Summaries",today(),".csv"), row.names = F)
write.csv(day.summary.data, file = paste0("Processed_Data/MACS_Day_Summaries",today(),".csv"), row.names = F)
