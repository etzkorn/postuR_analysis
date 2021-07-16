
# Take person-level csv files of activity/posture timeseries, 
# generate diurnal recumbent summaries
# 2021-06-27

library(tidyverse)
library(lubridate)

min.files <- dir("../../../MACS_Accel/Accel_Results/OneMinute_Data/", full.names = T)
diurnal.data <- tibble()

############################################################
# Person-Level Summaries
for(i in min.files){
      min.data = read.csv(i)
      id = min.data$id[1]
      
      diurnal.data =  min.data %>% 
        mutate(hour = minute(time)/60 + hour(time))%>%
        group_by(hour)%>%
        summarise(down = mean(down, na.rm=T), .groups = "drop") %>%
        bind_rows(diurnal.data)

      cat(id, "processed.\n")
      
      #Re-Save Summary Data with Every Iteration
      save(diurnal.data, file = paste0("Processed_Data/Diurnal_Recumbent_Summaries",today(),".rdata"))
}

save(diurnal.data, file = paste0("Processed_Data/Diurnal_Recumbent_Summaries",today(),".rdata"))

diurnal.data =  diurnal.data %>% 
  group_by(hour)%>%
  summarise(down = mean(down, na.rm=T), .groups = "drop")

png("Plots/Paper_Plots/Down_Proportion_TimeofDay.png",
    height = 600, width = 800)
print(
ggplot(diurnal.data) +
  geom_line(aes(x = hour, y = down), size = 2) +
  theme_bw(30) + 
  scale_x_continuous("Time of Day (hour)", breaks = c(0,6,12,18,24),
                     limits = c(0,24),expand = c(0.01,.01))+
  ylim(0,1) + ylab("Proportion of People Recumbent")
)
dev.off()

diurnal.data %>% filter(hour == 4)
#0.871

diurnal.data %>% filter(hour == 13)
#0.155
