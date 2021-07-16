
rm(list = ls())

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(gridExtra)

load("Processed_Data/MACS_Summaries2021-06-26.rdata")
load("Processed_Data/MACS_Day_Summaries2021-06-27.rdata")

################################################################
### Print Summary Statistics

# average proportion of day spend sleeping
summary(summary.data$down*24, na.rm = T)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4642  8.5279  9.9652 10.3735 11.8310 20.7000 

day.summary.data %>%
mutate(weekend = wday %in% c("Sat", "Sun")) %>% 
group_by(weekend) %>%
summarise(mean(down*24), median(down*24), quantile(down*24,0.25), quantile(down*24,0.75))

{
png("Plots/Paper_Plots/Hours_per_Day_Weekday.png",
    height = 600, width = 800)
print(
day.summary.data %>%
  filter(wear.days>0.9) %>%
ggplot()+
  geom_boxplot(aes(y = down*24, x = wday))+
  theme_bw(28) +
  theme(legend.position = "none") +
  xlab("Day of Week") + 
  scale_y_continuous("Estimated Time Recumbent (hours/day)",breaks = 4*(0:6))
)
dev.off()
}


