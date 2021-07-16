
# Explore how classifications change based on angle used to initially
# label a point recumbent or upright.

library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(purrr)
library(tidyr)
rm(list = ls())

data.dir <- dir("Data/Data_SMASH_ZIO/OneMinute_Data_2021-06-21",full.names = T)
data <- tibble()
for(i in data.dir){
  data <- bind_rows(read.csv(i), data)
}
rm(data.dir)

head(data)
data <-
      data %>%
      group_by(id, cluster.meanshift.14)

theta.star <- seq(30,60, length = 7)

for(theta.star.i in theta.star){
      varname = paste0("down", theta.star.i) 
      data <- data %>%
      mutate(!!varname := (mean(theta > theta.star.i) > 0.5))
}

data <- data %>% ungroup %>% group_by(id)

for(theta.star.i in theta.star){
      varname = paste0("down0_", theta.star.i) 
      data <- data %>%
            mutate(!!varname := theta > theta.star.i)
}


df <-data %>% ungroup %>%
      select(id, down30:down60) %>%
      gather(key = "theta", value = "down",-id) %>%
      group_by(id, theta) %>%
      summarise(down = mean(down)) %>%
      mutate(theta = as.numeric(gsub("down","",theta))) 

df2 <-
df %>% group_by(theta) %>% 
      summarise(m = median(down*100),
                q1 = quantile(down*100,0.25),
                q3 = quantile(down*100,0.75))

png(filename = "Figures/2_Appendix_Images/Sensitivity_Angular_Threshold.png",
    width = 800, height = 600)
ggplot(df) +
geom_ribbon(data = df2, aes(ymin = q1, ymax = q3, x = theta),
            alpha = 0.2, fill = "blue")+
geom_line(aes(x = theta,y = (down*100),group = id)) +
geom_point(aes(x = theta,y = (down*100))) +
scale_x_continuous("Angular Threshold (degrees)",
                   minor_breaks = NULL) +
scale_y_continuous("Time Spent Recumbent (%)",
                   minor_breaks = NULL,limits = c(10,70))+
geom_line(data = df2, aes(y = m, x = theta),
          color = "blue", size = 1)+
#geom_point(data = df2, aes(y = m, x = theta),
#                color = "blue", size = 2)+
theme_bw(20)

dev.off()


df3 <-data %>% ungroup %>%
      select(id, down0_30:down0_60) %>%
      gather(key = "theta", value = "down",-id) %>%
      group_by(id, theta) %>%
      summarise(down = mean(down)) %>%
      mutate(theta = as.numeric(gsub("down0_","",theta))) 

df4 <-
      df3 %>% group_by(theta) %>% 
      summarise(m = median(down*100),
                q1 = quantile(down*100,0.25),
                q3 = quantile(down*100,0.75))

png(filename = "Figures/2_Appendix_Images/Sensitivity_Angular_Threshold2.png",
    width = 800, height = 600)
ggplot(df3) +
      geom_ribbon(data = df4, aes(ymin = q1, ymax = q3, x = theta),
                  alpha = 0.2, fill = "blue")+
      geom_line(aes(x = theta,y = (down*100),group = id)) +
      geom_point(aes(x = theta,y = (down*100))) +
      scale_x_continuous("Angular Threshold (degrees)",
                         minor_breaks = NULL) +
      scale_y_continuous("Time Spent Recumbent (%)",
                         minor_breaks = NULL,limits = c(10,70))+
      geom_line(data = df4, aes(y = m, x = theta),
                color = "blue", size = 1)+
      #geom_point(data = df2, aes(y = m, x = theta),
      #                color = "blue", size = 2)+
      theme_bw(20)

dev.off()
