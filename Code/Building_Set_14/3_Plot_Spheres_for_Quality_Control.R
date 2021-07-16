rm(list=ls())

## Packages
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(purrr)
source("../Zio_Package/postuR/R/rotate_data.R")

## SET DATA DIRECTORY (Change for MACS study)
#data.dir <- file.path("Data_MACS2_ZIO")

data.dir <- "Data/Data_SMASH_ZIO/OneMinute_Data_2021-06-21"
processed.files <- dir(data.dir, full.names = T)

plot.dir <- "Figures/6_Check_Algorithm_Plots/"
dir.create(plot.dir)

#####################################################
# Make Data frame for recording errors
tibble(id = sub(".png","",dir(plot.dir))) %>% 
write.csv(file = "Plots/Check_Algorithm_Plots/Algorithm_check4.csv")

#####################################################
# Plot Spheres with MAD and Recumbent Labels

for(i in processed.files){
data = read.csv(file = i)
id = data$id[1]

for(j in unique(data$wear.bout)){

png(file=file.path(plot.dir, paste0(id, "_", j,".png")),
    height = 600, width=1200)
      
  sphere.plot <- 
    data %>% filter(wear.bout==j) %>% arrange( mad) %>%
    ggplot() + 
    theme_bw()+
    theme(legend.position = "none",
          strip.background = element_blank(), 
          strip.text = element_blank())+
    scale_color_continuous(low = "blue", high="red") +
    xlim(-1,1) +
    ylim(-1,1)
  
  p1 <- sphere.plot + 
    ggtitle("X, Y Axis Medians (After Rotation)") +
    ylab("Median Y (g)") +
    xlab("Median X (g)") +
    geom_point(aes(x = rx, y = ry, 
                   color = mad), alpha = 0.15)
  
  p2 <- sphere.plot + 
    ggtitle("X, Y Axis Medians (After Rotation)") +
    ylab("Median Y (g)") +
    xlab("Median X (g)") +
    geom_point(aes(x = rx, y = ry, 
                   color = down), alpha = 0.15)
  
  print(grid.arrange(p1, p2, ncol=2))

dev.off()
}
}
