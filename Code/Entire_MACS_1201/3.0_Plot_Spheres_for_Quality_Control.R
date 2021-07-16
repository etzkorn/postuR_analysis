rm(list=ls())

## Packages
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(purrr)
source("../postuR/postuR/R/rotate_data.R")

## SET DATA DIRECTORY (Change for MACS study)
#data.dir <- file.path("Data_MACS2_ZIO")

data.dir <- "../../../../MACS_Accel/Accel_Results/OneMinute_Data/"
processed.files <- dir(data.dir)

plot.dir <- "Plots/Check_Algorithm_Plots/0_Check_Algorithm_Spheres"
dir.create(plot.dir)

load("Upright_Posture_Summaries_2021-06-23.rdata")
top.data <- top.data %>% ungroup %>%
  nest(data = c(axis, coord)) %>%
  mutate(top = map(data, ~unlist(.$coord))) %>%
  select(-data)


#####################################################
# Plot Spheres with MAD and Recumbent Labels

# I actually started saving the rotated data at 121
for(i in processed.files[121:length(processed.files)]){
data = read.csv(file = file.path(data.dir, i))
id = substr(6,22, x = i)
#data= data %>% 
#  group_by(id, wear.bout) %>%
#  nest() %>% 
#  left_join(top.data) %>%
#  mutate(top.null = unlist(map(top, is.null))) %>%
#  filter(!top.null) %>%
#  mutate(rdata = map2(data,top, 
#                      ~rotate.data(cbind(.x$x, .x$y, .x$z), from = .y, to = c(0,0,1)) %>%
#                        as_tibble)) %>%
#  select(-top, -top.null) %>%
#  unnest(c(data, rdata))
  

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



#####################################################
# Make Data frame for recording errors
tibble(id = sub(".png","",dir(plot.dir))) %>% 
write.csv(file = "Plots/Check_Algorithm_Plots/Algorithm_check4.csv")
