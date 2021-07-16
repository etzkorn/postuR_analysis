library(tidyverse)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(movMF)
library(lubridate)

source("../Zio_Package/postuR/R/process_zacl.R")
source("../Zio_Package/postuR/R/check_nonwear.R")
source("../Zio_Package/postuR/R/calculate_removal_time.R")
source("../Zio_Package/postuR/R/find_top.R")
source("../Zio_Package/postuR/R/euclid_norm.R")
source("../Zio_Package/postuR/R/mad.R")
source("../Zio_Package/postuR/R/rotate_data.R")

##### Load/Decimate Raw Data
load(file = "../Towson_Accelerometer_Study/Data/Raw_Labeled_1.5Decimated.Rdata")


##### Save labels
labels <- ungroup(df) %>% select(time, variable, id) %>%
   dplyr::mutate(time = lubridate::floor_date(time, unit = seconds(30))) %>%
   dplyr::group_by(id, time) %>%
   dplyr::summarise(label = variable[1]) %>%
   ungroup

set.seed(10000)
k = 0.98

##### Pre-Algorithm Simulate Flip
df <- df %>% ungroup %>% filter(!is.na(x)&!is.na(y)&!is.na(z)) %>%
      mutate(epoch = floor_date(time, seconds(30))) %>% 
      nest(-id) %>%
      mutate(top = map(data, find.top),
             cut.point = round(runif(n(),0,1)*unlist(map(data, nrow))),
             to = rmovMF(n(), theta = c(0,0,0)) %>%
                   unclass %>% t %>% as.data.frame %>% as.list,
             rotated = map2(data, to,
                        ~dplyr::select(.x,x,y,z) %>%
                        rotate.data(from = c(0,-1,0), to = .y)),
             rotated = map2(data, rotated, ~cbind(.x,.y)),
             #rotate coordinates if they fall past removal point
             rotated = map2(rotated, cut.point,
                         ~ mutate(.x, 
                                  rotated = 1:n() > .y,
                                  x = ifelse(rotated, rx, x),
                                  y = ifelse(rotated, ry, y),
                                  z = ifelse(rotated, rz, z))),
             # estimate removal times
             removal.time = map(data, calculate.removal.time),
             removal.time.rotated = map(rotated, calculate.removal.time),
             # grab ratios
             ratio = map(removal.time, ~.$r.ratio)%>% unlist,
             ratio.rotated = map(removal.time.rotated,~ .$r.ratio)%>% unlist,
             # decision whether device was rotated
             removed.rotated = ratio.rotated < k,
             removed = ratio < k,
             # add time group back to data
             rotated = map2(rotated, removal.time.rotated,
                         ~mutate(.x, wear.bout = (time > .y$time)& (.y$r.split*0.99 > .y$r.total))),
             data = map2(data, removal.time,
                            ~mutate(.x, wear.bout = (time > .y$time)& (.y$r.split*0.99 > .y$r.total)))) 

df <- df %>% 
      mutate(phi0 = map2(top, to, ~acos(sum(.x*.y)/sqrt(sum(.x^2)))) %>% unlist,
             cut.point.est = map(removal.time, ~.$time) ,
             cut.point.time = map2(data, cut.point, ~.x$time[.y]) ,
             cut.error= map2(cut.point.time, cut.point.est, ~difftime(.x,.y, units = "min")))

#########################################################################
##### Evaluate Algorithm for Rotated Data
#########################################################################

for(k in seq(0.95, 1, by = 0.01)){
      fits <- list()
   
      for(i in 1:length(df$rotated)){
                 fits[[i]] <- 
                 select(df$rotated[[i]], -variable,-en,-zhat2,-epoch, -rx, -ry, -rz, -rotated, -wear.bout) %>%
                 process.zacl(p = 0.95, k = k, theta.star = 45, epoch.seconds = 30,
                              nonwear.window= 94*15, nonwear.tol=10, mimimum.wear.bout=0)
      }
      
      df$fits <- fits
      
      ###########################################################################
      ##### Accuracy for flipped data
      print(k)
      print(
      df %>% 
      select(fits) %>%
      unnest(fits) %>%
      left_join(labels)  %>%
      ungroup %>%
      filter(label!="none") %>%
      summarise(accuracy = mean((down==1 & label == "lying") | (down==0 & label != "lying")),
                specificity = mean((down==0 & label != "lying"))/mean(label != "lying"),
                sensitivity = mean((down==1 & label == "lying"))/mean(label == "lying"))
      )
}
