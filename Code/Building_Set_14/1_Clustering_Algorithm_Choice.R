
# Types of Clustering Explored
# vonMises Mixtures
# dbscan
# hierarchical clustering (centroid, median, single, ward)
# Meanshift

library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(purrr)
library(tidyr)

#clustering packages
library(fastcluster)
library(meanShiftR)
library(dbscan)

data.dir <- file.path("Data/Data_SMASH_ZIO/Raw_2018-10-18/Minute_Data_2019-04-26_15:15:35/")
load(dir(data.dir,full.names = T)[1])
rm(data.dir)

data <- nest(data, -id, -time.group)

#####################################################
# Hierarchical (Single)
data$single <-
map(data$data,
    ~cbind(.$x,.$y,.$z) %>%
     hclust.vector(method="single", metric='euclidean')) %>%
lapply(FUN = function(clusters){
             lapply(5:15, FUN = function(k){
                   cutree(clusters, k = k)
             }) %>% 
             as.data.frame(col.names= paste0("single", 5:15)) 
       }
)

# Plot HC Single
for(i in 1:nrow(data)){
      png(filename = paste0("Figures/Resubmission_Plots/SingleHC",i,".png"),
          height = 1000, width = 1500)
      print(
            data[i,]%>%
                  unnest(c("data", "single")) %>%
                  select(id, time.group, rx, ry, rz, single5:single15) %>%
                  pivot_longer(cols = single5:single15) %>%
                  mutate(name = as.numeric(sub("single","",name))) %>%
                  arrange(value) %>%
                  ggplot()+
                  geom_point(aes(x = rx, y = ry, color = factor(value)))+
                  facet_wrap("name") + 
                  theme_void(20) + 
                  scale_colour_brewer(palette = "Set1")+
                  theme(legend.position = "none")
      )
      dev.off()
}

#####################################################
# Hierarchical (Ward)
data$ward <-
      map(data$data,
          ~cbind(.$x,.$y,.$z) %>%
                hclust.vector(method="ward", metric='euclidean')) %>%
      lapply(FUN = function(clusters){
            lapply(5:15, FUN = function(k){
                  cutree(clusters, k = k)
            }) %>% 
                  as.data.frame(col.names= paste0("ward", 5:15)) 
      }
      )

# Plot HC Ward
for(i in 1:nrow(data)){
      png(filename = paste0("Figures/Resubmission_Plots/WardHC",i,".png"),
          height = 1000, width = 1500)
      print(
            data[i,]%>%
                  unnest(c("data", "ward")) %>%
                  select(id, time.group, rx, ry, rz, ward5:ward15) %>%
                  pivot_longer(cols = ward5:ward15) %>%
                  mutate(name = as.numeric(sub("ward","",name))) %>%
                  arrange(value) %>%
                  ggplot()+
                  geom_point(aes(x = rx, y = ry, color = factor(value)))+
                  facet_wrap("name") + 
                  theme_void(20) + 
                  scale_colour_brewer(palette = "Set1")+
                  theme(legend.position = "none")
      )
      dev.off()
}

#####################################################
# Hierarchical (Median)

data$median <-
      map(data$data,
          ~cbind(.$x,.$y,.$z) %>%
                hclust.vector(method="median", metric='euclidean')) %>%
      lapply(FUN = function(clusters){
            lapply(5:15, FUN = function(k){
                  cutree(clusters, k = k)
            }) %>% 
                  as.data.frame(col.names= paste0("median", 5:15)) 
      }
      )

# Plot HC Ward
for(i in 1:nrow(data)){
      png(filename = paste0("Figures/Resubmission_Plots/MedianHC",i,".png"),
          height = 1000, width = 1500)
      print(
            data[i,]%>%
                  unnest(c("data", "median")) %>%
                  select(id, time.group, rx, ry, rz, median5:median15) %>%
                  pivot_longer(cols = median5:median15) %>%
                  mutate(name = as.numeric(sub("median","",name))) %>%
                  arrange(value) %>%
                  ggplot()+
                  geom_point(aes(x = rx, y = ry, color = factor(value)))+
                  facet_wrap("name") + 
                  theme_void(20) + 
                  scale_colour_brewer(palette = "Set1")+
                  theme(legend.position = "none")
      )
      dev.off()
}


#####################################################
# Hierarchical (Centroid)

data$centroid <-
      map(data$data,
          ~cbind(.$x,.$y,.$z) %>%
                hclust.vector(method="centroid", metric='euclidean')) %>%
      lapply(FUN = function(clusters){
            lapply(5:15, FUN = function(k){
                  cutree(clusters, k = k)
            }) %>% 
                  as.data.frame(col.names= paste0("centroid", 5:15)) 
      }
      )

# Plot HC Ward
for(i in 1:nrow(data)){
      png(filename = paste0("Figures/Resubmission_Plots/CentroidHC",i,".png"),
          height = 1000, width = 1500)
      print(
            data[i,]%>%
                  unnest(c("data", "centroid")) %>%
                  select(id, time.group, rx, ry, rz, centroid5:centroid15) %>%
                  pivot_longer(cols = centroid5:centroid15) %>%
                  mutate(name = as.numeric(sub("centroid","",name))) %>%
                  arrange(value) %>%
                  ggplot()+
                  geom_point(aes(x = rx, y = ry, color = factor(value)))+
                  facet_wrap("name") + 
                  theme_void(20) + 
                  scale_colour_brewer(palette = "Set1")+
                  theme(legend.position = "none")
      )
      dev.off()
}

#####################################################
# MeanShift

data$ms <-
      lapply(data$data, FUN = function(data){
            lapply(seq(0.06, 0.28, by=0.02), FUN = function(w){
                  (data[,c("x","y","z")] %>% 
                  as.matrix() %>%
                  meanShift(bandwidth = rep(w, 3), iter = 100))$assignment %>%
                  as.vector
            }) %>% 
            as.data.frame(col.names= paste0("ms", seq(0.06, 0.28, by=0.02))) 
      }
      )

# Plot HC Ward
for(i in 1:nrow(data)){
      png(filename = paste0("Figures/Resubmission_Plots/MeanShift",i,".png"),
          height = 1000, width = 1500)
      print(
            data[i,]%>%
                  unnest(c("data", "ms")) %>%
                  select(id, time.group, rx, ry, rz, ms0.06:ms0.28) %>%
                  pivot_longer(cols = ms0.06:ms0.28) %>%
                  mutate(name = as.numeric(sub("ms","",name))) %>%
                  arrange(value) %>%
                  ggplot()+
                  geom_point(aes(x = rx, y = ry, color = factor(value)))+
                  facet_wrap("name") + 
                  theme_void(20) + 
                  scale_colour_brewer(palette = "Set1")+
                  theme(legend.position = "none")
      )
      dev.off()
}

lapply(data$ms, 
       FUN = function(df){
             apply(df, 2, FUN = function(v){
                   length(unique(v))
             })
       }) %>%
do.call(what = "rbind")

#####################################################
# Minimum number of clusters to separate upright from recumbent

# Single
n.single <-c(16,16,16,16,16,10,8,16,5,16,16,16,16,16,5,16,16,16,7)
mean(n.single > 15)

# Ward
n.ward <- c(6,5,5,5,5,5,5,5,5,5,5,7,5,5,5,5,5,5,5)
max.ward<-c(5,6,6,7,6,7,6,7,7,9,5,5,6,6,6,9,7,5,5)

# find optimal cluster count
sapply(5:15, function(x) mean(x < max.ward & x >= n.ward)) %>% cbind(5:15)

# Median
n.median <- c(16, 8, 16, 13, 14, 6, 8, 7, 6, 5, 9, 10, 8, 5, 5, 11, 13, 12, 5)
max.median <- c(6, 5, 5,5,5,7,5,15,12,15,5,5,5,10,8,15,6,10,5)

# find optimal cluster count
sapply(5:15, function(x) mean(x < max.median & x >= n.median)) %>% cbind(5:15)

# Centroid
n.centroid <- c(11, 5, 5, 6, 5, 5, 5, 7, 5, 5, 7, 5, 5, 5, 5, 10, 5, 7, 5)
max.centroid <- c(8, 14, 14,17, 8,10,17,17,11,17,11,13,9,13,8,17,13,11,17)

# find optimal cluster count
sapply(5:15, function(x) mean(x < max.centroid & x >= n.centroid)) %>% cbind(5:15)

# Maximum Kernel Width for Mean shift Clustering
ms.width <- c(0.1, 0.22, 0.12, 0.28, 0.18, 0.2, 0.26, 0.08, 0.26, 0.28, 0.14, 0.28, 0.28, 0.2, 0.28, 0.16, 0.28, 0.24, 0.28)
min.width <- c(0.16, 0.14, 0.06, 0.1, 0.12, 0.04, 0.06,0.04,0.04,0.04,0.14,0.12,0.04,0.06,0.08,0.08,0.08,0.1,0.08)
length(ms.width)

table(ms.width > min.width)

# find optimal bandwidth
sapply((4:30)/100, function(x) mean(x < ms.width & x >= min.width)) %>% cbind((4:30)/100)

# Conclusion: Ward clustering seems to separate the recumbent clusters 
# from the upright clusters using the fewest numbers of clusters overall.
