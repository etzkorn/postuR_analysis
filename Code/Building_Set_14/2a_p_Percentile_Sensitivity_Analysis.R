# Perform sensitivity analysis for the quantile used to estimate the top of the sphere.

# Vary |H_i| from 0.9 T_i ... 0.99 T_i
# check angular change in upright orientation
# check change in classifications

###########################################################
# Check Angular Change in Upright Orientation
rm(list = ls())
raw.file <- dir("Data/Data_SMASH_ZIO/Full_Data_2019-04-26_15:15:35/", full.names = T)
raw.file <- raw.file[grepl("Full_Data", raw.file) & grepl(".rdata", raw.file)]

## Packages
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(movMF)

chord2theta <- function(chord) 2*asin(chord/2)/pi*180

top.data <- tibble()
max.theta <- c()

for(i in raw.file){
## Grab one raw data file
load(i)

data <- data %>%
mutate(r = sqrt(x^2 + y^2 + z^2),
       x = x/r,
       y = y/r,
       z = z/r,
       r2 = (1-r)^2)

# calculate quantiles
q <- quantile(data$r2, probs = seq(0.9, 0.99, by = 0.01))

# get centers for each quantile
cutoff.means <-
	sapply(q, FUN = function(q0){
		data %>%
		filter(r2>q0) %>%
		select(x,y,z) %>%
		sapply(mean)
	}) %>%
	apply(2, function(m) m / sqrt(sum(m^2))) %>% t()

mean.resultant.length <-
      sapply(q, FUN = function(q0){
            data %>%
                  filter(r2>q0) %>%
                  select(x,y,z) %>%
                  sapply(mean)
      }) %>%
      apply(2, function(m) sqrt(sum(m^2))) %>% unclass

# calculate angular differences

theta.diffs <- cutoff.means %>%
	dist %>%
	as.matrix %>%
	chord2theta

theta90 <- theta.diffs[,1]
theta95 <- theta.diffs[,6]
theta99 <- theta.diffs[,10]

theta.diffs <- c(NA,diag(theta.diffs[1:9,2:10]))

max.theta <- cutoff.means %>%
	dist %>%
	max %>%
	chord2theta %>%
	c(max.theta)

# diffs

# combine data
cutoff.means <- data.frame(p = rownames(cutoff.means),
		   q = q,
		   mean.resultant.length = mean.resultant.length,
		   theta.diffs = theta.diffs,
		   theta90 = theta90,
		   theta95 = theta95,
		   theta99 = theta99) %>%
	cbind(cutoff.means) %>%
	mutate(id = data$id[1])

top.data <- bind_rows(top.data, cutoff.means)
}
###########################################################
# Plot angular differences from 95th percentile mean

png(filename = "Figures/Resubmission_Plots/Sensitivity_Percentile.png",
    width = 800, height = 600)
top.data %>%
#filter(!id %in% c("150831_N509676032","150903_N509636036",
#	      "151031_N520942075","151102_N520912069")) %>%
mutate(p = as.numeric(gsub("%","", p))) %>%
ggplot() +
geom_line(aes(x = p,
	  y = (theta95), group = id)) +
geom_point(aes(x = p,
	y = (theta95), group = id)) +
scale_x_continuous("Percentile",breaks = 90:99,minor_breaks = NULL, expand = c(0.01,0.01)) +
scale_y_continuous("Angular Change (degrees)", expand = c(0.01,0.01))+
theme_bw(20)
dev.off()

# Plot angular differences from 99th percentile mean

png(filename = "Figures/Resubmission_Plots/Sensitivity_Percentile2.png",
    width = 800, height = 600)
top.data %>%
#filter(!id %in% c("150831_N509676032","150903_N509636036",
#	      "151031_N520942075","151102_N520912069")) %>%
mutate(p = as.numeric(gsub("%","", p))) %>%
ggplot() +
geom_path(aes(x = p,
	  y = (theta99), group = id)) +
geom_point(aes(x = p,
	   y = (theta99), group = id)) +
scale_x_continuous("Percentile",breaks = 90:99,minor_breaks = NULL, expand = c(0.01,0.01)) +
scale_y_continuous("Angular Change (degrees)", expand = c(0.01,0.01))+
theme_bw(20)
dev.off()

# Plot angular differences from 90th percentile mean

png(filename = "Figures/Resubmission_Plots/Sensitivity_Percentile3.png",
    width = 800, height = 600)
top.data %>%
	#filter(!id %in% c("150831_N509676032","150903_N509636036",
	#	      "151031_N520942075","151102_N520912069")) %>%
	mutate(p = as.numeric(gsub("%","", p))) %>%
	ggplot() +
	geom_path(aes(x = p,
		  y = (theta90), group = id)) +
	geom_point(aes(x = p,
		   y = (theta90), group = id)) +
	scale_x_continuous("Percentile",breaks = 90:99,minor_breaks = NULL, expand = c(0.01,0.01)) +
	scale_y_continuous("Angular Change (degrees)", expand = c(0.01,0.01))+
	theme_bw(30)
dev.off()

summary(top.data$theta90[top.data$p == "99%"])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.8327  2.6175  4.2269  4.8678  4.6688 20.2406
sort(top.data$theta90[top.data$p == "99%"])
#[1]  0.8327011  0.9852590  1.4776786  2.3321806  2.7126440  2.7790493  2.8282541  3.9638634  4.4899962  4.5314901
#[11]  4.5509194  4.5979630  4.8813924  5.7576668 10.9233013 20.2406366

###########################################################
# Check Changes in mean resultant length

png(filename = "Figures/Resubmission_Plots/Sensitivity_Percentile_ResultantLength.png",
    width = 800, height = 600)
top.data %>%
      mutate(p = as.numeric(gsub("%","", p)),
             change = id %in% c("150831_N509676032",
                                "150911_N509606040",
                                "151102_N520912069")) %>%
      ggplot() +
      geom_path(aes(x = p,
                    y = (mean.resultant.length), group = id, color = change)) +
      geom_point(aes(x = p,
                     y = (mean.resultant.length), group = id, color = change)) +
      scale_x_continuous("Percentile",breaks = 90:99,minor_breaks = NULL, expand = c(0.01,0.01)) +
      scale_y_continuous("Mean Resultant Length", expand = c(0.01,0.01))+
      theme_bw(30) +
      theme(legend.position = "none")
dev.off()

###########################################################
# Check Changes in classification
min.data <- tibble()
data.dir <- dir("Data/Data_SMASH_ZIO/OneMinute_Data_2021-06-21/", full.names = T)
for(i in data.dir){
      min.data <- bind_rows(read.csv(i), min.data)
}
min.data <- 
min.data %>%
      # remove individuals with change points
      filter(!id %in% c("150831_N509676032","150911_N509606040","151102_N520912069"))

top.data <- top.data %>%
	rename(tx = x, ty = y, tz = z)#%>%
	filter(!id %in% c("150831_N509676032","150911_N509606040","151102_N520912069"))

concordance <-
left_join(min.data, top.data , by = "id") %>%
mutate(theta1 = acos(x*tx + y*ty + z*tz),
       down1 = theta1 >= pi/4) %>%
group_by(id, p, cluster.meanshift.14) %>%
mutate(p.down1 = mean(down1),
       down1 = as.numeric((down1)|(p.down1 > 0.5))) %>%
ungroup %>%
group_by(id, time) %>%
mutate(same95 = down1 == down1[p == "95%"],
       different95 = down1 != down1[p == "95%"],
       bothdown95 = down1 & down1[p == "95%"],
       bothup95 = (!down1) & (!down1[p == "95%"]),
       down95 = down1[p == "95%"],
       up95 = !down1[p == "95%"],
       same90 = down1 == down1[p == "90%"],
       different90 = down1 != down1[p == "90%"],
       bothdown90 = down1 & down1[p == "90%"],
       bothup90 = (!down1) & (!down1[p == "90%"]),
       down90 = down1[p == "90%"],
       up90 = !down1[p == "90%"]) %>%
ungroup %>%
group_by(p,id) %>%
summarise(concordance90 = mean(same90, na.rm = T),
          discordance90 = mean(different90, na.rm = T),
          bothup90 = mean(bothup90, na.rm = T)/mean(up90, na.rm = T),
          bothdown90 = mean(bothdown90, na.rm = T)/mean(down90, na.rm = T))

png(filename = "Figures/2_Appendix_Images/Sensitivity_Percentile4.png",
    width = 800, height = 600)
concordance %>%
	mutate(p = as.numeric(gsub("%","", p))) %>%
	ggplot() +
	geom_path(aes(x = p,
		  y = concordance90*100, group = id)) +
	geom_point(aes(x = p,
		   y = concordance90*100)) +
	scale_x_continuous("Percentile",breaks = 90:99,minor_breaks = NULL, expand = c(0.01,0.01)) +
	scale_y_continuous("Concordance (%)", expand = c(0.01,0.01))+
	theme_bw(30)
dev.off()

summary(1- concordance$concordance90[concordance$p == "95%"])
#   Min.   1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.0000000 0.0006759 0.0015556 0.0023833 0.0034217 0.0072969 
summary(1- concordance$concordance90[concordance$p == "99%"])
# 0.000000 0.003733 0.007043 0.007920 0.011307 0.022427 




