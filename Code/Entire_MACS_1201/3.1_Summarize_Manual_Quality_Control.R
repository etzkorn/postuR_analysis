library(tidyverse)
library(knitr)
library(readr)
library(dplyr)

# This data set contains manually labelled errors from inspecting the spherical plots
df <- read_csv("Plots/Check_Algorithm_Plots/Algorithm_check4.csv")
df <- df[,2:6]

# upright orientation seems incorrectly estimated
df$top[is.na(df$top)] <- 0

# changepoint evident, but not detected
df$split[is.na(df$split)] <- 0

# possible multiple changepoints
df$multisplit[is.na(df$multisplit)] <- 0

# small changepoint not detected, but likely did not cause an error
df$smallsplit[is.na(df$smallsplit)] <- 0

df <- df %>% 
      tidyr::separate(id, c("process.date", "sn", "wear.bout"), sep = "_") %>%
      mutate(wear.bout = as.numeric(wear.bout))

grouped <- 
df %>% group_by(sn) %>%
summarise(top = any(top),
          multisplit = any(multisplit),
          split = any(split&!multisplit),
          smallsplit = any(smallsplit)) 
grouped %>%
select(top:smallsplit) %>% 
lapply(table)

#top
#FALSE  TRUE 
#1189    12 

#multisplit
#FALSE  TRUE 
#1198     3 

#split
#FALSE  TRUE 
#1192    9 

#smallsplit
#FALSE  TRUE 
#1190    11 

table(df$wear.bout)
