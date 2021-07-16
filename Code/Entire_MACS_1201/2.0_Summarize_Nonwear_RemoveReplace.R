
rm(list = ls())
load("Nonwear_Summaries_2021-06-25.rdata")
load("Upright_Posture_Summaries_2021-06-23.rdata")


# indicate inclusion if more than 24 hours of wear
nonwear.data$included <- -as.numeric(nonwear.data$record.length)*(1-nonwear.data$nonwear.prop)>=1
sum(nonwear.data$included)
#1208

# filter out people with not enough wear anyway
top.data$included <- top.data$id %in% nonwear.data$id[nonwear.data$included]

length(unique(top.data$id[top.data$included])) #1201
length(unique(top.data$id)) #1202

top.data <- top.data[top.data$included,]

######################################################
### Nonwear Summaries

# (1) Proportion of People with any non-wear time
mean(nonwear.data$nonwear.prop >0) #0.05910543
sum(nonwear.data$nonwear.prop >0) #74

# Save list of any person who has any nonwear for manual checking
nonwear.ids <- nonwear.data$id[nonwear.data$nonwear.prop > 0]
save(nonwear.ids, file = paste0("Processed_Data/NonWear_Filenames",today(),".rdata"))

# (2) Total recording length/person
summary(-as.numeric(nonwear.data$record.length))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.1021  6.0251 13.0207 10.1063 13.9500 14.0004

# (2b) Total recording length
sum(-as.numeric(nonwear.data$record.length))
#12632.88

#(2c) Number of people with fewer than 24 hours of wear before screening
sum(-as.numeric(nonwear.data$record.length)<1)
# 0.032
mean(-as.numeric(nonwear.data$record.length)<1)
# 40

#(2d) Number of people with fewer than 24 hours of wear after screening
sum(-as.numeric(nonwear.data$record.length)*(1-nonwear.data$nonwear.prop)<1)
# 42

mean(-as.numeric(nonwear.data$record.length*(1-nonwear.data$nonwear.prop))<1)
# 0.0336

# (3) Total wear time per/person among people with more than 24 hours of valid wear
summary((-as.numeric(nonwear.data$record.length)*(1-nonwear.data$nonwear.prop))[nonwear.data$included])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.007   6.673  13.123  10.367  13.950  14.000 

# (b) Total wear time among people with more than 24 hours of valid wear
sum((-as.numeric(nonwear.data$record.length)*(1-nonwear.data$nonwear.prop))[nonwear.data$included])
# 12523.48

# (4) Nonwear/Nonrecord length
summary((-as.numeric(nonwear.data$record.length)*nonwear.data$nonwear.prop)[nonwear.data$nonwear.prop > 0])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1275  0.2061  0.4637  1.1784  1.1859 11.9508

#(4b) Proportion of recording time representing non-wear
sum((-as.numeric(nonwear.data$record.length)*nonwear.data$nonwear.prop))/
  sum(-as.numeric(nonwear.data$record.length))
# 0.006716228

#(4b) Total of recording time representing non-wear
sum((-as.numeric(nonwear.data$record.length)*nonwear.data$nonwear.prop))
#  84.84

# (5) Proportion of Record Time representing Nonwear/Nonrecord 
summary(nonwear.data$nonwear.prop[nonwear.data$nonwear.prop > 0])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01021 0.03196 0.08366 0.17302 0.21943 0.93100  


# (6) Wear time among nonwearers 
summary((-as.numeric(nonwear.data$record.length)*(1-nonwear.data$nonwear.prop))[nonwear.data$nonwear.prop > 0])
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2322  2.8334  6.0695  6.8356 11.4270 13.8574 


# (7) Frequency of wear bout counts
summary(nonwear.data$wear.bouts[nonwear.data$nonwear.prop > 0])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.000   3.000   3.000   3.833   4.000  12.000 

# (8) Total number of nonwear bouts
sum((nonwear.data$wear.bouts-1))
# 204

######################################################
### Removal/Replacement Summaries

# (1) Number of remaining people with any non-wear
top.data %>% ungroup %>% group_by(id) %>% 
  filter(length(unique(floor(wear.bout)))>1) %>% 
  ungroup%>%
  summarise(length(unique(id)))
# 21

# (2) Number of people who remove/replaced device
top.data %>% ungroup %>%
filter(wear.bout %% 1 == 0.5) %>%
summarise(length(unique(id)))
# 43

# (3) Number of remaining people with either non-wear or removal-replacement
top.data %>% ungroup %>% group_by(id) %>% 
  filter((length(unique(floor(wear.bout)))>1) | any(wear.bout %% 1 == 0.5)) %>%
  ungroup %>%
  summarise(length(unique(id)))
# 58

# number of removal/replacements
top.data %>% ungroup %>%
  filter(wear.bout %% 1 == 0.5) %>%
  nrow
# 44

# central orientation of top during first wear bout
m <- 
top.data$top[top.data$wear.bout == 1] %>% 
do.call(what = "rbind") %>%
apply(1, function(x) x/sqrt(sum(x^2))) %>%
rowMeans

sqrt(sum(m^2))
# mean resultant length = 0.946

m/sqrt(sum(m^2))
# central orientation
# x         y         z 
# 0.6197832 0.6829396 0.3866035

# angular difference between population center
top.data$top[top.data$wear.bout == 1] %>% 
  do.call(what = "rbind") %>%
  apply(1, function(x) 180/pi*acos(sum(x*m)/sqrt(sum(x^2))/sqrt(sum(m^2)))) %>%
  summary

