

source("../Zio_Package/postuR/R/process_zacl.R")
source("../Zio_Package/postuR/R/check_nonwear.R")
source("../Zio_Package/postuR/R/calculate_removal_time.R")
source("../Zio_Package/postuR/R/find_top.R")
source("../Zio_Package/postuR/R/euclid_norm.R")
source("../Zio_Package/postuR/R/mad.R")
source("../Zio_Package/postuR/R/rotate_data.R")

library(dplyr)
library(ggplot2) 
library(reshape2)
library(lubridate)
library(tidyr)
library(purrr)

labels<- ungroup(df) %>% select(time, variable, id) %>%
  dplyr::mutate(time = lubridate::floor_date(time, unit = seconds(30))) %>%
  dplyr::group_by(id, time) %>%
  dplyr::summarise(label = variable[1]) %>%
  ungroup

for(k in seq(0.95, 1, by=0.01)){
print(k)
  
load(file = "../Towson_Accelerometer_Study/Data/Raw_Labeled_1.5Decimated.Rdata")

df <- ungroup(df) %>% 
select(-en, -zhat2, -variable) %>%
group_by(id) %>% 
nest()

df$min.data <- lapply(df$data, FUN = process.zacl, k = k, epoch.seconds = 30, 
                      nonwear.window=94*60, nonwear.tol=0, nonwear.filter=F, mimimum.wear.bout=1)

df <- 
df %>% select(-data) %>% unnest(min.data) %>% left_join(labels) %>% filter(!is.na(label))

df <- filter(df, !is.na(label))

# Proportion time spent lying down
with(df, mean(label=="lying")) %>% print()
#0.1275613

# Accuracy
with(df, mean((down==1 & label=="lying")|(down==0 & label!="lying")))%>% print()
# [0.95 - 0.99] 0.9601732
# [1.0] 0.9572872

# Sensitivity
with(df, mean((down==1) & (label=="lying"))/mean(label=="lying"))%>% print()
# [0.95 - 0.99] 0.9705882
# [1.00] 0.9705882

# Specificity
with(df, mean((down==0) & (label!="lying"))/mean(label!="lying"))%>% print()
# [0.95- 0.99] 0.9586503
# [1.00] 0.9553424
}

# False Positive: Sitting
with(df, mean((down==1) & (label=="sit"))/mean(label=="sit"))
#0.2298578

# False Positive: Reading
with(df, mean((down==1) & (label=="read"))/mean(label=="read"))
#0.04662005

# Subjects with 100% accuracy
df %>% group_by(id) %>%
  summarise(acc = mean((down==1 & label=="lying")|(down==0 & label!="lying"))) %>%
  summarise(sum(acc == 1), mean(acc == 1))

