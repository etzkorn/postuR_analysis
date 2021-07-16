rm(list = ls())

load(file = "../Towson_Accelerometer_Study/Data/Raw_Labeled.Rdata")
headsource("../Zio_Package/postuR/R/process_zacl.R")
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

df <- ungroup(df) %>%
filter(!is.na(variable)) %>%
mutate(theta.y = acos(-y / (sqrt(x^2 + y^2 + z^z))) * 180 /pi,
       down = theta.y >=65) %>%
filter(!is.na(down))

# Proportion time spent lying down
with(df, mean(variable=="lying"))
#0.1275613

# Accuracy
with(df, mean((down==1 & variable=="lying")|(down==0 & variable!="lying")))
#0.9635961

# Sensitivity
with(df, mean((down==1) & (variable=="lying"))/mean(variable=="lying"))
# 0.9067487

# Specificity
with(df, mean((down==0) & (variable!="lying"))/mean(variable!="lying"))
#0.9760676
