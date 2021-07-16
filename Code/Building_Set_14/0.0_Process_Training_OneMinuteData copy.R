
# This code performs all neccessary steps for preprocessing the data for 
# the MACS study.

# you need to set the following options 

# this should be a folder with all of the relevant .hea and .zacl files.
# the path should be relative to your current working directory.
# there must be matching ids across hea and zacl files

rm(list=ls())
library(dplyr)
library(lubridate)

source("../Zio_Package/postuR/R/calculate_removal_time.R")
source("../Zio_Package/postuR/R/check_nonwear.R")
source("../Zio_Package/postuR/R/find_top.R")
source("../Zio_Package/postuR/R/mad.R")
source("../Zio_Package/postuR/R/euclid_norm.R")
source("../Zio_Package/postuR/R/process_zacl.R")
source("../Zio_Package/postuR/R/rotate_data.R")
# 1) screen wear time.
# 2) produce epoch-level activity and posture summaries
# 3) aggregate across time


data.dir <- file.path("Data","Data_SMASH_ZIO","Full_Data_2019-04-26_15:15:35")
raw.files <- dir(data.dir, recursive = T,full.names = T)
min.data.dir <- file.path("Data","Data_SMASH_ZIO",paste0("OneMinute_Data_", today()))
dir.create(min.data.dir)

# error on 129, 195, 209, 486, 873, 1132
#for(i in raw.files[c(209, 486, 873, 1132)]){
for(i in raw.files){
  id <- sub(".rdata","",sub(".*Full_", "", i))
  load(i)
  print(which(i==raw.files))
  
  tryCatch({
  min.data <- process.zacl(data, epoch.seconds = 60) %>%
    mutate(id = id)
  write.csv(min.data, 
            file = file.path(min.data.dir, paste0("Min1_",id,".csv")),
            row.names = F)
  },
  error = function(e) {cat(i, ", ", id, "\n")}
  )
  cat(which(i==raw.files))
}

  