
# This code performs all neccessary steps for preprocessing the data for 
# the MACS study.

# you need to set the following options 

# this should be a folder with all of the relevant .hea and .zacl files.
# the path should be relative to your current working directory.
# there must be matching ids across hea and zacl files

rm(list=ls())
library(dplyr)
library(lubridate)

source("../postuR/postuR/R/check_nonwear.R")
source("../postuR/postuR/R/find_top.R")
source("../postuR/postuR/R/euclid_norm.R")
source("../postuR/postuR/R/mad.R")
source("../postuR/postuR/R/process_zacl.R")
source("../postuR/postuR/R/read_header.R")
source("../postuR/postuR/R/read_zacl.R")
source("../postuR/postuR/R/rotate_data.R")
source("../postuR/postuR/R/calculate_removal_time.R")

raw.files <- dir("../../../MACS_Accel/Accel_Results/Full_Data/", full.names = T)
minute.directory <- "../../../MACS_Accel/Accel_Results/OneMinute_Data"
dir.create(minute.directory)

# error on 7897, 1162
#for(i in raw.files[c(209, 486, 873, 1132)]){
for(i in raw.files[1163:length(raw.files)]){
  id <- sub(".rdata","",sub(".*Full_", "", i))
  load(i)
  
  tryCatch({
  min.data <- process.zacl(data, epoch.seconds = 60) %>%
    mutate(id = id)
  write.csv(min.data, 
            file = file.path(minute.directory, paste0("Min1_",min.data$id[1],".csv")),
            row.names = F)
  },
  error = function(e){
    print(e)
    cat(i, ", ", id, "\n")
  }
  )
}


full.ids <- sub(".rdata","",sub(".*Full_", "",dir("../../../MACS_Accel/Accel_Results/Full_Data/")))
ids <- sub(".csv","",sub("Min1_", "", dir(minute.directory)))

# number of ids without enough data for processing
setdiff(full.ids, ids) %>% length()
