
library(readxl)
library(dplyr)

## To generate a full PUF dataset
 #setwd(".../Dissertation/Data Sets/PIAAC/PIAAC PUF")

# load all 35 PIAAC PUF files and merge them into one file
files <- list.files(".", pattern = "*.csv")
puf.raw <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = F)))

# add necessary data info (data collection round, county code and county name)
puf.index <- read_excel("Data Sets/PIAAC/PIAAC PUF List.xlsx", sheet = 1)

# put together
puf <- puf.raw %>% left_join(puf.index[, 5:10], by = c("CNTRYID" = "CountryID"))

 #check the result
puf %>% dplyr::select(CNTRYID, CountryCode, Country, Country, DataCollected, File) %>% distinct()
table(is.na(puf$Country))
(test <- puf %>% group_by(CNTRYID, Country)%>% summarise(N = n()))

# export the merged data file
saveRDS(puf, "Data Analysis/Clean Datasets/PUF_full_DB.rds")
#write.csv(puf, "Data Analysis/Clean Datasets/PUF_full_DB.csv", row.names = F)
