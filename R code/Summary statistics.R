
library(tidyverse)# R package for data manipulation

# load data
 #study 1 data is Sample_piaac_clean.csv, study 2 is Study2_piaac_for sample summary.csv
dt <- read.csv("Data Analysis/Clean Datasets/Sample_piaac_clean.csv", stringsAsFactors = T, na.strings = c("NA", ""))

# prepare data for modeling: re-code variable, re-level reference level, etc.
dt <- dt %>% mutate(Employed = dplyr::recode(Employment, "Employed" = 1, "Unemployed" = 0))
dt$Education <- relevel(dt$Education, ref = "Below high school")

mean(dt$PVLIT7, na.rm = T)
sd(dt$PVLIT1, na.rm = T)


## Calculate pooled means and sd of three cognitive competencies
# literacy score
lit = dt%>%select(PVLIT1:PVLIT10)%>%
  pivot_longer(cols = PVLIT1:PVLIT10)%>%
  group_by(name)%>%
  summarise(mean= mean(value, na.rm = T), 
            sd= sd(value, na.rm = T))
  
mean(lit$mean); mean(lit$sd)

# numeracy score
num = dt%>%select(PVNUM1:PVNUM10)%>%
  pivot_longer(cols = PVNUM1:PVNUM10)%>%
  group_by(name)%>%
  summarise(mean= mean(value, na.rm = T), 
            sd= sd(value, na.rm = T))

mean(num$mean); mean(num$sd)

# literacy + numeracy score
litnum = dt%>%select(PVLITNUM1:PVLITNUM10)%>%
  pivot_longer(cols = PVLITNUM1:PVLITNUM10)%>%
  group_by(name)%>%
  summarise(mean= mean(value, na.rm = T), 
            sd= sd(value, na.rm = T))

mean(litnum$mean); mean(litnum$sd)

# PSTRE score
pstre = dt%>%select(PVPSL1:PVPSL10)%>%
  pivot_longer(cols = PVPSL1:PVPSL10)%>%
  group_by(name)%>%
  summarise(mean= mean(value, na.rm = T), 
            sd= sd(value, na.rm = T))

mean(pstre$mean); mean(pstre$sd)

