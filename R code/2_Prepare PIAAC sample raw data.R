
# Prepare sample dataset
#load R packages for functions will be used
packages.need <- c("tidyverse", "readxl")
if (length(setdiff(packages.need, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages.need, rownames(installed.packages())))}
library(tidyverse); library(readxl)


# Load dataset
puf <- readRDS("Data Analysis/Clean Datasets/PUF_full_DB.rds")
piaac.selected <- read_excel("Data Analysis/Clean Datasets/Selected dissertation variables.xlsx", sheet = 1)
edu.lux <- read_excel("Data Analysis/Clean Datasets/Selected dissertation variables.xlsx", sheet = 3)

#********************************************************************
# Data preparing
#********************************************************************
#look-up tables for highest qualification of formal education 
 #note: B_Q01a and B_Q01a3 are only used to verify the values of EDCAT6
BQ01a.lux = edu.lux %>% filter(Variable == "B_Q01a") %>% dplyr::select(Value, B_Q01a_lable = Label, B_Q01a_mapped = Mapped)
BQ01a3.lux = edu.lux %>% filter(Variable == "B_Q01a3") %>% dplyr::select(Value, B_Q01a3_lable = Label, B_Q01a3_mapped = Mapped)
EDCAT.lux = edu.lux %>% filter(Variable == "EDCAT6") %>% dplyr::select(Value, EDCAT6_lable = Label, EDCAT6_mapped = Mapped)

puf2 <- puf %>%
  #re-code STEM
  mutate(STEM = case_when(B_Q01b %in% c("5","6","7") ~ "STEM",
                          B_Q01b %in% c("1","2","3","4","8","9") ~ "Non-STEM",
                          TRUE ~ NA_character_))%>%
  
  #impute employment status
  mutate(Employed01 = ifelse((C_Q01a == "1" & !is.na(C_Q01a))|C_Q01b == "1" , 1, 0),
         Employed02 = ifelse(C_Q02a == "2" & C_Q02b == "1" & C_Q02c == "1" , 1, 0),
         Employed_flag = ifelse(Employed01 + Employed02 >= 1, 1, 0)) %>%
  mutate(OutOfLaborforce_flag = ifelse(C_Q03_02 == "1" |C_Q03_03 == "1" |C_Q03_05 == "1"|C_Q03_08 == "1"|C_Q03_09 == "1", 1, 0))%>%
  mutate(Employment = ifelse(Employed_flag == 1, "Employed", ifelse(OutOfLaborforce_flag == 1, "Out of labor force", "Unemployed")))%>%
  
  #impute highest education attainment level
  left_join(BQ01a.lux, by = c("B_Q01a" = "Value"))%>%
  left_join(BQ01a3.lux, by = c("B_Q01a3" = "Value")) %>%
  left_join(EDCAT.lux, by = c("EDCAT6" = "Value"))%>%
  mutate(Education_BQ1a = ifelse(B_Q01a_mapped == "Foreign qualification", B_Q01a3_mapped, B_Q01a_mapped))%>%
  
  #re-code occupation
  mutate(Occupation = case_when(ISCOSKIL4 == "1" ~ "Skilled",
                                ISCOSKIL4 == "2" ~ "White-collar",
                                ISCOSKIL4 == "3" ~ "Blue-collar",
                                ISCOSKIL4 == "4" ~ "Elementary",
                                TRUE ~ NA_character_))%>%
  #re-code gender
  mutate(Gender = case_when(GENDER_R == "1" ~ "M", GENDER_R == "2" ~ "F", TRUE ~ NA_character_))


#---------------------------------
# checking results
table(puf2$B_Q01b, puf2$STEM, useNA = "ifany")

table(puf2$Employed01, useNA = "ifany")
puf2[1:50, c("C_Q01a", "C_Q01b", "Employed01")]

table(puf2$Employed02, useNA = "ifany")
puf2[10:60, c("C_Q02a", "C_Q02b", "C_Q02c", "Employed02")]

table(puf2$OutOfLaborforce_flag, useNA = "ifany")
puf2[1:50, c("C_Q03_02", "C_Q03_03", "C_Q03_05", "C_Q03_08", "C_Q03_09", "OutOfLaborforce_flag")]

table(employed = puf2$Employed_flag, out = puf2$OutOfLaborforce_flag, useNA = "ifany")
table(puf2$Employment, useNA = "ifany")
unique(puf2[, c("Employed_flag","OutOfLaborforce_flag", "Employment")])

table(puf2$B_Q01a_lable, puf2$B_Q01a, useNA = "ifany")
table(puf2$B_Q01a3_lable, puf2$B_Q01a3, useNA = "ifany")
table(puf2$B_Q01a_lable, puf2$B_Q01a_mapped, useNA = "ifany")
table(puf2$B_Q01a3_lable, puf2$B_Q01a3_mapped, useNA = "ifany")
table(puf2$B_Q01a_lable == puf2$B_Q01a3_lable)
table(puf2$Education == puf2$B_Q01a_mapped)

table(puf2$EDCAT6, puf2$Education, useNA = "ifany")
table(puf2$ISCOSKIL4, puf2$Occupation, useNA = "ifany")
table(puf2$GENDER_R, puf2$Gender, useNA = "ifany")


#----------------------------------------
vars.imputed = c(names(puf2[, c(1,1330:1348)]), "Age_group")
# [1] "CNTRYID"              "CountryCode"          "DataCollectedRound"   "DataCollected"        "Country"              "STEM"                 "Employed01"           "Employed02"           "Employed_flag"       
# [10] "OutOfLaborforce_flag" "Employment"           "B_Q01a_lable"         "B_Q01a_mapped"        "B_Q01a3_lable"        "B_Q01a3_mapped"       "EDCAT6_lable"         "EDCAT6_mapped"        "Education_BQ1a"      
# [19] "Occupation"           "Gender"               "Age_group" 

sampled.raw <- puf2 %>% 
  filter (AGEG5LFS %in% c(3,4))%>%
  mutate(Age_group = case_when(AGEG5LFS == 3 ~ "25to29", AGEG5LFS == 4 ~ "30to34"))%>%
  filter(Employment != "Out of labor force")%>%
  dplyr::select(all_of(vars.imputed), piaac.selected$Name)


# export the data set
write.csv(sampled.raw, "Data Analysis/Clean Datasets/Sample_piaac_raw.csv", row.names = F)
