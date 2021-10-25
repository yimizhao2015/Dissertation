
## Prepare clean sample data

library(tidyverse)
library(readxl)

# load data sets
raw <- read.csv("Data Analysis/Clean Datasets/Sample_piaac_raw.csv", stringsAsFactors = F)
country <- read_excel("Data Analysis/Clean Datasets/piaac country_clean.xlsx", sheet = 1)


#---------------------------------------------------
## prepare reshaped country portfolio data
country_wide_raw <- country %>% 
  dplyr::select(CountryCode, Indicator, Value)%>%
  pivot_wider(names_from = Indicator, values_from = Value)

names(country_wide_raw); unique(country[, c("IndicatorName","Indicator")])
names(country_wide_raw) <- c("CountryCode", "CollegeCompletion_raw","HSCompletion_raw", "GDPPCAP_raw",  "Internet_raw", "Mobile_raw", "Poverty_raw")  

 #Standard scaling the country-level variables
country_wide <- country_wide_raw %>%
  mutate(CollegeCompletion = scale(CollegeCompletion_raw, center = T, scale = T),
         HSCompletion = scale(HSCompletion_raw, center = T, scale = T),
         GDPPCAP = scale(GDPPCAP_raw, center = T, scale = T),
         Internet = scale(Internet_raw, center = T, scale = T),
         Mobile = scale(Mobile_raw, center = T, scale = T),
         Poverty = scale(Poverty_raw, center = T, scale = T))

#---------------------------------
## Clean/impute piacc data, and merge country data
#subset needed variables 
sampled <- raw %>% 
            dplyr::select(Weight = SPFWT0, CNTRYID:STEM, Employment, Education = EDCAT6_mapped, Occupation, Gender, Age = Age_group, 
                          Earnings_raw = EARNHRBONUSPPP, Training = FNFAET12JR, Experience_raw = C_Q09_C, Schooling = YRSQUAL, PVLIT1, PVLIT2:PVLIT9, PVLIT10,
                          PVNUM1, PVNUM2:PVNUM9, PVNUM10, PVPSL1, PVPSL2:PVPSL9, PVPSL10) %>%
            mutate_at(c("Earnings_raw", "Training", "Experience_raw", "Schooling"), as.numeric)


#check data missingness
str(sampled)
table(is.na(sampled$CNTRYID));table(is.na(sampled$CountryCode)); table(is.na(sampled$DataCollectedRound)); 
table(is.na(sampled$DataCollected)); table(is.na(sampled$Country))

summary(sampled$PVLIT1)#literacy scores PVLIT1 to PVLIT1o, with missing values
summary(sampled$PVNUM1)#numeracy scores PVNUM1 to PVNUM1o, with missing values
summary(sampled$PVPSL1)#PSTRE scores PVPSL1 to PVPSL10, with missing values, three countries didn't test PSTRE (France,Spain,Italy)

table(sampled$STEM, useNA = "ifany")#missing values
table(sampled$Employment, useNA = "ifany")
table(sampled$Education, useNA = "ifany")#missing values
table(sampled$Occupation, useNA = "ifany")#missing values
table(sampled$Gender, useNA = "ifany")
table(sampled$Age, useNA = "ifany")
summary(sampled$Earnings_raw)#missing values, need imputation, unemployed no earnings, outliners (<0.1, >500)
table(sampled$Training , useNA = "ifany") #has missing values
summary(sampled$Experience_raw)#need imputation, cannot be larger than age
summary(sampled$Schooling)#missing values

 # 9 countries have no earnings info: Austria,Canada,Germany,Hungary,Peru,Singapore,Sweden,Turkey,USA
earnings.missing <- sampled %>% group_by(Country) %>% 
  summarize(
    N = n(),
    nas = sum(is.na(Earnings_raw)),
    avg = mean(Earnings_raw, na.rm = T))%>%
  mutate(na_percent = nas/N)
earnings.missing %>% filter(is.na(avg))

#Clean and impute sample data
#remove rows with all missing values on necessary variables, 450 empty rows
 #names(sampled)[17:46] are PV values of three competencies
cols <- c("STEM","Education","Occupation","Earnings_raw","Training","Experience_raw","Schooling", names(sampled)[17:46])

sampled2 <- sampled[!apply(is.na(sampled[cols]), 1, all), ]%>% 
  #impute earnings: unemployed no earnings,outliners (<0.1, >500)
  mutate(Earnings = ifelse(Earnings_raw < 0.1|Earnings_raw >500|Employment == "Unemployed", NA, Earnings_raw))%>%
  #impute work experience: 25to29 <= 13, 30to34 <= 18 (started working since 16)
  mutate(Experience_flag = case_when(
                     Experience_raw == 0 & Employment == "Employed" ~ "other",
                     Age == "25to29" & Experience_raw <= 13 ~ "Yes",
                     Age == "30to34" & Experience_raw <= 18 ~ "Yes",
                     TRUE ~ "other"))%>%
  mutate(Experience = ifelse(Experience_flag == "Yes", Experience_raw, NA))%>%
  #add new variable: average of Literacy and Numeracy score
  mutate(PVLITNUM1 = (PVLIT1 + PVNUM1)/2,
         PVLITNUM2 = (PVLIT2 + PVNUM2)/2,
         PVLITNUM3 = (PVLIT3 + PVNUM3)/2,
         PVLITNUM4 = (PVLIT4 + PVNUM4)/2,
         PVLITNUM5 = (PVLIT5 + PVNUM5)/2,
         PVLITNUM6 = (PVLIT6 + PVNUM6)/2,
         PVLITNUM7 = (PVLIT7 + PVNUM7)/2,
         PVLITNUM8 = (PVLIT8 + PVNUM8)/2,
         PVLITNUM9 = (PVLIT9 + PVNUM9)/2,
         PVLITNUM10 = (PVLIT10 + PVNUM10)/2)%>%
  # center and standardize competency scores
  mutate(across(starts_with("PVPSL"), list(s = scale)))%>%
  mutate(across(starts_with("PVLITNUM"), list(s = scale)))%>%
  #merge country data
  left_join(country_wide, by = "CountryCode")

# change NA cells to empty
sampled2[is.na(sampled2)] <- ""


#---------------------------------------------------
## export clean data
write.csv(sampled2, "Data Analysis/Clean Datasets/Sample_piaac_clean.csv", row.names = F)


