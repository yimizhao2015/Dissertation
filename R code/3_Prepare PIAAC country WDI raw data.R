

library(readxl)
library(tidyverse)

#load data sets: World Development Indicators (WDI) data downloaded on 8/19/2021, PIAAC country list, and selected country-level variable list
wdi <- read.csv("Data Sets/World Development Indicators (WDI)/WDI_csv_08192021/WDIData.csv", stringsAsFactors = F)
countries <- read_excel("Data Sets/PIAAC/PIAAC PUF List.xlsx", sheet = 1)
selected <- read_excel("Data Analysis/Clean Datasets/Selected dissertation variables.xlsx", sheet = 2)


# subset WDI data for 35 PIAAC countries
piaac <- wdi %>% 
  filter(Country.Code %in% countries$CountryCode, Indicator.Code %in% selected$`Series Code`)%>%
  left_join(countries[, c("CountryCode","DataCollected")], by = c("Country.Code" = "CountryCode"))%>%
  dplyr::select (CountryCode = Country.Code, Country = Country.Name, DataCollected, 
                 IndicatorName = Indicator.Name, Indicator = Indicator.Code, X2010:X2020)%>%
  #select value for indicators(closest year)
  mutate(Value1 = case_when(
                   DataCollected == "Round 1(August 2011-March 2012)" ~ X2011,
                   DataCollected == "Round 2(April 2014-March 2015)" ~ X2014,
                   DataCollected == "Round 3(July 2017-Dec 2017)" ~ X2017))


piaac[is.na(piaac)] <- ""

write.csv(piaac, "Data Analysis/Clean Datasets/piaac country_raw.csv", row.names = F)
