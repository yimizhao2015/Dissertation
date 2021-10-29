
# prepare data for study 2
library(tidyverse)

dt <- read.csv("Data Analysis/Clean Datasets/Sample_piaac_clean.csv", stringsAsFactors = F)

 #check earnings missing
earnings.missing <- dt %>% group_by(Country) %>% 
  summarize(
    N = n(),
    nas0 = sum(is.na(Earnings_raw)),
    nas = sum(is.na(Earnings))
    )%>%
  mutate(
    nas_diff = nas - nas0,
    nas_p0 = nas0/N,
    nas_p = nas/N)%>%
  arrange(desc(nas_p))

(noEearnings <- earnings.missing %>% filter(nas_p == 1))# 9 countries have no earnings info
# Country                        N  nas0   nas nas_diff nas_p0 nas_p
# <chr>                      <int> <int> <int>    <int>  <dbl> <dbl>
# 1 Austria                    858   858   858        0      1     1
# 2 Canada                    4032  4032  4032        0      1     1
# 3 Germany                    886   886   886        0      1     1
# 4 Hungary                    956   956   956        0      1     1
# 5 Peru                      1424  1424  1424        0      1     1
# 6 Singapore                 1029  1029  1029        0      1     1
# 7 Sweden                     719   719   719        0      1     1
# 8 Turkey                     985   985   985        0      1     1
# 9 United States of America   928   928   928        0      1     1


#------------------------------------------------------------------------
## Prepare study 2 original survey data (employed respondents within 26 countries reported earnings info)
dt2 <- dt %>% 
  filter(!Country %in% noEearnings$Country, Employment == "Employed")

table(is.na(dt2$Earnings))
# FALSE  TRUE 
# 18572  4169 

#------------------------------------------------------------------------
## subset original survey data (all employed with a reasonable hourly earnings), for sample descriptive summary
dt3 <- dt2 %>% filter(!is.na(Earnings)) %>% select(Weight:PVLITNUM10)

summary(dt3$Earnings)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.26    7.03   11.67   14.09   17.78  491.62 


write.csv(dt2,"Data Analysis/Clean Datasets/Study2_piaac.csv", row.names = F)
write.csv(dt3,"Data Analysis/Clean Datasets/Study2_piaac_for sample summary.csv", row.names = F)

