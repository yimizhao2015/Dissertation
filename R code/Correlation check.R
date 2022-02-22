

#Correlation check
library(tidyverse)

dt <- read.csv("Data Analysis/Clean Datasets/Sample_piaac_clean.csv")

## Correlation/Multicollinearity checking
#among three competencies, education (years of schooling) vs. three competencies
(c1 = dt %>% select(Schooling, PVLIT1, PVNUM1, PVPSL1) %>% cor(.,use = "na.or.complete") %>% round(2))
(c2 = dt %>% select(Schooling, PVLIT2, PVNUM2, PVPSL2) %>% cor(.,use = "na.or.complete") %>% round(2))
(c3 = dt %>% select(Schooling, PVLIT3, PVNUM3, PVPSL3) %>% cor(.,use = "na.or.complete") %>% round(2))
(c4 = dt %>% select(Schooling, PVLIT4, PVNUM4, PVPSL4) %>% cor(.,use = "na.or.complete") %>% round(2))
(c5 = dt %>% select(Schooling, PVLIT5, PVNUM5, PVPSL5) %>% cor(.,use = "na.or.complete") %>% round(2))
(c6 = dt %>% select(Schooling, PVLIT6, PVNUM6, PVPSL6) %>% cor(.,use = "na.or.complete") %>% round(2))
(c7 = dt %>% select(Schooling, PVLIT7, PVNUM7, PVPSL7) %>% cor(.,use = "na.or.complete") %>% round(2))
(c8 = dt %>% select(Schooling, PVLIT8, PVNUM8, PVPSL8) %>% cor(.,use = "na.or.complete") %>% round(2))
(c9 = dt %>% select(Schooling, PVLIT9, PVNUM9, PVPSL9) %>% cor(.,use = "na.or.complete") %>% round(2))
(c10 = dt %>% select(Schooling, PVLIT10, PVNUM10, PVPSL10) %>% cor(.,use = "na.or.complete") %>% round(2))

#schooling vs. literacy (r = 0.285)
(SCHnLIT = c(c1[1,2], c2[1,2], c3[1,2], c4[1,2], c5[1,2], c6[1,2], c7[1,2], c8[1,2], c9[1,2], c10[1,2]))
mean(SCHnLIT)
#schooling vs. numeracy (r = 0.265)
(SCHnNUM = c(c1[1,3], c2[1,3], c3[1,3], c4[1,3], c5[1,3], c6[1,3], c7[1,3], c8[1,3], c9[1,3], c10[1,3]))
mean(SCHnNUM)
#schooling vs. PSTRE (r = 0.262)
(SCHnPSL = c(c1[1,4], c2[1,4], c3[1,4], c4[1,4], c5[1,4], c6[1,4], c7[1,4], c8[1,4], c9[1,4], c10[1,4]))
mean(SCHnPSL)
#literacy vs. numeracy (r = 0.86)
(LITnNUM = c(c1[2,3], c2[2,3], c3[2,3], c4[2,3], c5[2,3], c6[2,3], c7[2,3], c8[2,3], c9[2,3], c10[2,3]))
mean(LITnNUM)
#literacy vs. PSTRE (r = 0.789)
(LITnPSL = c(c1[2,4], c2[2,4], c3[2,4], c4[2,4], c5[2,4], c6[2,4], c7[2,4], c8[2,4], c9[2,4], c10[2,4]))
mean(LITnPSL)
#numeracy vs. PSTRE (r = 0.755)
(NUMnPSL = c(c1[3,4], c2[3,4], c3[3,4], c4[3,4], c5[3,4], c6[3,4], c7[3,4], c8[3,4], c9[3,4], c10[3,4]))
mean(NUMnPSL)