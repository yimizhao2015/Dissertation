
### Study 1 Model Fitting

library(tidyverse)#R package for data manipulation
library(lme4)#R package for HLM modeling
library(lmerTest)#R package for hypothesis testing

##################################################################
#################### Data Preparation ############################
##################################################################
# load data
dt <- read.csv("Data Analysis/Clean Datasets/Sample_piaac_clean.csv", stringsAsFactors = T, na.strings = c("NA", ""))

# prepare data for modeling: re-code variable, re-level reference level, etc.
dt <- dt %>% mutate(Employed = dplyr::recode(Employment, "Employed" = 1, "Unemployed" = 0))
dt$Education <- relevel(dt$Education, ref = "Below high school")

##################################################################
#################### GLMM Model Fitting ##########################
##################################################################
#Generalized Linear Mixed Model (GLMM) Model Fitting
## Define functions for GLMM model fitting
# define ^2 function
PVU = function(x){return (x^2)}

# define the function for Study 1
study1Func = function(a, b, c = NULL, d = NULL){
  #fixed effects statistics (estimate, standard error, and df)
  mod.est = data.frame()
  mod.se = data.frame()
  #random effects statistics (variance and standard deviation)
  mod.v = data.frame()
  mod.std = data.frame()
  #AIC
  mod.aic = c()
  
  for(i in 1:10){
    formu = as.formula(paste("Employed ~ 1 + ", a[i], b, c[i], d, sep = ""))
    mod = glmer(formu, data = dt, family = binomial(link = "logit"), 
                control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
    
    mod.est =  mod.est %>% rbind(coef(summary(mod))[, 1])
    mod.se =  mod.se %>% rbind(coef(summary(mod))[, 2])
    mod.v =  mod.v %>% rbind(data.frame(VarCorr(mod))[, 4])
    mod.std =  mod.std %>% rbind(data.frame(VarCorr(mod))[, 5])
    mod.aic = mod.aic %>% append(AIC(mod))
  }
  row.names(mod.est) = paste("est_pv", 1:10, sep = "")
  row.names(mod.se) = paste("se_pv", 1:10, sep = "")
  row.names(mod.v) = paste("v_pv", 1:10, sep = "")
  row.names(mod.std) = paste("std_pv", 1:10, sep = "")
  
  #extracting/calculating fixed effects statistics 
  feffects = data.frame(var = row.names(coef(summary(mod))))%>%
    cbind(t(mod.est), t(mod.se)) %>%
    rowwise()%>%
    mutate(theta = mean(c_across(est_pv1:est_pv10)),#population estimate of plausible values
           b = var(c_across(est_pv1:est_pv10)),#imputed variance of plausible values 
           u = mean(PVU(c_across(se_pv1:se_pv10))))%>%#sampling variance
    ungroup()%>%
    mutate(v = u+(1+1/10)*b)%>%#imputed variance of plausible values
    mutate(se_new = sqrt(v),
           z_value = theta/se_new,
           p_value = round(pnorm(abs(z_value), lower.tail = F)*2, 5),
           p_sign = case_when(
             p_value <= 0.001 ~ "***",
             p_value > 0.001 & p_value <= 0.01 ~ "**",
             p_value > 0.01 & p_value <= 0.05 ~ "*",
             p_value > 0.05 & p_value <= 0.1 ~ ".",
             TRUE ~ ""),
           reported = paste(round(theta, 3), " (", round(se_new, 3), ")", p_sign, sep = ""))
  
  #extracting/calculating random effects statistics 
  reffects.vars = data.frame(VarCorr(mod))%>% 
    mutate(dummy = paste(grp, var1, var2, sep = "-"))%>%
    dplyr::select(dummy)
  
  reffects = data.frame(var = reffects.vars)%>%
    cbind(t(mod.v), t(mod.std))%>%
    rowwise()%>%
    mutate(variance = mean(c_across(v_pv1:v_pv10)),#imputed variance of plausible values
           std = mean(c_across(std_pv1:std_pv10)))%>%#imputed std of plausible values
    ungroup()%>%
    mutate(reported = paste(round(variance, 3), " (", round(std, 3), ")", sep = ""))
  
  #put together
  outlist = list("Fixed" = feffects,
                 "Random" = reffects, 
                 "AIC" = mean(mod.aic))
  
  return(outlist)
}

#----------------------------------------------------------------------------------------
### Generalized Linear Mixed Model (GLMM) Model Fitting  
## pv variables
pvlitnum = c("PVLITNUM1_s","PVLITNUM2_s","PVLITNUM3_s","PVLITNUM4_s", "PVLITNUM5_s","PVLITNUM6_s","PVLITNUM7_s","PVLITNUM8_s","PVLITNUM9_s", "PVLITNUM10_s") 
pvps =  c("PVPSL1_s", "PVPSL2_s", "PVPSL3_s", "PVPSL4_s", "PVPSL5_s", "PVPSL6_s", "PVPSL7_s", "PVPSL8_s", "PVPSL9_s","PVPSL10_s")

## (1) GLMM Null Model: the baseline, no explanatory variables
m0 <- glmer(Employed ~ 1 + (1 |Country), data = dt, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
summary(m0)

## (2) Literacy & Numeracy GLMM modeling -- KEEP ALL LEVEL 1 VARIABLES
## 2.1 Random intercept model (1|Country)
 #separate models for all ten PVs (PVLITNUM1_s to PVLITNUM10_s), random intercept for Literacy & Numeracy, other level 1 & level 2 variables
 #refined (remove all level 2 variables - GDPPCAP, Poverty, HSCompletion, CollegeCompletion, Mobile, Internet)
formu.lnm1 = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1|Country)"
formu.lnm1.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1|Country)"

lnm1 = study1Func(a = pvlitnum, b = formu.lnm1)
lnm1_ref = study1Func(a = pvlitnum, b = formu.lnm1.ref)

## 2.2 Random intercept and random slope (1 + PVLITNUM|Country)
 #refined (remove all level 2 variables)
formu.lnm2.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm2.d = " |Country)"
formu.lnm2.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

lnm2 = study1Func(a = pvlitnum, b = formu.lnm2.b, c = pvlitnum, d = formu.lnm2.d)
lnm2_ref = study1Func(a = pvlitnum, b = formu.lnm2.b.ref, c = pvlitnum, d = formu.lnm2.d)

## 2.3 Random intercept and random slope (1 + PVLITNUM + Gender|Country)
 #refined (remove all level 2 variables)
formu.lnm3.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm3.d = " + Gender|Country)"
formu.lnm3.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

lnm3 = study1Func(a = pvlitnum, b = formu.lnm3.b, c = pvlitnum, d = formu.lnm3.d)
lnm3_ref = study1Func(a = pvlitnum, b = formu.lnm3.b.ref, c = pvlitnum, d = formu.lnm3.d)

## 2.4 Random intercept and random slope (1 + PVLITNUM + STEM|Country)
#refined (remove all level 2 variables)
formu.lnm4.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm4.d = " + STEM|Country)"
formu.lnm4.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

lnm4 = study1Func(a = pvlitnum, b = formu.lnm4.b, c = pvlitnum, d = formu.lnm4.d)
lnm4_ref = study1Func(a = pvlitnum, b = formu.lnm4.b.ref, c = pvlitnum, d = formu.lnm4.d)

## 2.5 Random intercept and random slope (1 + PVLITNUM + Training|Country)
#refined (remove all level 2 variables)
formu.lnm5.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm5.d = " + Training|Country)"
formu.lnm5.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

lnm5 = study1Func(a = pvlitnum, b = formu.lnm5.b, c = pvlitnum, d = formu.lnm5.d)
lnm5_ref = study1Func(a = pvlitnum, b = formu.lnm5.b.ref, c = pvlitnum, d = formu.lnm5.d)

## 2.6 Random intercept and random slope (1 + PVLITNUM + Gender + STEM|Country)
#refined (remove all level 2 variables)
formu.lnm6.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm6.d = " + Gender + STEM|Country)"
formu.lnm6.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

lnm6 = study1Func(a = pvlitnum, b = formu.lnm6.b, c = pvlitnum, d = formu.lnm6.d)
lnm6_ref = study1Func(a = pvlitnum, b = formu.lnm6.b.ref, c = pvlitnum, d = formu.lnm6.d)

## 2.7 Random intercept and random slope (1 + PVLITNUM + Gender + Training|Country)
#refined (remove all level 2 variables)
formu.lnm7.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm7.d = " + Gender + Training|Country)"
formu.lnm7.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "
formu.lnm7.b.ref2 = " + Gender + Age + STEM + Education + Training + Experience + Occupation + HSCompletion + (1 + " #keep HSCompletion

lnm7 = study1Func(a = pvlitnum, b = formu.lnm7.b, c = pvlitnum, d = formu.lnm7.d)
lnm7_ref = study1Func(a = pvlitnum, b = formu.lnm7.b.ref, c = pvlitnum, d = formu.lnm7.d)
lnm7_ref2 = study1Func(a = pvlitnum, b = formu.lnm7.b.ref2, c = pvlitnum, d = formu.lnm7.d)

## 2.8 Random intercept and random slope (1 + PVLITNUM + STEM + Training|Country)
#refined (remove all level 2 variables)
formu.lnm8.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm8.d = " + STEM + Training|Country)"
formu.lnm8.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

lnm8 = study1Func(a = pvlitnum, b = formu.lnm8.b, c = pvlitnum, d = formu.lnm8.d)
lnm8_ref = study1Func(a = pvlitnum, b = formu.lnm8.b.ref, c = pvlitnum, d = formu.lnm8.d)

#as.formula(paste("Employed ~ 1 + ", pvlitnum[2], formu.lnm7.b.ref, pvlitnum[2], formu.lnm7.d, sep = ""))

#---------------------------------------------------------------
## (3) PSTRE GLMM modeling
## 3.1 Random intercept model (1|Country)
#separate models for all ten PVs (PVPSL1_s to PVPSL10_s), random intercept for PSTRE, other level 1 & level 2 variables
#refined (remove all level 2 variables - Poverty, Internet, GDPPCAP, HSCompletion, Mobile, CollegeCompletion)
formu.psm1 = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1|Country)"
formu.psm1.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1|Country)"

psm1 = study1Func(a = pvps, b = formu.psm1)
psm1_ref = study1Func(a = pvps, b = formu.psm1.ref)

## 3.2 Random intercept and random slope (1 + PVPSL|Country)
#refined (remove all level 2 variables)
formu.psm2.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm2.d = " |Country)"
formu.psm2.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

psm2 = study1Func(a = pvps, b = formu.psm2.b, c = pvps, d = formu.psm2.d)
psm2_ref = study1Func(a = pvps, b = formu.psm2.b.ref, c = pvps, d = formu.psm2.d)

## 3.3 Random intercept and random slope (1 + PVPSL + Gender|Country)
#refined (remove all level 2 variables)
formu.psm3.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm3.d = " + Gender|Country)"
formu.psm3.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

psm3 = study1Func(a = pvps, b = formu.psm3.b, c = pvps, d = formu.psm3.d)
psm3_ref = study1Func(a = pvps, b = formu.psm3.b.ref, c = pvps, d = formu.psm3.d)

## 3.4 Random intercept and random slope (1 + PVPSL + STEM|Country)
#refined (remove all level 2 variables)
formu.psm4.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm4.d = " + STEM|Country)"
formu.psm4.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

psm4 = study1Func(a = pvps, b = formu.psm4.b, c = pvps, d = formu.psm4.d)
psm4_ref = study1Func(a = pvps, b = formu.psm4.b.ref, c = pvps, d = formu.psm4.d)

## 3.5 Random intercept and random slope (1 + PVPSL + Training|Country)
#refined (remove all level 2 variables)
formu.psm5.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm5.d = " + Training|Country)"
formu.psm5.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

psm5 = study1Func(a = pvps, b = formu.psm5.b, c = pvps, d = formu.psm5.d)
psm5_ref = study1Func(a = pvps, b = formu.psm5.b.ref, c = pvps, d = formu.psm5.d)

## 3.6 Random intercept and random slope (1 + PVPSL + Gender + STEM|Country)
#refined (remove all level 2 variables)
formu.psm6.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm6.d = " + Gender + STEM|Country)"
formu.psm6.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

psm6 = study1Func(a = pvps, b = formu.psm6.b, c = pvps, d = formu.psm6.d)
psm6_ref = study1Func(a = pvps, b = formu.psm6.b.ref, c = pvps, d = formu.psm6.d)

## 3.7 Random intercept and random slope (1 + PVPSL + Gender + Training|Country)
#refined (remove all level 2 variables)
formu.psm7.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm7.d = " + Gender + Training|Country)"
formu.psm7.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

psm7 = study1Func(a = pvps, b = formu.psm7.b, c = pvps, d = formu.psm7.d)
psm7_ref = study1Func(a = pvps, b = formu.psm7.b.ref, c = pvps, d = formu.psm7.d)

## 3.8 Random intercept and random slope (1 + PVPSL + STEM + Training|Country)
#refined (remove all level 2 variables)
formu.psm8.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm8.d = " + STEM + Training|Country)"
formu.psm8.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "
formu.psm8.b.ref2 = " + Gender + Age + STEM + Education + Training + Experience + Occupation + Mobile + (1 + " #keep Mobile

psm8 = study1Func(a = pvps, b = formu.psm8.b, c = pvps, d = formu.psm8.d)
psm8_ref = study1Func(a = pvps, b = formu.psm8.b.ref, c = pvps, d = formu.psm8.d)
psm8_ref2 = study1Func(a = pvps, b = formu.psm8.b.ref2, c = pvps, d = formu.psm8.d)

#as.formula(paste("Employed ~ 1 + ", pvps[1], formu.psm7.b.ref, pvps[1], formu.psm7.d, sep = ""))

#-------------------------------------------
# test models
## Random intercept and random slope model - AIC comparisons
# 1 + PVLITNUM|Country: 15733.01 (PVLITNUM1_s), 15728.97 (PVLITNUM5_s)
# 1 + PVLITNUM + Gender|Country: 15730.47 (PVLITNUM1_s), 15726.53 (PVLITNUM5_s)
# 1 + PVLITNUM + Age|Country: 15729.28 (PVLITNUM1_s), 15733.45 (PVLITNUM5_s)
# 1 + PVLITNUM + STEM|Country: 15732.23 (PVLITNUM1_s), 15727.54 (PVLITNUM5_s)
# 1 + PVLITNUM + Education|Country: 15740.04 (PVLITNUM1_s), 15734.19 (PVLITNUM5_s), model running takes a long time
# 1 + PVLITNUM + Training|Country: 15727.8 (PVLITNUM1_s), 15724.09 (PVLITNUM5_s)
# 1 + PVLITNUM + Experience|Country: 15699.05 (PVLITNUM1_s), 15694.5(PVLITNUM5_s)
# 1 + PVLITNUM + Occupation|Country: 15732.49 (PVLITNUM1_s), 15727.75 (PVLITNUM5_s), model running takes a long time
# 1 + PVLITNUM + Gender + STEM|Country: 15731.71 (PVLITNUM1_s), 15727.64 (PVLITNUM5_s)
# 1 + PVLITNUM + Gender + Training|Country: 15727.78 (PVLITNUM1_s), 15724.2 (PVLITNUM5_s)
# 1 + PVLITNUM + Gender + Experience|Country: 15693.06 (PVLITNUM1_s), 15689.2 (PVLITNUM5_s), model running takes a long time

 # for LITNUM
tmod <- glmer(Employed ~ 1 + PVLITNUM1_s + Gender + Age + STEM + Education + Training + Experience + Occupation +
              GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + PVLITNUM1_s + STEM + Training|Country),
              data = dt, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(tmod)


tmod.ref <- glmer(Employed ~ 1 + PVLITNUM1_s + Gender + Age + STEM + Education + Training + Experience + Occupation +
                  (1 + PVLITNUM1_s + STEM + Training|Country),
                  data = dt, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(tmod.ref)

 # for PSTRE
tmod <- glmer(Employed ~ 1 + PVPSL1_s + Gender + Age + STEM + Education + Training + Experience + Occupation +
              GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + PVPSL1_s + Training|Country),
              data = dt, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(tmod)
AIC(tmod)


tmod.ref <-glmer(Employed ~ 1 + PVPSL1_s + Gender + Age + STEM + Education + Training + Experience + Occupation +
                  (1 + PVPSL1_s + Training|Country),
                 data = dt, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(tmod.ref)
AIC(tmod.ref)

#-------------------------------------------------
# export model outputs
# t = psm7$Fixed
# write.csv(t, "t.csv", row.names = F)

