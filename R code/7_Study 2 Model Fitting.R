
### Study 2 Model Fitting on Original Employed data

library(tidyverse)#R package for data manipulation
library(lme4)#R package for HLM modeling
library(lmerTest)#R package for hypothesis testing

##################################################################
#################### Data Preparation ############################
##################################################################
# load data
dt <- read.csv("Data Analysis/Clean Datasets/Study2_piaac.csv", stringsAsFactors = T, na.strings = c("NA", ""))

# prepare data for modeling: log earnings, re-level education reference level
dt <- dt %>% mutate(LogEarnings = log(Earnings))
dt$Education <- relevel(dt$Education, ref = "Below high school")

# explore the data
par(mfrow = c(2, 1))
hist(dt$Earnings); hist(dt$LogEarnings)

table(dt$Employment, is.na(dt$Earnings))
table(dt$Employment, is.na(dt$Earnings_raw))

# check missing-ness of Earnings
 #earnings missing from 6.8% (Norway) to 39.8% (Greece)
earnings.missing <- dt %>% group_by(Country, Employment) %>% 
  summarize(
    N = n(),
    nas = sum(is.na(Earnings)))%>%
  mutate(nas_p = nas/N)%>%
  arrange(Employment, desc(nas_p))

##################################################################
############## Standard Mincer Equation Fitting ##################
##################################################################
# standard Mincer earnings modeling
mincer.mod <- lm(LogEarnings ~ Schooling + Experience + I(Experience^2), data = dt)
summary(mincer.mod)
exp(coef(mincer.mod))
confint(mincer.mod)
AIC(mincer.mod)

# refined standard Mincer earnings modeling
mincer.mod2 <- lm(LogEarnings ~ Schooling + Experience, data = dt)
summary(mincer.mod2)
exp(coef(mincer.mod2))
confint(mincer.mod2)
AIC(mincer.mod2)

anova(mincer.mod, mincer.mod2)

##################################################################
#################### HLM Model Fitting ###########################
##################################################################
## Define functions for HLM model fitting
# define ^2 function
PVU = function(x){return (x^2)}

# define the function for Study 2
study2Func = function(a, b, c = NULL, d = NULL){
  
  #fixed effects statistics (estimate, standard error, and degree of freedom)
  mod.est = data.frame()
  mod.se = data.frame()
  mod.df = data.frame()
  #random effects statistics (variance and standard deviation)
  mod.v = data.frame()
  mod.std = data.frame()
  #AIC
  mod.aic = c()
  
  for(i in 1:10){
    formu = as.formula(paste("LogEarnings ~ 1 + ", a[i], b, c[i], d, sep = ""))
    mod = lmer(formu, data = dt, REML = F)
    
    mod.est =  mod.est %>% rbind(coef(summary(mod))[, 1])
    mod.se =  mod.se %>% rbind(coef(summary(mod))[, 2])
    mod.df =  mod.df %>% rbind(coef(summary(mod))[, 3])
    mod.v =  mod.v %>% rbind(data.frame(VarCorr(mod))[, 4])
    mod.std =  mod.std %>% rbind(data.frame(VarCorr(mod))[, 5])
    mod.aic = mod.aic %>% append(AIC(mod))
  }
  row.names(mod.est) = paste("est_pv", 1:10, sep = "")
  row.names(mod.se) = paste("se_pv", 1:10, sep = "")
  row.names(mod.df) = paste("df_pv", 1:10, sep = "")
  row.names(mod.v) = paste("v_pv", 1:10, sep = "")
  row.names(mod.std) = paste("std_pv", 1:10, sep = "")
  
  #extracting/calculating fixed effects statistics 
  feffects = data.frame(var = row.names(coef(summary(mod))))%>%
    cbind(t(mod.est), t(mod.se), t(mod.df)) %>%
    rowwise()%>%
    mutate(theta = mean(c_across(est_pv1:est_pv10)),#population estimate of plausible values
           df = round(mean(c_across(df_pv1:df_pv10)), 0),#imputed degree of freedom
           b = var(c_across(est_pv1:est_pv10)),#imputed variance of plausible values 
           u = mean(PVU(c_across(se_pv1:se_pv10))))%>%#sampling variance
    ungroup()%>%
    mutate(v = u+(1+1/10)*b)%>%#imputed variance of plausible values
    mutate(se_new = sqrt(v),
           t_value = theta/se_new,
           p_value = round(pt(abs(t_value), df, lower.tail = F)*2, 5),#two-tailed p-value
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

#------------------------------------------------------------------------
### HLM model fitting
## plausible variables
pvlitnum = c("PVLITNUM1_s","PVLITNUM2_s","PVLITNUM3_s","PVLITNUM4_s", "PVLITNUM5_s","PVLITNUM6_s","PVLITNUM7_s","PVLITNUM8_s","PVLITNUM9_s", "PVLITNUM10_s") 
pvps =  c("PVPSL1_s", "PVPSL2_s", "PVPSL3_s", "PVPSL4_s", "PVPSL5_s", "PVPSL6_s", "PVPSL7_s", "PVPSL8_s", "PVPSL9_s","PVPSL10_s")

## (1) HLM Null Model: the baseline, no explanatory variables
m0 <- lmer(LogEarnings ~ 1 + (1 |Country), data = dt, REML = F)
summary(m0)

#------------------------------------------------------------------------
## (2) Literacy & Numeracy HLM modeling
## 2.1 Random intercept model (1|Country)
 #separate models for all ten PVs (PVLITNUM1_s to PVLITNUM10_s), random intercept for Literacy & Numeracy, other level 1 & level 2 variables
 #refined (remove Poverty, Mobile, CollegeCompletion)
formu.lnm1 = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1|Country)"
formu.lnm1.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1|Country)"

lnm1 = study2Func(a = pvlitnum, b = formu.lnm1)
lnm1_ref = study2Func(a = pvlitnum, b = formu.lnm1.ref)

## 2.2 Random intercept and random slope (1 + PVLITNUM|Country)
 #refined (remove Mobile, Poverty, CollegeCompletion)
formu.lnm2.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm2.d = " |Country)"
formu.lnm2.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

lnm2 = study2Func(a = pvlitnum, b = formu.lnm2.b, c = pvlitnum, d = formu.lnm2.d)
lnm2_ref = study2Func(a = pvlitnum, b = formu.lnm2.b.ref, c = pvlitnum, d = formu.lnm2.d)

## 2.3 Random intercept and random slope (1 + PVLITNUM + Gender|Country)
 #refined (remove Mobile, Poverty, CollegeCompletion)
formu.lnm3.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm3.d = " + Gender|Country)"
formu.lnm3.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

lnm3 = study2Func(a = pvlitnum, b = formu.lnm3.b, c = pvlitnum, d = formu.lnm3.d)
lnm3_ref = study2Func(a = pvlitnum, b = formu.lnm3.b.ref, c = pvlitnum, d = formu.lnm3.d)

## 2.4 Random intercept and random slope (1 + PVLITNUM + STEM|Country)
 #refined (remove Mobile, Poverty, CollegeCompletion)
formu.lnm4.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm4.d = " + STEM|Country)"
formu.lnm4.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

lnm4 = study2Func(a = pvlitnum, b = formu.lnm4.b, c = pvlitnum, d = formu.lnm4.d)
lnm4_ref = study2Func(a = pvlitnum, b = formu.lnm4.b.ref, c = pvlitnum, d = formu.lnm4.d)

## 2.5 Random intercept and random slope (1 + PVLITNUM + Training|Country)
 #refined (remove Mobile, Poverty, CollegeCompletion)
formu.lnm5.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm5.d = " + Training|Country)"
formu.lnm5.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

lnm5 = study2Func(a = pvlitnum, b = formu.lnm5.b, c = pvlitnum, d = formu.lnm5.d)
lnm5_ref = study2Func(a = pvlitnum, b = formu.lnm5.b.ref, c = pvlitnum, d = formu.lnm5.d)

## 2.6 Random intercept and random slope (1 + PVLITNUM + Gender + STEM|Country)
 #refined (remove Poverty, Mobile, CollegeCompletion)
formu.lnm6.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm6.d = " + Gender + STEM|Country)"
formu.lnm6.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

lnm6 = study2Func(a = pvlitnum, b = formu.lnm6.b, c = pvlitnum, d = formu.lnm6.d)
lnm6_ref = study2Func(a = pvlitnum, b = formu.lnm6.b.ref, c = pvlitnum, d = formu.lnm6.d)

#as.formula(paste("LogEarnings ~ 1 + ", pvlitnum[2], formu.lnm6.b.ref, pvlitnum[2], formu.lnm6.d, sep = ""))

#------------------------------------------------------------------------
## (3) PSTRE HLM model
## 3.1 Random intercept (1|Country)
#separate models for all ten PVs (PVPSL1_s to PVPSL10_s), random intercept for PSTRE, other level 1 & level 2 variables
#model formulas, refined (remove Mobile, Poverty, CollegeCompletion)
formu.psm1 = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1|Country)"
formu.psm1.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1|Country)"

psm1 = study2Func(a = pvps, b = formu.psm1)
psm1_ref = study2Func(a = pvps, b = formu.psm1.ref)

## 3.2 Random intercept and random slope (1 + PVPSL|Country)
#refined (remove Mobile, Poverty, CollegeCompletion)
formu.psm2.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm2.d = " |Country)"
formu.psm2.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

psm2 = study2Func(a = pvps, b = formu.psm2.b, c = pvps, d = formu.psm2.d)
psm2_ref = study2Func(a = pvps, b = formu.psm2.b.ref, c = pvps, d = formu.psm2.d)


## 3.3 Random intercept and random slope (1 + PVPSL + Gender|Country)
#refined (remove Poverty, Mobile, CollegeCompletion)
formu.psm3.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm3.d = " + Gender|Country)"
formu.psm3.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

psm3 = study2Func(a = pvps, b = formu.psm3.b, c = pvps, d = formu.psm3.d)
psm3_ref = study2Func(a = pvps, b = formu.psm3.b.ref, c = pvps, d = formu.psm3.d)

## 3.4 Random intercept and random slope (1 + PVPSL + STEM|Country)
#refined (remove Mobile, Poverty, CollegeCompletion)
formu.psm4.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm4.d = " + STEM|Country)"
formu.psm4.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

psm4 = study2Func(a = pvps, b = formu.psm4.b, c = pvps, d = formu.psm4.d)
psm4_ref = study2Func(a = pvps, b = formu.psm4.b.ref, c = pvps, d = formu.psm4.d)

## 3.5 Random intercept and random slope (1 + PVPSL + Training|Country)
#refined (remove Mobile, Poverty, CollegeCompletion)
formu.psm5.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm5.d = " + Training|Country)"
formu.psm5.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

psm5 = study2Func(a = pvps, b = formu.psm5.b, c = pvps, d = formu.psm5.d)
psm5_ref = study2Func(a = pvps, b = formu.psm5.b.ref, c = pvps, d = formu.psm5.d)

## 3.6 Random intercept and random slope (1 + PVPSL + Gender + STEM|Country)
#refined (remove Poverty, Mobile, CollegeCompletion)
formu.psm6.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm6.d = " + Gender + STEM|Country)"
formu.psm6.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

psm6 = study2Func(a = pvps, b = formu.psm6.b, c = pvps, d = formu.psm6.d)
psm6_ref = study2Func(a = pvps, b = formu.psm6.b.ref, c = pvps, d = formu.psm6.d)

#as.formula(paste("LogEarnings ~ 1 + ", pvps[2], formu.psm6.b.ref, pvps[2], formu.psm6.d, sep = ""))

#------------------------------------------------------------------------
# test model
## Random intercept and random slope model - AIC comparisons
# 1 + PVLITNUM|Country: 18797.5 (PVLITNUM1_s), 18777.0 (PVLITNUM5_s)
# 1 + PVLITNUM + Gender|Country: 18767.6 (PVLITNUM1_s), 18745.8 (PVLITNUM5_s)
# 1 + PVLITNUM + Age|Country: 18789.4 (PVLITNUM1_s), 18766.7 (PVLITNUM5_s), some PVs Singular or failed to converge (PVLITNUM1, PVLITNUM2, PVLITNUM3, PVLITNUM6, PVLITNUM8, PVLITNUM10)
# 1 + PVLITNUM + STEM|Country: 18784.1 (PVLITNUM1_s), 18763.2 (PVLITNUM5_s)
# 1 + PVLITNUM + Education|Country: 18763.6 (PVLITNUM1_s), 18743.0 (PVLITNUM5_s), all PVs Singular, model failed to converge with 1 negative eigenvalue
# 1 + PVLITNUM + Training|Country: 18767.9 (PVLITNUM1_s), 18746.8 (PVLITNUM5_s)
# 1 + PVLITNUM + Experience|Country: 18784.4 (PVLITNUM1_s), 18759.9 (PVLITNUM5_s), all PVs, model failed to converge with max|grad|
# 1 + PVLITNUM + Occupation|Country: 18680.4 (PVLITNUM1_s), 18661.3 (PVLITNUM5_s), all PVs Singular
# 1 + PVLITNUM + Gender + STEM|Country: 18765.1 (PVLITNUM1_s), 18743.0 (PVLITNUM5_s), some PVs model failed to converge (PVLITNUM2, PVLITNUM4, PVLITNUM7)
# 1 + PVLITNUM + Gender + Training|Country: 18738.0 (PVLITNUM1_s), 18716.1 (PVLITNUM5_s), some PVs singular or failed to converge (PVLITNUM1, PVLITNUM2, PVLITNUM6, PVLITNUM10)


# tmod <- lmer(LogEarnings ~ 1 + PVLITNUM9_s + Gender + Age + STEM + Education + Training + Experience + Occupation +
#             GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + PVLITNUM9_s + Training|Country),
#             data = dt, REML = F)

tmod <- lmer(LogEarnings ~ 1 + PVPSL1_s + Gender + Age + STEM + Education + Training + Experience + Occupation + 
             GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + PVPSL7_s + Gender|Country),
             data = dt, REML = F)

summary(tmod)
lmerTest::rand(tmod)
AIC(tmod)

#try all available optimizers
tmod_allfit = summary(allFit(tmod))
t(tmod_allfit$fixef)
t(tmod_allfit$theta)
t(tmod_allfit$sdcor)

#tmod.ref <- lmer(LogEarnings ~ 1 + PVLITNUM9_s + Gender + Age + STEM + Education + Training + Experience + Occupation + 
#                    GDPPCAP + HSCompletion + Internet + (1 + PVLITNUM9_s + Training|Country), data = dt, REML = F)

tmod.ref <- lmer(LogEarnings ~ 1 + PVPSL3_s + Gender + Age + STEM + Education + Training + Experience + Occupation + 
                 GDPPCAP + HSCompletion + Internet + (1 + PVPSL3_s + Training|Country),
                 data = dt, REML = F)
summary(tmod.ref)
lmerTest::rand(tmod.ref)
AIC(tmod.ref)

#try all available optimizers
tmod.ref_allfit = summary(allFit(tmod.ref))
t(tmod.ref_allfit$fixef)
t(tmod.ref_allfit$theta)
t(tmod.ref_allfit$sdcor)

#-------------------------------------------------
## Extract the model outcomes
# t = psm6_ref$Random
# write.csv(t, "t.csv", row.names = F)

# aic_lnmodels = data.frame(
#   model = c("lnm1", "lnm2", "lnm3", "lnm4", "lnm5", "lnm6", "lnm1_ref", "lnm2_ref", "lnm3_ref", "lnm4_ref", "lnm5_ref", "lnm6_ref"),
#   aic = c(lnm1$AIC, lnm2$AIC, lnm3$AIC, lnm4$AIC, lnm5$AIC, lnm6$AIC, lnm1_ref$AIC, lnm2_ref$AIC, lnm3_ref$AIC, lnm4_ref$AIC, lnm5_ref$AIC, lnm6_ref$AIC))
# 
# aic_psmodels = data.frame(
#   model = c("psm1", "psm2", "psm3", "psm4", "psm5", "psm6", "psm1_ref", "psm2_ref", "psm3_ref", "psm4_ref", "psm5_ref", "psm6_ref"),
#   aic = c(psm1$AIC, psm2$AIC, psm3$AIC, psm4$AIC, psm5$AIC, psm6$AIC, psm1_ref$AIC, psm2_ref$AIC, psm3_ref$AIC, psm4_ref$AIC, psm5_ref$AIC, psm6_ref$AIC))
# 


