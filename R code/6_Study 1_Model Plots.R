
# For Study 1 final selected model plots

library(tidyverse)# R package for data manipulation and plots
library(lme4)#R package for HLM modeling
library(lmerTest)#R package for hypothesis testing
library(tidytext)#R package for sorting ggplot plots

## Data preparation
dt <- read.csv("Data Analysis/Clean Datasets/Sample_piaac_clean.csv", stringsAsFactors = T, na.strings = c("NA", ""))

# prepare data for modeling: re-code variable, re-level reference level, etc.
dt <- dt %>% mutate(Employed = dplyr::recode(Employment, "Employed" = 1, "Unemployed" = 0))
dt$Education <- relevel(dt$Education, ref = "Below high school")

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

#--------------------------------------------------------------------------------
### Plots for models
# pv variables
pvlitnum = c("PVLITNUM1_s","PVLITNUM2_s","PVLITNUM3_s","PVLITNUM4_s", "PVLITNUM5_s","PVLITNUM6_s","PVLITNUM7_s","PVLITNUM8_s","PVLITNUM9_s", "PVLITNUM10_s") 
pvps =  c("PVPSL1_s", "PVPSL2_s", "PVPSL3_s", "PVPSL4_s", "PVPSL5_s", "PVPSL6_s", "PVPSL7_s", "PVPSL8_s", "PVPSL9_s","PVPSL10_s")

#-----------------------------------
## Final GLMM Model Fitting on Literacy and Numeracy
#2.3 Random intercept and random slope (1 + PVLITNUM + Gender|Country)
#refined (remove all level 2 variables)
formu.lnm3.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm3.d = " + Gender|Country)"
formu.lnm3.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

lnm3_ref = study1Func(a = pvlitnum, b = formu.lnm3.b.ref, c = pvlitnum, d = formu.lnm3.d)

#(1) Plot fixed effects: model estimates and CI
estLITNUM = lnm3_ref$Fixed %>%
  mutate(conf.low = theta - 1.96*se_new,
         conf.high = theta + 1.96*se_new,
         est.exp = exp(theta),
         l95.exp = exp(conf.low),
         u95.exp = exp(conf.high))

estLITNUM$var = c("(Intercept)", "Literacy + Numeracy", "Male", "Age (30to34)", "STEM", "Education (Bachelor's degree or beyond)",
                   "Education (High school or equivalent)", "Education (Some college)", "Training", "Experience", 
                   "Occupation (Elementary)", "Occupation (Skilled)", "Occupation (White-collar)")

(plotLITNUM <- ggplot(estLITNUM, aes(x = var, y = est.exp, ymin = l95.exp, ymax = u95.exp)) + 
    geom_linerange() + geom_point() + ylim(c(0, 2.5)) +
    ylab("OR Estimate (Literacy + Numeracy)") + xlab("Parameter") + 
    geom_hline(yintercept = 1, color = "blue") + coord_flip() + theme_minimal()) 

#(2) Plot random effects: dot plots with 1.39 error bars
#model results from refined model with plausible value 1
lnm3_ref_pv1 <- glmer(Employed ~ 1 + PVLITNUM1_s + Gender + Age + Education + Training + Experience + Occupation + 
                        (1 + PVLITNUM1_s + Gender|Country), data = dt, family = binomial(link = "logit"),
                      control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

se1 <- coef(summary(lnm3_ref_pv1)) %>% as.data.frame()#SE of random effects as parameters
r1 <- ranef(lnm3_ref_pv1)$Country %>% 
  mutate(Country = rownames(.)) %>%
  rename(LiteracyNumeracy = PVLITNUM1_s, Male = GenderM)%>%
  pivot_longer(-Country, names_to = "ranef", values_to = "est")%>%
  mutate(se = case_when(
    ranef == "(Intercept)" ~ se1["(Intercept)", "Std. Error"],
    ranef == "LiteracyNumeracy" ~ se1["PVLITNUM1_s", "Std. Error"],
    ranef == "Male" ~ se1["GenderM", "Std. Error"]
  )) %>%
  mutate(Country = fct_reorder(Country, desc(Country)))

 #plot of random intercept and slope estimates with ±1.39 SE error bars 
ggplot(r1, aes(x = Country, y = est, ymin = est - 1.39 *se, ymax = est + 1.39*se)) +
  geom_pointrange(size = 0.2) + 
  ylab("Log odds estimate") +
  xlab(" ") +
  coord_flip() +
  facet_wrap(~ ranef) +
  theme_bw()

 #plot - sorted by est
r1 %>% mutate(dummy = tidytext::reorder_within(Country, est, ranef)) %>%
  ggplot(aes(x = dummy, y = est, ymin = est - 1.39 *se, ymax = est + 1.39*se)) +
  geom_pointrange(size = 0.2) + 
  facet_wrap(~ ranef, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  ylab("Log odds estimate") +
  xlab(" ") +
  theme_bw()

#-----------------------------------
## Final GLMM Model Fitting on PSTRE
 #3.3 Random intercept and random slope (1 + PVPSL + Gender|Country)
 #refined (remove all level 2 variables)
formu.psm3.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm3.d = " + Gender|Country)"
formu.psm3.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + (1 + "

psm3_ref = study1Func(a = pvps, b = formu.psm3.b.ref, c = pvps, d = formu.psm3.d)

#(1) Plot fixed effects: model estimates and CI
estPSTRE = psm3_ref$Fixed %>%
  mutate(conf.low = theta - 1.96*se_new,
         conf.high = theta + 1.96*se_new,
         est.exp = exp(theta),
         l95.exp = exp(conf.low),
         u95.exp = exp(conf.high))

estPSTRE$var = c("(Intercept)", "PSTRE", "Male", "Age (30to34)", "STEM", "Education (Bachelor's degree or beyond)",
                 "Education (High school or equivalent)", "Education (Some college)", "Training", "Experience", 
                 "Occupation (Elementary)", "Occupation (Skilled)", "Occupation (White-collar)")

(plotPSTRE <- ggplot(estPSTRE, aes(x = var, y = est.exp, ymin = l95.exp, ymax = u95.exp)) + 
  geom_linerange() + geom_point() +  ylim(c(0, 2.5)) +
  ylab("OR Estimate (PSTRE)") + xlab("Parameter") + 
  geom_hline(yintercept = 1, color = "blue") + coord_flip() + theme_minimal()) 

#(2) Plot random effects: dot plots with 1.39 error bars 
#model results from refined model with plausible value 1
psm3_ref_pv1 <- glmer(Employed ~ 1 + PVPSL1_s + Gender + Age + STEM + Education + Training + Experience + Occupation + 
                      (1 + PVPSL1_s + Gender|Country), data = dt, family = binomial(link = "logit"),
                      control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

se2 <- coef(summary(psm3_ref_pv1)) %>% as.data.frame()#SE of random effects as parameters
r2 <- ranef(psm3_ref_pv1)$Country %>% 
  mutate(Country = rownames(.)) %>%
  rename(PSTRE = PVPSL1_s, Male = GenderM)%>%
  pivot_longer(-Country, names_to = "ranef", values_to = "est")%>%
  mutate(se = case_when(
    ranef == "(Intercept)" ~ se2["(Intercept)", "Std. Error"],
    ranef == "PSTRE" ~ se2["PVPSL1_s", "Std. Error"],
    ranef == "Male" ~ se2["GenderM", "Std. Error"]
  )) %>%
  mutate(Country = fct_reorder(Country, desc(Country)))

#plot of random intercept and slope estimates with ±1.39 SE error bars 
ggplot(r2, aes(x = Country, y = est, ymin = est - 1.39 *se, ymax = est + 1.39*se)) +
  geom_pointrange(size = 0.2) + 
  ylab("Log odds estimate") +
  xlab(" ") +
  coord_flip() +
  facet_wrap(~ factor(ranef, c("(Intercept)","PSTRE","Male"))) +
  theme_bw()

#plot - sorted by est
r2 %>% mutate(dummy = tidytext::reorder_within(Country, est, ranef)) %>%
  ggplot(aes(x = dummy, y = est, ymin = est - 1.39 *se, ymax = est + 1.39*se)) +
  geom_pointrange(size = 0.2) + 
  facet_wrap(~ factor(ranef, c("(Intercept)","PSTRE","Male")), scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  ylab("Log odds estimate") +
  xlab(" ") +
  theme_bw()

#--------------------------------
#export random effects 
# write.csv(r1, "r1.csv", row.names = F)
# write.csv(r2, "r2.csv", row.names = F)