
# For Study 2 final selected model plots

library(tidyverse)# R package for data manipulation and plots
library(lme4)#R package for HLM modeling
library(lmerTest)#R package for hypothesis testing
library(tidytext)#R package for sorting ggplot plots

## Data preparation
# load data
dt <- read.csv("Data Analysis/Clean Datasets/Study2_piaac_original.csv", stringsAsFactors = T, na.strings = c("NA", ""))

# prepare data for modeling: log earnings, re-level education reference level
dt <- dt %>% mutate(LogEarnings = log(Earnings))
dt$Education <- relevel(dt$Education, ref = "Below high school")

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

#--------------------------------------------------------------------------------
### Plots for models
#plausible variables
pvlitnum = c("PVLITNUM1_s","PVLITNUM2_s","PVLITNUM3_s","PVLITNUM4_s", "PVLITNUM5_s","PVLITNUM6_s","PVLITNUM7_s","PVLITNUM8_s","PVLITNUM9_s", "PVLITNUM10_s") 
pvps =  c("PVPSL1_s", "PVPSL2_s", "PVPSL3_s", "PVPSL4_s", "PVPSL5_s", "PVPSL6_s", "PVPSL7_s", "PVPSL8_s", "PVPSL9_s","PVPSL10_s")

#-----------------------------------
## Final HLM Model Fitting on Literacy and Numeracy
 #2.6 Random intercept and random slope (1 + PVLITNUM + Gender + STEM|Country)
 #refined (remove Poverty, Mobile, CollegeCompletion)
formu.lnm6.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.lnm6.d = " + Gender + STEM|Country)"
formu.lnm6.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

lnm6_ref = study2Func(a = pvlitnum, b = formu.lnm6.b.ref, c = pvlitnum, d = formu.lnm6.d)

#(1) Plot fixed effects: model estimates and CI
estLITNUM = lnm6_ref$Fixed %>%
  mutate(conf.low = theta - 1.96*se_new,
         conf.high = theta + 1.96*se_new,
         est.exp = exp(theta),
         l95.exp = exp(conf.low),
         u95.exp = exp(conf.high))

estLITNUM$var = c("(Intercept)", "Literacy + Numeracy", "Male", "Age (30to34)", "STEM", "Education (Bachelor's degree or beyond)",
                   "Education (High school or equivalent)", "Education (Some college)", "Training", "Experience", 
                   "Occupation (Elementary)", "Occupation (Skilled)", "Occupation (White-collar)", "GDPPCAP", "HSCompletion", "Internet")

  #exclude "intercept" because its scale is way beyond the other parameters 
(plotLITNUM <- ggplot(estLITNUM[-1,], aes(x = var, y = est.exp, ymin = l95.exp, ymax = u95.exp)) + 
    geom_linerange() + geom_point() + ylim(c(0, 2)) +
    ylab("OR Estimate (Literacy + Numeracy)") + xlab("Parameter") + 
    geom_hline(yintercept = 1, color = "blue") + coord_flip() + theme_minimal()) 

#(2) Plot random effects: dot plots with 1.39 error bars
#model results from refined model with plausible value 1
lnm6_ref_pv1 <- lmer(LogEarnings ~ 1 + PVLITNUM1_s + Gender + Age + STEM + Education + Training + Experience + Occupation + 
                     GDPPCAP + HSCompletion + Internet + (1 + PVLITNUM1_s + Gender + STEM|Country), data = dt, REML = F)
summary(lnm6_ref_pv1)
rand(lnm6_ref_pv1)
# ANOVA-like table for random-effects: Single term deletions
# Model: LogEarnings ~ PVLITNUM1_s + Gender + Age + STEM + Education + Training + Experience + Occupation + 
#                      GDPPCAP + HSCompletion + Internet + (1 + PVLITNUM1_s + Gender + STEM | Country)
#                                                             npar logLik  AIC    LRT Df  Pr(>Chisq)    
# <none>                                                       27 -10139 20331                         
# PVLITNUM1_s in (1 + PVLITNUM1_s + Gender + STEM | Country)   23 -10179 20404 80.723  4    < 2e-16 ***
#   Gender in (1 + PVLITNUM1_s + Gender + STEM | Country)      23 -10178 20403 79.648  4    < 2e-16 ***
#   STEM in (1 + PVLITNUM1_s + Gender + STEM | Country)        23 -10144 20334 11.031  4    0.02622 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

se1 <- coef(summary(lnm6_ref_pv1)) %>% as.data.frame()#SE of random effects as parameters
r1 <- ranef(lnm6_ref_pv1)$Country %>% 
  mutate(Country = rownames(.)) %>%
  rename(LiteracyNumeracy = PVLITNUM1_s, Male = GenderM, STEM = STEMSTEM)%>%
  pivot_longer(-Country, names_to = "ranef", values_to = "est")%>%
  mutate(se = case_when(
    ranef == "(Intercept)" ~ se1["(Intercept)", "Std. Error"],
    ranef == "LiteracyNumeracy" ~ se1["PVLITNUM1_s", "Std. Error"],
    ranef == "Male" ~ se1["GenderM", "Std. Error"],
    ranef == "STEM" ~ se1["STEMSTEM", "Std. Error"]
  )) %>%
  mutate(Country = fct_reorder(Country, desc(Country)))

 #plot of random intercept and slope estimates with ±1.39 SE error bars 
ggplot(r1, aes(x = Country, y = est, ymin = est - 1.39 *se, ymax = est + 1.39*se)) +
  geom_pointrange(size = 0.2) + 
  ylab("Natural logarithm estimate of hourly earnings") +
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
  ylab("Natural logarithm estimate of hourly earnings") +
  xlab(" ") +
  theme_bw()

#-----------------------------------
## Final HLM Model Fitting on PSTRE
 #3.6 Random intercept and random slope (1 + PVPSL + Gender + STEM|Country)
 #refined (remove Poverty, Mobile, CollegeCompletion)
formu.psm6.b = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + Poverty + HSCompletion + CollegeCompletion + Mobile + Internet + (1 + "
formu.psm6.d = " + Gender + STEM|Country)"
formu.psm6.b.ref = " + Gender + Age + STEM + Education + Training + Experience + Occupation + GDPPCAP + HSCompletion + Internet + (1 + "

psm6_ref = study2Func(a = pvps, b = formu.psm6.b.ref, c = pvps, d = formu.psm6.d)

#(1) Plot fixed effects: model estimates and CI
estPSTRE = psm6_ref$Fixed %>%
  mutate(conf.low = theta - 1.96*se_new,
         conf.high = theta + 1.96*se_new,
         est.exp = exp(theta),
         l95.exp = exp(conf.low),
         u95.exp = exp(conf.high))

estPSTRE$var = c("(Intercept)", "PSTRE", "Male", "Age (30to34)", "STEM", "Education (Bachelor's degree or beyond)",
                 "Education (High school or equivalent)", "Education (Some college)", "Training", "Experience", 
                 "Occupation (Elementary)", "Occupation (Skilled)", "Occupation (White-collar)", "GDPPCAP", "HSCompletion", "Internet")

 #exclude "intercept" because its scale is way beyond the other parameters
(plotPSTRE <- ggplot(estPSTRE[-1,], aes(x = var, y = est.exp, ymin = l95.exp, ymax = u95.exp)) + 
  geom_linerange() + geom_point() +  ylim(c(0, 2)) +
  ylab("OR Estimate (PSTRE)") + xlab("Parameter") + 
  geom_hline(yintercept = 1, color = "blue") + coord_flip() + theme_minimal()) 

#(2) Plot random effects: dot plots with 1.39 error bars 
#model results from refined model with plausible value 1
psm6_ref_pv1 <- lmer(LogEarnings ~ 1 + PVPSL1_s + Gender + Age + STEM + Education + Training + Experience + Occupation + 
                     GDPPCAP + HSCompletion + Internet + (1 + PVPSL1_s + Gender + STEM|Country), data = dt, REML = F)
summary(psm6_ref_pv1)
rand(psm6_ref_pv1)
# ANOVA-like table for random-effects: Single term deletions
# Model: LogEarnings ~ PVPSL1_s + Gender + Age + STEM + Education + Training + Experience + Occupation + 
#                      GDPPCAP + HSCompletion + Internet + (1 + PVPSL1_s + Gender + STEM | Country)
#                                                      npar  logLik   AIC    LRT Df  Pr(>Chisq)    
# <none>                                                 27 -8321.4 16697                         
# PVPSL1_s in (1 + PVPSL1_s + Gender + STEM | Country)   23 -8346.5 16739 50.187  4  3.300e-10 ***
#   Gender in (1 + PVPSL1_s + Gender + STEM | Country)   23 -8356.3 16759 69.749  4  2.565e-14 ***
#   STEM in (1 + PVPSL1_s + Gender + STEM | Country)     23 -8327.8 16702 12.757  4    0.01253 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

se2 <- coef(summary(psm6_ref_pv1)) %>% as.data.frame()#SE of random effects as parameters
r2 <- ranef(psm6_ref_pv1)$Country %>% 
  mutate(Country = rownames(.)) %>%
  rename(PSTRE = PVPSL1_s, Male = GenderM, STEM = STEMSTEM)%>%
  pivot_longer(-Country, names_to = "ranef", values_to = "est")%>%
  mutate(se = case_when(
    ranef == "(Intercept)" ~ se2["(Intercept)", "Std. Error"],
    ranef == "PSTRE" ~ se2["PVPSL1_s", "Std. Error"],
    ranef == "Male" ~ se2["GenderM", "Std. Error"],
    ranef == "STEM" ~ se2["STEMSTEM", "Std. Error"]
  )) %>%
  mutate(Country = fct_reorder(Country, desc(Country)))

#plot of random intercept and slope estimates with ±1.39 SE error bars 
ggplot(r2, aes(x = Country, y = est, ymin = est - 1.39 *se, ymax = est + 1.39*se)) +
  geom_pointrange(size = 0.2) + 
  ylab("Natural logarithm estimate of hourly earnings") +
  xlab(" ") +
  coord_flip() +
  facet_wrap(~ ranef) +
  theme_bw()

#plot - sorted by est
r2 %>% mutate(dummy = tidytext::reorder_within(Country, est, ranef)) %>%
  ggplot(aes(x = dummy, y = est, ymin = est - 1.39 *se, ymax = est + 1.39*se)) +
  geom_pointrange(size = 0.2) + 
  facet_wrap(~ ranef, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  ylab("Natural logarithm estimate of hourly earnings") +
  xlab(" ") +
  theme_bw()

#--------------------------------
#export random effects 
# write.csv(r1, "r1.csv", row.names = F)
# write.csv(r2, "r2.csv", row.names = F)