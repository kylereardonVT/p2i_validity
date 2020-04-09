# install packages

library(dplyr)
library(rio)
library(here)
library(tidyverse)
library(janitor)
library(devtools)
library(lavaan)
library(itemanalysis)
library(svMisc)
library(matrixStats)
library(pastecs)
library(psych)
library(corrr)
library(broom)
library(semTools)
library(irr)
library(semPlot)
library(sjstats)
library(heplots)
library(caret)
library(MASS)
library(haven)
library(mvnormtest)
library(MVN)

# import OQ-45 for correlations

oq45 <- import(here("finaldata", "oq45.csv")) %>%  
  characterize() %>% 
  janitor::clean_names() %>% 
  filter(time == 1) %>% 
  as.tibble()

# import BRIEF-IR for correlations

briefir <- import(here("finaldata", "briefir.csv")) %>%  
  characterize() %>% 
  janitor::clean_names() %>% 
  select(remote_client_id, time, version, gec_raw, b1:b75) %>% 
  mutate_at(vars(b1:b75), as.factor) %>% 
  na_if("Not Answered") %>% 
  mutate_at(.vars = vars(b1:b75), fct_relevel, c("Often a problem", 
                                                 "Sometimes a problem", 
                                                 "Never a problem")) %>% 
  mutate_if(is.factor, as.integer) %>% 
  filter(time == 1) %>% 
  rename(version_b = version,
         time_b = time) %>% 
  mutate(gec_mean = mean(c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17,
                           b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31, b32, b33,
                           b34, b35, b36, b37, b38, b39, b40, b41, b42, b43, b44, b45, b46, b47, b48, b49,
                           b50, b51, b52, b53, b54, b55, b56, b57, b58, b59, b60, b61, b62, b63, b64, b65,
                           b66, b67, b68, b69, b70, b71, b72, b73, b74, b75), na.rm = TRUE)) %>% 
  as.tibble()

# import BRIEF-SR for correlations

briefsr <- import(here("finaldata", "briefsr.csv")) %>%  
  characterize() %>% 
  janitor::clean_names() %>% 
  select(remote_client_id, time, version, gec_raw, b1:b75) %>% 
  mutate_at(vars(b1:b75), as.factor) %>% 
  na_if("Not Answered") %>% 
  mutate_at(.vars = vars(b1:b75), fct_relevel, c("Often a problem", 
                                                 "Sometimes a problem", 
                                                 "Never a problem")) %>%
  mutate_if(is.factor, as.integer) %>% 
  filter(time == 1) %>% 
  rename(version_b = version,
         time_b = time) %>% 
  mutate(gec_mean = mean(c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17,
                           b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31, b32, b33,
                           b34, b35, b36, b37, b38, b39, b40, b41, b42, b43, b44, b45, b46, b47, b48, b49,
                           b50, b51, b52, b53, b54, b55, b56, b57, b58, b59, b60, b61, b62, b63, b64, b65,
                           b66, b67, b68, b69, b70, b71, b72, b73, b74, b75), na.rm = TRUE)) %>% 
  as.tibble()

brief_full <- bind_rows(briefir, briefsr)

# import Master for Gender and ASD status

master <- import(here("finaldata", "master.csv")) %>%  
  characterize() %>% 
  janitor::clean_names() %>% 
  select(id, gender, asd) %>%
  mutate(gender = na_if(gender, "NB")) %>% 
  rename("remote_client_id" = "id") %>% 
  mutate(gender = factor(gender)) %>% 
  mutate(asd = factor(asd)) %>% 
  as.tibble()

# import PII-IR

piiir <- import(here("finaldata", "piiir.csv")) %>%  
  characterize() %>% 
  janitor::clean_names() %>%
  select(remote_client_id, time, version, q1:q124) %>%
  mutate_at(vars(q1:q124), as.factor) %>% 
  na_if("Not applicable or No opportunity") %>% 
  na_if("Not Answered") %>% 
  mutate_at(.vars = vars(q1:q124), fct_relevel, c("Seldom or Never when needed", 
                                                  "Sometimes when needed", 
                                                  "Often when needed",
                                                  "Almost always or Always when needed")) %>% 
  mutate_if(is.factor, as.integer) %>% 
  as.tibble()

# import PII-SR

piisr <- import(here("finaldata", "piisr.csv")) %>%  
  characterize() %>% 
  janitor::clean_names() %>% 
  select(remote_client_id, time, version, q1:q124) %>%
  mutate_at(vars(q1:q124), as.factor) %>% 
  na_if("Not applicable or No opportunity") %>% 
  na_if("Not Answered") %>% 
  mutate_at(.vars = vars(q1:q124), fct_relevel, c("Seldom or Never when needed", 
                                                  "Sometimes when needed", 
                                                  "Often when needed",
                                                  "Almost always or Always when needed")) %>% 
  mutate_if(is.factor, as.integer) %>% 
  as.tibble() 

# create full PII file

pii_full <- bind_rows(piiir, piisr)

# add demographics and subscale means to PII file

pii_full_demo <- inner_join(pii_full, master, by = "remote_client_id") %>% 
  select(remote_client_id:q124, gender, asd) %>% 
  rowwise() %>% 
  mutate(FULL = mean(c(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22, q23, q24, q25, q26, q27, q28, q29, q30, q31, q32, q33, q34, q35, q36, q37, q38, q39, q40, q41, q42, q43, q44, q45, q46, q47, q48, q49, q50, q51, q52, q53, q54, q55, q56, q57, q58, q59, q60, q61, q16, q63, q64, q65, q66, q67, q68, q69, q70, q71, q72, q73, q74, q75, q76, q77, q78, q79, q80, q81, q82, q83, q84, q85, q86, q87, q88, q89, q90, q91, q92, q93, q94, q95, q96, q97, q98, q99, q100, q101, q102, q103, q104, q105, q106, q107, q108, q109, q110, q111, q112, q113, q114, q115, q116, q117, q118, q119, q120, q121, q122, q123, q124), na.rm = TRUE),
         ACA = mean(c(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22, q23, q24, q25, q26, q27, q28, q29, q30), na.rm = TRUE),
         EMOREG = mean(c(q31, q32, q33, q34, q35, q36, q37, q38, q39, q40, q41), na.rm = TRUE),
         HEALTH = mean(c(q42, q43, q44, q45, q46, q47, q48, q49, q50, q51, q52, q53, q54, q55, q56, q57, q58), na.rm = TRUE),
         DAILY = mean(c(q59, q60, q61, q62, q63, q64, q65, q66, q67, q68, q69, q70, q71, q72, q73, q74, q75, q76, q77, q78, q79), na.rm = TRUE),
         INTER = mean(c(q80, q81, q82, q83, q84, q85, q86, q87, q88, q89, q90, q91, q92, q93, q94, q95, q96, q97, q98, q99, q100, q101, q102, q103), na.rm = TRUE),
         TECH = mean(c(q104, q105, q106, q107, q108, q109, q110, q111, q112), na.rm = TRUE),
         VOC = mean(c(q113, q114, q115, q116, q117, q118, q119, q120, q121, q122, q123, q124), na.rm = TRUE),
         ACA_TECH_VOC = mean(c(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22, q23, q24, q25, q26, q27, q28, q29, q30, q104, q105, q106, q107, q108, q109, q110, q111, q112, q113, q114, q115, q116, q117, q118, q119, q120, q121, q122, q123, q124), na.rm = TRUE),
         EMO_INTER = mean(c(q31, q32, q33, q34, q35, q36, q37, q38, q39, q40, q41, q80, q81, q82, q83, q84, q85, q86, q87, q88, q89, q90, q91, q92, q93, q94, q95, q96, q97, q98, q99, q100, q101, q102, q103), na.rm = TRUE),
         HEALTH_DAILY = mean(c(q42, q43, q44, q45, q46, q47, q48, q49, q50, q51, q52, q53, q54, q55, q56, q57, q58, q59, q60, q61, q62, q63, q64, q65, q66, q67, q68, q69, q70, q71, q72, q73, q74, q75, q76, q77, q78, q79), na.rm = TRUE),
         coursework = mean(c(q1, q2, q3, q4), na.rm = TRUE),
         initiation = mean(c(q5, q6, q7), na.rm = TRUE),
         selfadvocacy = mean(c(q8, q9, q10), na.rm = TRUE),
         study = mean(c(q11, q12, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22), na.rm = TRUE),
         timemanage = mean(c(q23, q24, q25, q26, q27, q28, q29, q30), na.rm = TRUE),
         coping = mean(c(q31, q32, q33, q34, q35, q36, q37, q38), na.rm = TRUE),
         control = mean(c(q39, q40, q41), na.rm = TRUE),
         diet = mean(c(q42, q43, q44), na.rm = TRUE), 
         selfcare = mean(c(q45, q46, q47, q48, q49, q50), na.rm = TRUE),
         risky = mean(c(q51, q52, q53, q54), na.rm = TRUE),
         sleep = mean(c(q55, q56, q57, q58), na.rm = TRUE),
         hygiene = mean(c(q59, q60, q61, q62, q63, q64, q65, q66, q67), na.rm = TRUE),
         mealprep = mean(c(q68, q69, q70), na.rm = TRUE), 
         navigation = mean(c(q71, q72, q73, q74), na.rm = TRUE), 
         financial = mean(c(q75, q76, q77, q78, q79), na.rm = TRUE), 
         victimization = mean(c(q80, q81, q82, q83), na.rm = TRUE), 
         communication = mean(c(q84, q85, q86, q87, q88, q89, q90), na.rm = TRUE),
         relationships = mean(c(q91, q92, q93, q94, q95, q96, q97), na.rm = TRUE), 
         theory = mean(c(q98, q99, q100, q101), na.rm = TRUE),
         rules = mean(c(q102, q103), na.rm = TRUE),
         techskills = mean(c(q104, q105, q106, q107, q108, q109), na.rm = TRUE),
         behavior = mean(c(q110, q111, q112), na.rm = TRUE),
         jobskills = mean(c(q113, q114, q115, q116, q117, q118, q119, q120), na.rm = TRUE),
         search = mean(c(q121, q122, q123, q124), na.rm = TRUE),
         ALL_sub = mean(c(coursework, initiation, selfadvocacy, study, timemanage, coping, control, diet,
                          selfcare, risky, sleep, hygiene, mealprep, navigation, financial, victimization,
                          communication, relationships, theory, rules, techskills, behavior,
                          jobskills, search), na.rm = TRUE),
         ALL_sub_norisky = mean(c(coursework, initiation, selfadvocacy, study, timemanage, coping, control, diet,
                                  selfcare, sleep, hygiene, mealprep, navigation, financial, victimization,
                                  communication, relationships, theory, rules, techskills, behavior,
                                  jobskills, search), na.rm = TRUE),
         ACA_sub = mean(c(coursework, initiation, selfadvocacy, study, timemanage), na.rm = TRUE),
         EMO_sub = mean(c(coping, control), na.rm = TRUE),
         HEALTH_sub = mean(c(diet, selfcare, risky, sleep), na.rm = TRUE),
         HEALTH_sub_norisky = mean(c(diet, selfcare, sleep), na.rm = TRUE),
         DAILY_sub = mean(c(hygiene, mealprep, navigation, financial), na.rm = TRUE),
         INTER_sub = mean(c(victimization, communication, relationships, theory, rules), na.rm = TRUE),
         TECH_sub = mean(c(techskills, behavior), na.rm = TRUE),
         VOC_sub = mean(c(jobskills, search), na.rm = TRUE),
         ACA_TECH_VOC_sub = mean(c(coursework, initiation, selfadvocacy, study, timemanage, techskills, behavior, jobskills, search), na.rm = TRUE),
         EMO_INTER_sub = mean(c(coping, control, victimization, communication, relationships, theory, rules), na.rm = TRUE),
         HEALTH_DAILY_sub = mean(c(diet, selfcare, risky, sleep, hygiene, mealprep, navigation, financial), na.rm = TRUE))

# PII-Full Time 1

pii_tim1 <- pii_full_demo %>% 
  filter(time == 1)

pii_tim1_ir <- pii_tim1 %>% 
  filter(version == "IR") %>% 
  write_sav("pii_tim1_ir.sav")

pii_tim1_sr <- pii_tim1 %>% 
  filter(version == "SR") %>% 
  write_sav("pii_tim1_sr.sav")

# m1 for seven factor model

m1 <- ' academic =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10 + q11 + q12 + q13 + q14 + q15 + q16 + q17 + q18 + q19 + q20 + q21 + q22 + q23 + q24 + q25 + q26 + q27 + q28 + q29 + q30  
emotionalreg =~ q31 + q32 + q33 + q34 + q35 + q36 + q37 + q38 + q39 + q40 + q41
healthwellness =~ q42 + q43 + q44 + q45 + q46 + q47 + q48 + q49 + q50 + q51 + q52 + q53 + q54 + q55 + q56 + q57 + q58
dailyliving =~ q59 + q60 + q61 + q62 + q63 + q64 + q65 + q66 + q67 + q68 + q69 + q70 + q71 + q72 + q73 + q74 + q75 + q76 + q77 + q78 + q79
interpersonal =~ q80 + q81 + q82 + q83 + q84 + q85 + q86 + q87 + q88 + q89 + q90 + q91 + q92 + q93 + q94 + q95 + q96 + q97 + q98 + q99 + q100 + q101 + q102 + q103
technology =~ q104 + q105 + q106 + q107 + q108 + q109 + q110 + q111 + q112
vocational =~ q113 + q114 + q115 + q116 + q117 + q118 + q119 + q120 + q121 + q122 + q123 + q124 '

# m2 for three factor model

m2 <- ' aca_tech_voc =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10 + q11 + q12 + q13 + q14 + q15 + q16 + q17 + q18 + q19 + q20 + q21 + q22 + q23 + q24 + q25 + q26 + q27 + q28 + q29 + q30 + q104 + q105 + q106 + q107 + q108 + q109 + q110 + q111 + q112 + q113 + q114 + q115 + q116 + q117 + q118 + q119 + q120 + q121 + q122 + q123 + q124
emotional_inter =~ q31 + q32 + q33 + q34 + q35 + q36 + q37 + q38 + q39 + q40 + q41 + q80 + q81 + q82 + q83 + q84 + q85 + q86 + q87 + q88 + q89 + q90 + q91 + q92 + q93 + q94 + q95 + q96 + q97 + q98 + q99 + q100 + q101 + q102 + q103
health_daily =~ q42 + q43 + q44 + q45 + q46 + q47 + q48 + q49 + q50 + q51 + q52 + q53 + q54 + q55 + q56 + q57 + q58 + q59 + q60 + q61 + q62 + q63 + q64 + q65 + q66 + q67 + q68 + q69 + q70 + q71 + q72 + q73 + q74 + q75 + q76 + q77 + q78 + q79 '

# subscale model with seven factors

sub1 <- ' academic =~ coursework + initiation + selfadvocacy + study + timemanage
emotional =~ coping + control
health =~ diet + selfcare + risky + sleep
daily =~ hygiene + mealprep + navigation + financial
interpersonal =~ victimization + communication + relationships + theory + rules
technology =~ techskills + behavior
employment =~ jobskills + search '

# subscale model with three factors

sub2 <- ' aca_tech_voc =~ coursework + initiation + selfadvocacy + study + timemanage + techskills + behavior + jobskills + search
emotional_inter =~ coping + control + victimization + communication + relationships + theory + rules 
health_daily =~ diet + selfcare + risky + sleep + hygiene + mealprep + navigation + financial '

# subscale model with seven factors (without risky)

sub3 <- ' employment =~ search + jobskills
technology =~ behavior + techskills
interpersonal =~ rules + theory + relationships + communication + victimization
daily =~ financial + navigation + mealprep + hygiene
health =~ sleep + selfcare + diet
emotional =~ control + coping
academic =~ timemanage + study + selfadvocacy + initiation + coursework '

# subscale model with three factors (without risky)

sub4 <- ' aca_tech_voc =~ coursework + initiation + selfadvocacy + study + timemanage + techskills + behavior + jobskills + search
emotional_inter =~ coping + control + victimization + communication + relationships + theory + rules 
health_daily =~ diet + selfcare + sleep + hygiene + mealprep + navigation + financial '

# models with all time-1 data from IR and SR

fit_m1 <- cfa(m1, data = pii_tim1, missing = "fiml", std.lv = TRUE)
summary(fit_m1, fit.measures = TRUE, standardized = TRUE)
measurementInvariance(fit_m1, data = pii_tim1, group = "version")
lavInspect(fit_m1, "cor.lv")

fit_m2 <- cfa(m2, data = pii_tim1, missing = "fiml", std.lv = TRUE)
summary(fit_m2, fit.measures = TRUE, standardized = TRUE)
measurementInvariance(fit_m2, data = pii_tim1, group = "version")
lavInspect(fit_m1, "cor.lv")

# subscale model fit

fit_sub1 <- cfa(sub1, data = pii_tim1, missing = "fiml", std.lv = TRUE) 
summary(fit_sub1, fit.measures = TRUE, standardized = TRUE) 
measurementInvariance(model = sub1, data = pii_tim1, group = "version", std.lv = TRUE)
lavInspect(fit_sub1, "cor.lv")

fit_sub2 <- cfa(sub2, data = pii_tim1, missing = "fiml", std.lv = TRUE)
summary(fit_sub2, fit.measures = TRUE, standardized = TRUE)
measurementInvariance(model = sub2, data = pii_tim1, group = "version", std.lv = TRUE)
lavInspect(fit_sub2, "cor.lv")

# subscale model fit (drop risky)

fit_sub3 <- cfa(sub3, data = pii_tim1, missing = "fiml", std.lv = TRUE) 
summary(fit_sub3, fit.measures = TRUE, standardized = TRUE) 
modindices(fit_sub3)
measurementInvariance(model = sub3, data = pii_tim1, group = "version", std.lv = TRUE)
lavInspect(fit_sub3, "cor.lv")

pathdiagram <- semPaths(fit_sub3,
                        what = "path",
                        whatLabels = "par",
                        style = "lisrel",
                        rotation = 2,
                        intercepts = FALSE,
                        curvature = 5,
                        cardinal = TRUE,
                        nCharNodes = 6,
                        sizeMan = 12,
                        sizeMan2 = 2,
                        sizeLat = 11,
                        sizeLat2 = 4,
                        nDigits = 3,
                        asize = 1.25,
                        residScale = 10,
                        edge.color = "black")

fit_sub4 <- cfa(sub4, data = pii_tim1, missing = "fiml", std.lv = TRUE)
summary(fit_sub4, fit.measures = TRUE, standardized = TRUE)
measurementInvariance(model = sub4, data = pii_tim1, group = "version", std.lv = TRUE)
lavInspect(fit_sub4, "cor.lv")

# descriptive statistics

options(max.print=999999)

pii_tim1 <- describe(pii_tim1, na.rm = TRUE, interp=FALSE, skew = TRUE, ranges = TRUE, trim = .1,
                     type = 3, check = TRUE, fast = NULL, quant = NULL, IQR = FALSE, omit = FALSE) %>% 
  write.csv('pii_tim1.csv')

pii_tim1_ir <- describe(pii_tim1_ir, na.rm = TRUE, interp=FALSE, skew = TRUE, ranges = TRUE, trim = .1,
                        type = 3, check = TRUE, fast = NULL, quant = NULL, IQR = FALSE, omit = FALSE) %>% 
  write.csv('pii_tim1_ir.csv')

pii_tim1_sr <- describe(pii_tim1_sr, na.rm = TRUE, interp=FALSE, skew = TRUE, ranges = TRUE, trim = .1,
                        type = 3, check = TRUE, fast = NULL, quant = NULL, IQR = FALSE, omit = FALSE) %>% 
  write.csv('pii_tim1_sr.csv')

# internal consistency of 7 scales using subscales (sub1)

FULL_sub_norisky <- pii_tim1 %>%
  select(coursework, initiation, selfadvocacy, study, timemanage, coping, control, diet, selfcare, sleep, hygiene, mealprep, navigation, financial, victimization, communication, relationships, theory, rules, techskills, behavior, jobskills, search)

alpha(FULL_sub_norisky, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

ACA_sub <- pii_tim1 %>%
  select(coursework, initiation, selfadvocacy, study, timemanage)

alpha(ACA_sub, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

EMO_sub <- pii_tim1 %>%
  select(coping, control)

alpha(EMO_sub, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

HEALTH_sub_norisky <- pii_tim1 %>%
  select(diet, selfcare, sleep)

alpha(HEALTH_sub_norisky, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

DAILY_sub <- pii_tim1 %>%
  select(hygiene, mealprep, navigation, financial)

alpha(DAILY_sub, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

INTER_sub <- pii_tim1 %>%
  select(victimization, communication, relationships, theory, rules)

alpha(INTER_sub, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

TECH_sub <- pii_tim1 %>%
  select(techskills, behavior)

alpha(TECH_sub, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

VOC_sub <- pii_tim1 %>%
  select(jobskills, search)

alpha(VOC_sub, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

# internal consistency subscales (both versions)

coursework_both <- pii_tim1 %>%
  select(q1:q4)

alpha(coursework_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

initiation_both <- pii_tim1 %>%
  select(q5:q7)

alpha(initiation_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

selfadvocacy_both <- pii_tim1 %>%
  select(q8:q10)

alpha(selfadvocacy_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

study_both <- pii_tim1 %>%
  select(q11:q22)

alpha(study_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

timemanage_both <- pii_tim1 %>%
  select(q23:q30)

alpha(timemanage_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL) 

coping_both <- pii_tim1 %>%
  select(q31:q38)

alpha(coping_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

emotional_both <- pii_tim1 %>%
  select(q39:q41)

alpha(emotional_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

diet_both <- pii_tim1 %>%
  select(q42:q44)

alpha(diet_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

selfcare_both <- pii_tim1 %>%
  select(q45:q50)

alpha(selfcare_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

risky_both <- pii_tim1 %>%
  select(q51:q54)

alpha(risky_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

sleep_both <- pii_tim1 %>%
  select(q55:q58)

alpha(sleep_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

hygiene_both <- pii_tim1 %>%
  select(q59:q67)

alpha(hygiene_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

mealprep_both <- pii_tim1 %>%
  select(q68:q70)

alpha(mealprep_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

navigation_both <- pii_tim1 %>%
  select(q71:q74)

alpha(navigation_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

financial_both <- pii_tim1 %>%
  select(q75:q79)

alpha(financial_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

avoidvic_both <- pii_tim1 %>%
  select(q80:q83)

alpha(avoidvic_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

commskills_both <- pii_tim1 %>%
  select(q84:q90)

alpha(commskills_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

relationships_both <- pii_tim1 %>%
  select(q91:q97)

alpha(relationships_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

theoryofmind_both <- pii_tim1 %>%
  select(q98:q101)

alpha(theoryofmind_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

socialrules_both <- pii_tim1 %>%
  select(q102:q103)

alpha(socialrules_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

techskills_both <- pii_tim1 %>%
  select(q104:q109)

alpha(techskills_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

techbehav_both <- pii_tim1 %>%
  select(q110:q112)

alpha(techbehav_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

onjob_both <- pii_tim1 %>%
  select(q113:q120)

alpha(onjob_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

jobskills_both <- pii_tim1 %>%
  select(q121:q124)

alpha(jobskills_both, keys = NULL, cumulative = FALSE, title = NULL, max = 10, na.rm = TRUE,
      check.keys = TRUE, n.iter = 1, delete = TRUE, use = "pairwise", warnings = TRUE, n.obs = NULL)

# MANOVA seven-factor by Version (Full, sub-scales)

pii_dataonly_full <- pii_tim1 %>%
  select(ACA_sub, EMO_sub, HEALTH_sub_norisky, DAILY_sub, INTER_sub, TECH_sub, VOC_sub)

pii_dataonly_ir <- pii_tim1_ir %>%
  select(ACA_sub, EMO_sub, HEALTH_sub_norisky, DAILY_sub, INTER_sub, TECH_sub, VOC_sub)

pii_dataonly_sr <- pii_tim1_sr %>%
  select(ACA_sub, EMO_sub, HEALTH_sub_norisky, DAILY_sub, INTER_sub, TECH_sub, VOC_sub)

manova_7_sub_version <- manova(cbind(ACA_sub, EMO_sub, HEALTH_sub_norisky, DAILY_sub, INTER_sub, TECH_sub, VOC_sub) ~ version, data = pii_tim1)
summary(manova_7_sub_version, fit.measures = TRUE, standardized = TRUE, test = "Wilks", intercept = TRUE)
summary.aov(manova_7_sub_version, intercept = TRUE, error = TRUE)
EtaSq(manova_7_sub_version)

# MANOVA seven-factor (IR, sub-scales)

manova_7_sub_ir <- manova(cbind(ACA_sub, EMO_sub, HEALTH_sub_norisky, DAILY_sub, INTER_sub, TECH_sub, VOC_sub) ~ gender*asd, data = pii_tim1_ir)
summary(manova_7_sub_ir, fit.measures = TRUE, standardized = TRUE, test = "Wilks", intercept = TRUE)
summary.aov(manova_7_sub_ir, intercept = TRUE, error = TRUE)
EtaSq(manova_7_sub_ir)

# MANOVA seven-factor (SR, sub-scales)

manova_7_sub_sr <- manova(cbind(ACA_sub, EMO_sub, HEALTH_sub_norisky, DAILY_sub, INTER_sub, TECH_sub, VOC_sub) ~ gender*asd, data = pii_tim1_sr)
summary(manova_7_sub_sr, fit.measures = TRUE, standardized = TRUE, test = "Wilks", intercept = TRUE)
summary.aov(manova_7_sub_sr, intercept = TRUE, error = TRUE)
EtaSq(manova_7_sub_sr)

# DFA (version)

lda_version <- lda(version ~ ACA_sub + EMO_sub + HEALTH_sub_norisky + DAILY_sub + INTER_sub + TECH_sub + VOC_sub, data = pii_tim1)

# DFA (gender, IR)

lda_gender_ir <- lda(gender ~ ACA_sub + EMO_sub + HEALTH_sub_norisky + DAILY_sub + INTER_sub + TECH_sub + VOC_sub, data = pii_tim1_ir)

# DFA (gender, SR)

lda_gender_sr <- lda(gender ~ ACA_sub + EMO_sub + HEALTH_sub_norisky + DAILY_sub + INTER_sub + TECH_sub + VOC_sub, data = pii_tim1_sr)

# DFA (disability status, IR)

dfa_asd_ir <- lda(asd ~ ACA_sub + EMO_sub + HEALTH_sub_norisky + DAILY_sub + INTER_sub + TECH_sub + VOC_sub, data = pii_tim1_ir)

# DFA (disability status, SR)

dfa_asd_sr <- lda(asd ~ ACA_sub + EMO_sub + HEALTH_sub_norisky + DAILY_sub + INTER_sub + TECH_sub + VOC_sub, data = pii_tim1_sr)

# concurrent validity correlations (BRIEF)

brief_pii_ir <- inner_join(briefir, pii_tim1_ir, by = "remote_client_id") %>% 
  select(remote_client_id, gec_mean, gec_raw, version, version_b, ALL_sub_norisky, ACA_sub, EMO_sub, HEALTH_sub_norisky, DAILY_sub:VOC_sub)

data_only_ir <- brief_pii_ir %>% 
  select(-remote_client_id, -version, -version_b)

brief_pii_sr <- inner_join(briefsr, pii_tim1_sr, by = "remote_client_id") %>% 
  select(remote_client_id, gec_mean, gec_raw, version, version_b, ALL_sub_norisky, ACA_sub, EMO_sub, HEALTH_sub_norisky, DAILY_sub:VOC_sub)

data_only_sr <- brief_pii_sr %>% 
  select(-remote_client_id, -version, -version_b)

brief_pii_full <- bind_rows(brief_pii_ir, brief_pii_sr)

data_only <- brief_pii_full %>% 
  select(-remote_client_id, -version, -version_b)

correlate(data_only)

# concurrent validity correlations (OQ-45)

oq45_pii_sr <- inner_join(oq45, pii_tim1_sr, by = "remote_client_id") %>% 
  select(remote_client_id, subscale_r2d2_sd:subscale_r2d2_score, ALL_sub_norisky, ACA_sub, EMO_sub, HEALTH_sub_norisky, DAILY_sub:VOC_sub)

data_only_oqpii <- oq45_pii_sr %>% 
  select(-remote_client_id)

correlate(data_only_oqpii)

# interrater reliability

pii_tim1_wide <- pii_tim1 %>%
  pivot_wider(names_from = version,
              values_from = q1:HEALTH_DAILY_sub)

q1 <- pii_tim1_wide %>% 
  select(q1_IR, q1_SR)

q2 <- pii_tim1_wide %>% 
  select(q2_IR, q2_SR)

q3 <- pii_tim1_wide %>% 
  select(q3_IR, q3_SR)

q4 <- pii_tim1_wide %>% 
  select(q4_IR, q4_SR)

q5 <- pii_tim1_wide %>% 
  select(q5_IR, q5_SR)

q6 <- pii_tim1_wide %>% 
  select(q6_IR, q6_SR)

q7 <- pii_tim1_wide %>% 
  select(q7_IR, q7_SR)

q8 <- pii_tim1_wide %>% 
  select(q8_IR, q8_SR)

q9 <- pii_tim1_wide %>% 
  select(q9_IR, q9_SR)

q10 <- pii_tim1_wide %>% 
  select(q10_IR, q10_SR)

q11 <- pii_tim1_wide %>% 
  select(q11_IR, q11_SR)

q12 <- pii_tim1_wide %>% 
  select(q12_IR, q12_SR)

q13 <- pii_tim1_wide %>% 
  select(q13_IR, q13_SR)

q14 <- pii_tim1_wide %>% 
  select(q14_IR, q14_SR)

q15 <- pii_tim1_wide %>% 
  select(q15_IR, q15_SR)

q16 <- pii_tim1_wide %>% 
  select(q16_IR, q16_SR)

q17 <- pii_tim1_wide %>% 
  select(q17_IR, q17_SR)

q18 <- pii_tim1_wide %>% 
  select(q18_IR, q18_SR)

q19 <- pii_tim1_wide %>% 
  select(q19_IR, q19_SR)

q20 <- pii_tim1_wide %>% 
  select(q20_IR, q20_SR)

q21 <- pii_tim1_wide %>% 
  select(q21_IR, q21_SR)

q22 <- pii_tim1_wide %>% 
  select(q22_IR, q22_SR)

q23 <- pii_tim1_wide %>% 
  select(q23_IR, q23_SR)

q24 <- pii_tim1_wide %>% 
  select(q24_IR, q24_SR)

q25 <- pii_tim1_wide %>% 
  select(q25_IR, q25_SR)

q26 <- pii_tim1_wide %>% 
  select(q26_IR, q26_SR)

q27 <- pii_tim1_wide %>% 
  select(q27_IR, q27_SR)

q28 <- pii_tim1_wide %>% 
  select(q28_IR, q28_SR)

q29 <- pii_tim1_wide %>% 
  select(q29_IR, q29_SR)

q30 <- pii_tim1_wide %>% 
  select(q30_IR, q30_SR)

q31 <- pii_tim1_wide %>% 
  select(q31_IR, q31_SR)

q32 <- pii_tim1_wide %>% 
  select(q32_IR, q32_SR)

q33 <- pii_tim1_wide %>% 
  select(q33_IR, q33_SR)

q34 <- pii_tim1_wide %>% 
  select(q34_IR, q34_SR)

q35 <- pii_tim1_wide %>% 
  select(q35_IR, q35_SR)

q36 <- pii_tim1_wide %>% 
  select(q36_IR, q36_SR)

q37 <- pii_tim1_wide %>% 
  select(q37_IR, q37_SR)

q38 <- pii_tim1_wide %>% 
  select(q38_IR, q38_SR)

q39 <- pii_tim1_wide %>% 
  select(q39_IR, q39_SR)

q40 <- pii_tim1_wide %>% 
  select(q40_IR, q40_SR)

q41 <- pii_tim1_wide %>% 
  select(q41_IR, q41_SR)

q42 <- pii_tim1_wide %>% 
  select(q42_IR, q42_SR)

q43 <- pii_tim1_wide %>% 
  select(q43_IR, q43_SR)

q44 <- pii_tim1_wide %>% 
  select(q44_IR, q44_SR)

q45 <- pii_tim1_wide %>% 
  select(q45_IR, q45_SR)

q46 <- pii_tim1_wide %>% 
  select(q46_IR, q46_SR)

q47 <- pii_tim1_wide %>% 
  select(q47_IR, q47_SR)

q48 <- pii_tim1_wide %>% 
  select(q48_IR, q48_SR)

q49 <- pii_tim1_wide %>% 
  select(q49_IR, q49_SR)

q50 <- pii_tim1_wide %>% 
  select(q50_IR, q50_SR)

q51 <- pii_tim1_wide %>% 
  select(q51_IR, q51_SR)

q52 <- pii_tim1_wide %>% 
  select(q52_IR, q52_SR)

q53 <- pii_tim1_wide %>% 
  select(q53_IR, q53_SR)

q54 <- pii_tim1_wide %>% 
  select(q54_IR, q54_SR)

q55 <- pii_tim1_wide %>% 
  select(q55_IR, q55_SR)

q56 <- pii_tim1_wide %>% 
  select(q56_IR, q56_SR)

q57 <- pii_tim1_wide %>% 
  select(q57_IR, q57_SR)

q58 <- pii_tim1_wide %>% 
  select(q58_IR, q58_SR)

q59 <- pii_tim1_wide %>% 
  select(q59_IR, q59_SR)

q60 <- pii_tim1_wide %>% 
  select(q60_IR, q60_SR)

q61 <- pii_tim1_wide %>% 
  select(q61_IR, q61_SR)

q62 <- pii_tim1_wide %>% 
  select(q62_IR, q62_SR)

q63 <- pii_tim1_wide %>% 
  select(q63_IR, q63_SR)

q64 <- pii_tim1_wide %>% 
  select(q64_IR, q64_SR)

q65 <- pii_tim1_wide %>% 
  select(q65_IR, q65_SR)

q66 <- pii_tim1_wide %>% 
  select(q66_IR, q66_SR)

q67 <- pii_tim1_wide %>% 
  select(q67_IR, q67_SR)

q68 <- pii_tim1_wide %>% 
  select(q68_IR, q68_SR)

q69 <- pii_tim1_wide %>% 
  select(q69_IR, q69_SR)

q70 <- pii_tim1_wide %>% 
  select(q70_IR, q70_SR)

q71 <- pii_tim1_wide %>% 
  select(q71_IR, q71_SR)

q72 <- pii_tim1_wide %>% 
  select(q72_IR, q72_SR)

q73 <- pii_tim1_wide %>% 
  select(q73_IR, q73_SR)

q74 <- pii_tim1_wide %>% 
  select(q74_IR, q74_SR)

q75 <- pii_tim1_wide %>% 
  select(q75_IR, q75_SR)

q76 <- pii_tim1_wide %>% 
  select(q76_IR, q76_SR)

q77 <- pii_tim1_wide %>% 
  select(q77_IR, q77_SR)

q78 <- pii_tim1_wide %>% 
  select(q78_IR, q78_SR)

q79 <- pii_tim1_wide %>% 
  select(q79_IR, q79_SR)

q80 <- pii_tim1_wide %>% 
  select(q80_IR, q80_SR)

q81 <- pii_tim1_wide %>% 
  select(q81_IR, q81_SR)

q82 <- pii_tim1_wide %>% 
  select(q82_IR, q82_SR)

q83 <- pii_tim1_wide %>% 
  select(q83_IR, q83_SR)

q84 <- pii_tim1_wide %>% 
  select(q84_IR, q84_SR)

q85 <- pii_tim1_wide %>% 
  select(q85_IR, q85_SR)

q86 <- pii_tim1_wide %>% 
  select(q86_IR, q86_SR)

q87 <- pii_tim1_wide %>% 
  select(q87_IR, q87_SR)

q88 <- pii_tim1_wide %>% 
  select(q88_IR, q88_SR)

q89 <- pii_tim1_wide %>% 
  select(q89_IR, q89_SR)

q90 <- pii_tim1_wide %>% 
  select(q90_IR, q90_SR)

q91 <- pii_tim1_wide %>% 
  select(q91_IR, q91_SR)

q92 <- pii_tim1_wide %>% 
  select(q92_IR, q92_SR)

q93 <- pii_tim1_wide %>% 
  select(q93_IR, q93_SR)

q94 <- pii_tim1_wide %>% 
  select(q94_IR, q94_SR)

q95 <- pii_tim1_wide %>% 
  select(q95_IR, q95_SR)

q96 <- pii_tim1_wide %>% 
  select(q96_IR, q96_SR)

q97 <- pii_tim1_wide %>% 
  select(q97_IR, q97_SR)

q98 <- pii_tim1_wide %>% 
  select(q98_IR, q98_SR)

q99 <- pii_tim1_wide %>% 
  select(q99_IR, q99_SR)

q100 <- pii_tim1_wide %>% 
  select(q100_IR, q100_SR)

q101 <- pii_tim1_wide %>% 
  select(q101_IR, q101_SR)

q102 <- pii_tim1_wide %>% 
  select(q102_IR, q102_SR)

q103 <- pii_tim1_wide %>% 
  select(q103_IR, q103_SR)

q104 <- pii_tim1_wide %>% 
  select(q104_IR, q104_SR)

q105 <- pii_tim1_wide %>% 
  select(q105_IR, q105_SR)

q106 <- pii_tim1_wide %>% 
  select(q106_IR, q106_SR)

q107 <- pii_tim1_wide %>% 
  select(q107_IR, q107_SR)

q108 <- pii_tim1_wide %>% 
  select(q108_IR, q108_SR)

q109 <- pii_tim1_wide %>% 
  select(q109_IR, q109_SR)

q110 <- pii_tim1_wide %>% 
  select(q110_IR, q110_SR)

q111 <- pii_tim1_wide %>% 
  select(q111_IR, q111_SR)

q112 <- pii_tim1_wide %>% 
  select(q112_IR, q112_SR)

q113 <- pii_tim1_wide %>% 
  select(q113_IR, q113_SR)

q114 <- pii_tim1_wide %>% 
  select(q114_IR, q114_SR)

q115 <- pii_tim1_wide %>% 
  select(q115_IR, q115_SR)

q116 <- pii_tim1_wide %>% 
  select(q116_IR, q116_SR)

q117 <- pii_tim1_wide %>% 
  select(q117_IR, q117_SR)

q118 <- pii_tim1_wide %>% 
  select(q118_IR, q118_SR)

q119 <- pii_tim1_wide %>% 
  select(q119_IR, q119_SR)

q120 <- pii_tim1_wide %>% 
  select(q120_IR, q120_SR)

q121 <- pii_tim1_wide %>% 
  select(q121_IR, q121_SR)

q122 <- pii_tim1_wide %>% 
  select(q122_IR, q122_SR)

q123 <- pii_tim1_wide %>% 
  select(q123_IR, q123_SR)

q124 <- pii_tim1_wide %>% 
  select(q124_IR, q124_SR)

ALL <- pii_tim1_wide %>% 
  select(ALL_sub_norisky_IR, ALL_sub_norisky_SR)

ACA <- pii_tim1_wide %>% 
  select(ACA_sub_IR, ACA_sub_SR)

EMO <- pii_tim1_wide %>% 
  select(EMO_sub_IR, EMO_sub_SR)

HEALTH <- pii_tim1_wide %>% 
  select(HEALTH_sub_norisky_IR, HEALTH_sub_norisky_SR)

DAILY <- pii_tim1_wide %>% 
  select(DAILY_sub_IR, DAILY_sub_SR)

INTER <- pii_tim1_wide %>% 
  select(INTER_sub_IR, INTER_sub_SR)

TECH <- pii_tim1_wide %>% 
  select(TECH_sub_IR, TECH_sub_SR)

VOC <- pii_tim1_wide %>% 
  select(VOC_sub_IR, VOC_sub_SR)

coursework <- pii_tim1_wide %>% 
  select(coursework_IR, coursework_SR)

initiation <- pii_tim1_wide %>% 
  select(initiation_IR, initiation_SR)

selfadvocacy <- pii_tim1_wide %>% 
  select(selfadvocacy_IR, selfadvocacy_SR)

study <- pii_tim1_wide %>% 
  select(study_IR, study_SR)

timemanage <- pii_tim1_wide %>% 
  select(timemanage_IR, timemanage_SR)

coping <- pii_tim1_wide %>% 
  select(coping_IR, coping_SR)

emotional <- pii_tim1_wide %>% 
  select(emotional_IR, emotional_SR)

diet <- pii_tim1_wide %>% 
  select(diet_IR, diet_SR)

selfcare <- pii_tim1_wide %>% 
  select(selfcare_IR, selfcare_SR)

risky <- pii_tim1_wide %>% 
  select(risky_IR, risky_SR)

sleep <- pii_tim1_wide %>% 
  select(sleep_IR, sleep_SR)

hygiene <- pii_tim1_wide %>% 
  select(hygiene_IR, hygiene_SR)

mealprep <- pii_tim1_wide %>% 
  select(mealprep_IR, mealprep_SR)

navigation <- pii_tim1_wide %>% 
  select(navigation_IR, navigation_SR)

financial <- pii_tim1_wide %>% 
  select(financial_IR, financial_SR)

avoidvic <- pii_tim1_wide %>% 
  select(avoidvic_IR, avoidvic_SR)

commskills <- pii_tim1_wide %>% 
  select(commskills_IR, commskills_SR)

relationships <- pii_tim1_wide %>% 
  select(relationships_IR, relationships_SR)

theoryofmind <- pii_tim1_wide %>% 
  select(theoryofmind_IR, theoryofmind_SR)

socialrules <- pii_tim1_wide %>% 
  select(socialrules_IR, socialrules_SR)

techskills <- pii_tim1_wide %>% 
  select(techskills_IR, techskills_SR)

techbehav <- pii_tim1_wide %>% 
  select(techbehav_IR, techbehav_SR)

onjob <- pii_tim1_wide %>% 
  select(onjob_IR, onjob_SR)

jobsearch <- pii_tim1_wide %>% 
  select(jobsearch_IR, jobsearch_SR)

?icc(q1, model = "twoway", type = "agreement", unit = "single")
icc(q2, model = "twoway", type = "agreement", unit = "single")
icc(q3, model = "twoway", type = "agreement", unit = "single")
icc(q4, model = "twoway", type = "agreement", unit = "single")
icc(q5, model = "twoway", type = "agreement", unit = "single")
icc(q6, model = "twoway", type = "agreement", unit = "single")
icc(q7, model = "twoway", type = "agreement", unit = "single")
icc(q8, model = "twoway", type = "agreement", unit = "single")
icc(q9, model = "twoway", type = "agreement", unit = "single")
icc(q10, model = "twoway", type = "agreement", unit = "single")
icc(q11, model = "twoway", type = "agreement", unit = "single")
icc(q12, model = "twoway", type = "agreement", unit = "single")
icc(q13, model = "twoway", type = "agreement", unit = "single")
icc(q14, model = "twoway", type = "agreement", unit = "single")
icc(q15, model = "twoway", type = "agreement", unit = "single")
icc(q16, model = "twoway", type = "agreement", unit = "single")
icc(q17, model = "twoway", type = "agreement", unit = "single")
icc(q18, model = "twoway", type = "agreement", unit = "single")
icc(q19, model = "twoway", type = "agreement", unit = "single")
icc(q20, model = "twoway", type = "agreement", unit = "single")
icc(q21, model = "twoway", type = "agreement", unit = "single")
icc(q22, model = "twoway", type = "agreement", unit = "single")
icc(q23, model = "twoway", type = "agreement", unit = "single")
icc(q24, model = "twoway", type = "agreement", unit = "single")
icc(q25, model = "twoway", type = "agreement", unit = "single")
icc(q26, model = "twoway", type = "agreement", unit = "single")
icc(q27, model = "twoway", type = "agreement", unit = "single")
icc(q28, model = "twoway", type = "agreement", unit = "single")
icc(q29, model = "twoway", type = "agreement", unit = "single")
icc(q30, model = "twoway", type = "agreement", unit = "single")
icc(q31, model = "twoway", type = "agreement", unit = "single")
icc(q32, model = "twoway", type = "agreement", unit = "single")
icc(q33, model = "twoway", type = "agreement", unit = "single")
icc(q34, model = "twoway", type = "agreement", unit = "single")
icc(q35, model = "twoway", type = "agreement", unit = "single")
icc(q36, model = "twoway", type = "agreement", unit = "single")
icc(q37, model = "twoway", type = "agreement", unit = "single")
icc(q38, model = "twoway", type = "agreement", unit = "single")
icc(q39, model = "twoway", type = "agreement", unit = "single")
icc(q40, model = "twoway", type = "agreement", unit = "single")
icc(q41, model = "twoway", type = "agreement", unit = "single")
icc(q42, model = "twoway", type = "agreement", unit = "single")
icc(q43, model = "twoway", type = "agreement", unit = "single")
icc(q44, model = "twoway", type = "agreement", unit = "single")
icc(q45, model = "twoway", type = "agreement", unit = "single")
icc(q46, model = "twoway", type = "agreement", unit = "single")
icc(q47, model = "twoway", type = "agreement", unit = "single")
icc(q48, model = "twoway", type = "agreement", unit = "single")
icc(q49, model = "twoway", type = "agreement", unit = "single")
icc(q50, model = "twoway", type = "agreement", unit = "single")
icc(q51, model = "twoway", type = "agreement", unit = "single")
icc(q52, model = "twoway", type = "agreement", unit = "single")
icc(q53, model = "twoway", type = "agreement", unit = "single")
icc(q54, model = "twoway", type = "agreement", unit = "single")
icc(q55, model = "twoway", type = "agreement", unit = "single")
icc(q56, model = "twoway", type = "agreement", unit = "single")
icc(q57, model = "twoway", type = "agreement", unit = "single")
icc(q58, model = "twoway", type = "agreement", unit = "single")
icc(q59, model = "twoway", type = "agreement", unit = "single")
icc(q60, model = "twoway", type = "agreement", unit = "single")
icc(q61, model = "twoway", type = "agreement", unit = "single")
icc(q62, model = "twoway", type = "agreement", unit = "single")
icc(q63, model = "twoway", type = "agreement", unit = "single")
icc(q64, model = "twoway", type = "agreement", unit = "single")
icc(q65, model = "twoway", type = "agreement", unit = "single")
icc(q66, model = "twoway", type = "agreement", unit = "single")
icc(q67, model = "twoway", type = "agreement", unit = "single")
icc(q68, model = "twoway", type = "agreement", unit = "single")
icc(q69, model = "twoway", type = "agreement", unit = "single")
icc(q70, model = "twoway", type = "agreement", unit = "single")
icc(q71, model = "twoway", type = "agreement", unit = "single")
icc(q72, model = "twoway", type = "agreement", unit = "single")
icc(q73, model = "twoway", type = "agreement", unit = "single")
icc(q74, model = "twoway", type = "agreement", unit = "single")
icc(q75, model = "twoway", type = "agreement", unit = "single")
icc(q76, model = "twoway", type = "agreement", unit = "single")
icc(q77, model = "twoway", type = "agreement", unit = "single")
icc(q78, model = "twoway", type = "agreement", unit = "single")
icc(q79, model = "twoway", type = "agreement", unit = "single")
icc(q80, model = "twoway", type = "agreement", unit = "single")
icc(q81, model = "twoway", type = "agreement", unit = "single")
icc(q82, model = "twoway", type = "agreement", unit = "single")
icc(q83, model = "twoway", type = "agreement", unit = "single")
icc(q84, model = "twoway", type = "agreement", unit = "single")
icc(q85, model = "twoway", type = "agreement", unit = "single")
icc(q86, model = "twoway", type = "agreement", unit = "single")
icc(q87, model = "twoway", type = "agreement", unit = "single")
icc(q88, model = "twoway", type = "agreement", unit = "single")
icc(q89, model = "twoway", type = "agreement", unit = "single")
icc(q90, model = "twoway", type = "agreement", unit = "single")
icc(q91, model = "twoway", type = "agreement", unit = "single")
icc(q92, model = "twoway", type = "agreement", unit = "single")
icc(q93, model = "twoway", type = "agreement", unit = "single")
icc(q94, model = "twoway", type = "agreement", unit = "single")
icc(q95, model = "twoway", type = "agreement", unit = "single")
icc(q96, model = "twoway", type = "agreement", unit = "single")
icc(q97, model = "twoway", type = "agreement", unit = "single")
icc(q98, model = "twoway", type = "agreement", unit = "single")
icc(q99, model = "twoway", type = "agreement", unit = "single")
icc(q100, model = "twoway", type = "agreement", unit = "single")
icc(q101, model = "twoway", type = "agreement", unit = "single")
icc(q102, model = "twoway", type = "agreement", unit = "single")
icc(q103, model = "twoway", type = "agreement", unit = "single")
icc(q104, model = "twoway", type = "agreement", unit = "single")
icc(q105, model = "twoway", type = "agreement", unit = "single")
icc(q106, model = "twoway", type = "agreement", unit = "single")
icc(q107, model = "twoway", type = "agreement", unit = "single")
icc(q108, model = "twoway", type = "agreement", unit = "single")
icc(q109, model = "twoway", type = "agreement", unit = "single")
icc(q110, model = "twoway", type = "agreement", unit = "single")
icc(q111, model = "twoway", type = "agreement", unit = "single")
icc(q112, model = "twoway", type = "agreement", unit = "single")
icc(q113, model = "twoway", type = "agreement", unit = "single")
icc(q114, model = "twoway", type = "agreement", unit = "single")
icc(q115, model = "twoway", type = "agreement", unit = "single")
icc(q116, model = "twoway", type = "agreement", unit = "single")
icc(q117, model = "twoway", type = "agreement", unit = "single")
icc(q118, model = "twoway", type = "agreement", unit = "single")
icc(q119, model = "twoway", type = "agreement", unit = "single")
icc(q120, model = "twoway", type = "agreement", unit = "single")
icc(q121, model = "twoway", type = "agreement", unit = "single")
icc(q122, model = "twoway", type = "agreement", unit = "single")
icc(q123, model = "twoway", type = "agreement", unit = "single")
icc(q124, model = "twoway", type = "agreement", unit = "single")

icc(ALL, model = "twoway", type = "agreement", unit = "single")
icc(ACA, model = "twoway", type = "agreement", unit = "single")
icc(EMO, model = "twoway", type = "agreement", unit = "single")
icc(HEALTH, model = "twoway", type = "agreement", unit = "single")
icc(DAILY, model = "twoway", type = "agreement", unit = "single")
icc(INTER, model = "twoway", type = "agreement", unit = "single")
icc(TECH, model = "twoway", type = "agreement", unit = "single")
icc(VOC, model = "twoway", type = "agreement", unit = "single")

icc(coursework, model = "twoway", type = "agreement", unit = "single")
icc(initiation, model = "twoway", type = "agreement", unit = "single")
icc(selfadvocacy, model = "twoway", type = "agreement", unit = "single")
icc(study, model = "twoway", type = "agreement", unit = "single")
icc(timemanage, model = "twoway", type = "agreement", unit = "single")
icc(coping, model = "twoway", type = "agreement", unit = "single")
icc(emotional, model = "twoway", type = "agreement", unit = "single")
icc(diet, model = "twoway", type = "agreement", unit = "single")
icc(selfcare, model = "twoway", type = "agreement", unit = "single")
icc(risky, model = "twoway", type = "agreement", unit = "single")
icc(sleep, model = "twoway", type = "agreement", unit = "single")
icc(hygiene, model = "twoway", type = "agreement", unit = "single")
icc(mealprep, model = "twoway", type = "agreement", unit = "single")
icc(navigation, model = "twoway", type = "agreement", unit = "single")
icc(financial, model = "twoway", type = "agreement", unit = "single")
icc(avoidvic, model = "twoway", type = "agreement", unit = "single")
icc(commskills, model = "twoway", type = "agreement", unit = "single")
icc(relationships, model = "twoway", type = "agreement", unit = "single")
icc(theoryofmind, model = "twoway", type = "agreement", unit = "single")
icc(socialrules, model = "twoway", type = "agreement", unit = "single")
icc(techskills, model = "twoway", type = "agreement", unit = "single")
icc(techbehav, model = "twoway", type = "agreement", unit = "single")
icc(onjob, model = "twoway", type = "agreement", unit = "single")
icc(jobsearch, model = "twoway", type = "agreement", unit = "single")
