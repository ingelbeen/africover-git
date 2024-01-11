#############################################################################
# Population-based COVID-19 surveillance, Maputo, Moz, Dec2020-Mar2022      #
# Active acute respiratory illness surveillance                             #
# Script to analyse signs and symptoms associated with COVID-19 among       #
# possible cases (onset of at least one respiratory symptom, ageusia, or    #
# anosmia in the past 2 weeks)                                              #
#############################################################################

# install/load packages
pacman::p_load(lubridate, broom, dplyr)
#, ggmap, survival, flextable, janitor, knitr, httr, lmtest, scales, usethis, tidyverse, stringr, purrr, gtsummary, broom, lmtest, parameters, see)

# import possible case data
possiblecases <- read.csv("possiblecases_pseudo.csv")

# subset data to keep only those with results (112 no nasal swab collected, swab not tested, or no test result) and age recorded
possiblecases <- possiblecases %>% filter(!is.na(agegr10))
testedpossiblecases <- possiblecases %>% filter(testresult=="positive" | testresult=="negative")

#### 1. FREQUENCY OF TESTRESULTS BY AGE AND SEX ####
# frequencies confirmed
table(testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$testresult))*100,1)

# age confirmed vs negative - not possible with anonymized dataset
testedpossiblecases %>%
  filter(!is.na(age)) %>%
  group_by(testresult) %>%
  summarise(median=median(age),q25=quantile(age,0.25), q75=quantile(age,0.75))
# agegroup distribution of test results
table(testedpossiblecases$agegr10, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$agegr10, testedpossiblecases$testresult),2)*100,1) # missings not included now

# sex distribution of test results
table(testedpossiblecases$GENDER, testedpossiblecases$testresult,useNA = "always")
round(prop.table(table(testedpossiblecases$GENDER, testedpossiblecases$testresult),2)*100,1) # missings not included now

#### 2. FREQUENCIES OF CLINICAL SIGNS AND SYMPTOMS AMONG COVID-19 VERSUS SARS-CoV-2 NEGATIVE ####
# fever
table(testedpossiblecases$fever, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$fever, testedpossiblecases$testresult),2)*100,1) 
# throat pain
table(testedpossiblecases$throat, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$throat, testedpossiblecases$testresult),2)*100,1) 
# dyspnoea
table(testedpossiblecases$dyspnoea, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$dyspnoea, testedpossiblecases$testresult),2)*100,1) 
# anosmia
table(testedpossiblecases$anosmia, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$anosmia, testedpossiblecases$testresult),2)*100,1) 
# ageusia
table(testedpossiblecases$ageusia, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$ageusia, testedpossiblecases$testresult),2)*100,1) 
# fatigue
table(testedpossiblecases$fatigue, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$fatigue, testedpossiblecases$testresult),2)*100,1) 
# oxygen saturation below 92%
testedpossiblecases$O2under92[testedpossiblecases$oxygen_saturation<92] <- "yes"
testedpossiblecases$O2under92[testedpossiblecases$oxygen_saturation>91.9999999] <- "no"
table(testedpossiblecases$O2under92, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$O2under92, testedpossiblecases$testresult),2)*100,2) 
# oxygen saturation below 95%
testedpossiblecases$O2under95[testedpossiblecases$oxygen_saturation<95] <- "yes"
testedpossiblecases$O2under95[testedpossiblecases$oxygen_saturation>94.9999999] <- "no"
table(testedpossiblecases$O2under95, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$O2under95, testedpossiblecases$testresult),2)*100,1) 
# cough
table(testedpossiblecases$cough, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$cough, testedpossiblecases$testresult),2)*100,1) 
# rhinorrhea
table(testedpossiblecases$rhinorrhea, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$rhinorrhea, testedpossiblecases$testresult),2)*100,1) 
# chest_pain
table(testedpossiblecases$chest_pain, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$chest_pain, testedpossiblecases$testresult),2)*100,1) 
# nausea
table(testedpossiblecases$nausea, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$nausea, testedpossiblecases$testresult),2)*100,1) 
# headache
table(testedpossiblecases$headache, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$headache, testedpossiblecases$testresult),2)*100,1) 
# vomit
table(testedpossiblecases$vomit, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$vomit, testedpossiblecases$testresult),2)*100,1) 
# appitite_loss
table(testedpossiblecases$appitite_loss, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$appitite_loss, testedpossiblecases$testresult),2)*100,1) 
# diarrheoa
table(testedpossiblecases$diarrhea, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$diarrhea, testedpossiblecases$testresult),2)*100,1) 
# nosebleed
table(testedpossiblecases$nosebleed, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$nosebleed, testedpossiblecases$testresult),2)*100,1) 
# consciousness_change
table(testedpossiblecases$consciousness_change, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$consciousness_change, testedpossiblecases$testresult),2)*100,1) 
# convulsions
table(testedpossiblecases$convulsions, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$convulsions, testedpossiblecases$testresult),2)*100,1) 
# chills
table(testedpossiblecases$chills, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$chills, testedpossiblecases$testresult),2)*100,1) 
# myalgia
table(testedpossiblecases$myalgia, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$myalgia, testedpossiblecases$testresult),2)*100,1) 
# arthromyalgia
table(testedpossiblecases$arthromyalgia, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$arthromyalgia, testedpossiblecases$testresult),2)*100,1) 
# rash
table(testedpossiblecases$rash, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$rash, testedpossiblecases$testresult),2)*100,1) 

#### 3. MULTIVARIABLE ANALYSIS OF SYMPTOMS ASSOCIATED TO COVID-19 AMONG POSSIBLE CASES, CONTROLLING FOR AGE GROUP ####
## prepare data
# define variables of interest 
explanatory_vars <- c("fever", "throat", "rhinorrhea", "dyspnoea", "cough", "chest_pain", "vomit", "chills", "appitite_loss", "nausea", "diarrhea", "headache", "rash", "conjunctivitis", "myalgia", "arthromyalgia", "anosmia", "ageusia", "fatigue", "O2under95")
# convert dichotomous variables to 0/1 
testedpossiblecases <- testedpossiblecases %>%  
  mutate(across(                                      
    .cols = all_of(c(explanatory_vars, "testresult")),  ## for each column listed and "outcome"
    .fns = ~case_when(                              
      . %in% c("Sim", "yes", "positive")   ~ 1,           ## recode male, yes and death to 1
      . %in% c("Não", "Não sabe", "no",  "negative") ~ 0,           ## female, no and recover to 0
      TRUE                            ~ NA_real_)    ## otherwise set to missing
  )
  )

# add age group to the explanatory vars 
explanatory_vars <- c(explanatory_vars, "agegr10")

# drop observations with missing age group
testedpossiblecases <- testedpossiblecases %>%  filter(!is.na(agegr10)) 

# convert age group to factor and define which age group to make reference
testedpossiblecases$agegr10 <- factor(testedpossiblecases$agegr10)
testedpossiblecases <- testedpossiblecases %>% mutate(agegr10 = fct_relevel(agegr10, "0-9", after = 0)) 

# convert socio-economic level of household to factor and define which age group to make reference
testedpossiblecases$SesScoreQnt <- factor(testedpossiblecases$SesScoreQnt)
testedpossiblecases <- testedpossiblecases %>% mutate(SesScoreQnt = fct_relevel(SesScoreQnt, "3. med.", after = 0)) 

## logistic regression analyses - in the analyses in the manuscript, age as a continuous variable was used, while on the anonmymized data, only age grousp are available
# age groups
table(testedpossiblecases$agegr10, testedpossiblecases$testresult)
prop.table(table(testedpossiblecases$agegr10, testedpossiblecases$testresult),2)
glm(testresult ~ agegr10, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# sex
testedpossiblecases <- testedpossiblecases %>% 
  mutate(GENDER = fct_relevel(GENDER, "M", after = 0)) 
# univariable
glm(testresult ~ GENDER, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns
# multivariable (adjusting for age groups)
glm(testresult ~ GENDER + agegr10, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# socio-economic status quintile (should not be relevant here, but analyzed to check reporting biases)
table(testedpossiblecases$SesScoreQnt, testedpossiblecases$testresult)
round(prop.table(table(testedpossiblecases$SesScoreQnt, testedpossiblecases$testresult),2)*100,1)
glm(testresult ~ SesScoreQnt + agegr10, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# fever
table(testedpossiblecases$fever, testedpossiblecases$testresult)
prop.table(table(testedpossiblecases$fever, testedpossiblecases$testresult),2)
# univariable
glm(testresult ~ fever, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns
# multivariable (adjusting for age groups)
glm(testresult ~ fever + agegr10, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# throat
glm(testresult ~ throat, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ throat+ agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# rhinorrhea
glm(testresult ~ rhinorrhea, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ rhinorrhea + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# dyspnoea
glm(testresult ~ dyspnoea, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ dyspnoea + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# cough
glm(testresult ~ cough, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ cough + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# chest_pain
glm(testresult ~ chest_pain, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chest_pain + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# vomit
glm(testresult ~ vomit, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ vomit + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# anosmia
glm(testresult ~ anosmia, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ anosmia + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# ageusia
glm(testresult ~ ageusia, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# chills
glm(testresult ~ chills, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chills + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# nausea
glm(testresult ~ nausea , family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ nausea + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# diarrhea
glm(testresult ~ diarrhea, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ diarrhea + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# arthromyalgia
glm(testresult ~ arthromyalgia, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ arthromyalgia + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# myalgia
glm(testresult ~ myalgia, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ myalgia + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# fatigue
glm(testresult ~ fatigue, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ fatigue + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# O2under95
glm(testresult ~ O2under95, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ O2under95 + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# appitite_loss
glm(testresult ~ appitite_loss, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ appitite_loss + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# rash
glm(testresult ~ rash, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ rash + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# headache
glm(testresult ~ headache, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ headache + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# nosebleed
glm(testresult ~ nosebleed, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ nosebleed + agegr10, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# compare the association with anosmia and ageusia during alpha vs. delta vs. omikron peaks
testedpossiblecases$variant[testedpossiblecases$monthonset<"2021-06"] <- "alpha/beta"
testedpossiblecases$variant[testedpossiblecases$monthonset<"2021-11"&testedpossiblecases$monthonset>"2021-05"] <- "delta"
testedpossiblecases$variant[testedpossiblecases$monthonset>"2021-10"] <- "omikron"
testedpossiblecases$variant <- factor(testedpossiblecases$variant, levels = c("alpha/beta", "delta", "omikron"))
table(testedpossiblecases$variant, useNA = "always")

glm(testresult ~ anosmia + agegr10 + variant + anosmia*variant, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + agegr10 + variant + ageusia*variant, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns
# the interaction term between variant and anosmia/ageusia indicates the association disappears during the Omikron pandemic wave

# stratified analysis of only cases reported during omicron wave, to obtain association between associated symptoms and COVID-19 during omikron wave
testedpossiblecases_omikron <- testedpossiblecases %>% filter(testedpossiblecases$monthonset>"2021-10")

glm(testresult ~ anosmia + agegr10, family = "binomial", data = testedpossiblecases_omikron) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + agegr10, family = "binomial", data = testedpossiblecases_omikron) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ appitite_loss + agegr10, family = "binomial", data = testedpossiblecases_omikron) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chills + agegr10, family = "binomial", data = testedpossiblecases_omikron) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# before omikron -> check association without omicron cases
testedpossiblecases_alphabetadelta <- testedpossiblecases %>% filter(testedpossiblecases$monthonset<"2021-11")

glm(testresult ~ anosmia + agegr10, family = "binomial", data = testedpossiblecases_alphabetadelta) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + agegr10, family = "binomial", data = testedpossiblecases_alphabetadelta) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns