#############################################################################
# Population-based COVID-19 surveillance, Maputo, Moz, Dec2020-Mar2022      #
# Repeated SARS-CoV-2 sero-surveys                                          #
# Script to estimate SARS-CoV-2 sero-prevalence and identify risk factors   #
# for SARS-CoV-2 infection (through its proxy, sero-conversion)             #
#############################################################################

# install/load packages
pacman::p_load(readxl,dplyr,lubridate, ggplot2, survival, ggthemes, scales, forcats)

# import data
serosurvey <- read.csv("serosurvey_pseudo.csv")
# reformat date and factor variables
serosurvey$datacolheita <- as.Date(serosurvey$datacolheita)
serosurvey$agegr <- factor(serosurvey$agegr, levels = c("0-17 years", "18-49 years", ">=50 years"))
serosurvey$agegr10 <- factor(serosurvey$agegr10)

# do not consider samples collected after 31 July 2021 (testing incompleted)
serosurvey <- serosurvey %>% filter(datacolheita<as.Date("2021-08-01"))

# remove observation for which there is no age, no date of sample collection, or no result recorded
serosurvey_noNA <- serosurvey %>%
  filter(!is.na(agegr10) & !is.na(datacolheita) & !is.na(analyte25_RBD))

# remove duplicates
dups <- which(duplicated(serosurvey_noNA%>%select(pseudoID, datacolheita)))
serosurvey_noNA <- serosurvey_noNA %>% filter(!row.names(serosurvey_noNA) %in% dups) 

# create the serosurvey result variable used for the main analysis: positive value/antibodies against both RBD (receptor binding proteïn of spike) ánd NP (nucleocapsid protein) antigens, which gave the best diagnostic performance in the validation study
serosurvey_noNA <- serosurvey_noNA %>%
  mutate(
    seroresultRBD_NP = ifelse(
      rowSums(select(., analyte25_RBD, analyte26_NP) >= 1) >= 2,
      "positive after infection",
      "negative"
    )
  )
serosurvey_noNA$seroresultRBD_NP[serosurvey_noNA$seroresultRBD_NP=="positive after infection"&grepl("Sim",serosurvey_noNA$vacinated)==T] <- "positive after vaccination"
table(serosurvey_noNA$seroresultRBD_NP, useNA = "always")

# create an alternative result variable based on positive value Ab against 2 out of RBD, NP, and S1S2, used for a sensitivity analysis
serosurvey_noNA$seroresult2Ag <- "negative"
serosurvey_noNA <- serosurvey_noNA %>%
  mutate(
    seroresult2Ag = ifelse(
      rowSums(select(., analyte21_S1S2, analyte25_RBD, analyte26_NP) >= 1) >= 2,
      "positive after infection",
      "negative"
    )
  )
serosurvey_noNA$seroresult2Ag[serosurvey_noNA$seroresult2Ag=="positive after infection"&grepl("Sim",serosurvey_noNA$vacinated)==T] <- "positive after vaccination"
table(serosurvey_noNA$seroresult2Ag, useNA = "always")

# create a second alternative result variable based on positive value Ab against RBD, used for a sensitivity analysis, not taking NP into account - similar to the antigen target of most commercial ELISA tests
serosurvey_noNA <- serosurvey_noNA %>%
  mutate(
    seroresultRBD = ifelse(analyte25_RBD >= 1,
      "positive after infection",
      "negative"
    )
  )
serosurvey_noNA$seroresultRBD[serosurvey_noNA$seroresultRBD=="positive after infection"&grepl("Sim",serosurvey_noNA$vacinated)==T] <- "positive after vaccination"
table(serosurvey_noNA$seroresultRBD, useNA = "always")

#### 1. NUMBER SERO-SURVEY PARTICIPANTS AND PERCENTAGE SEROCONVERSIONS ####
# number of samples tested
nrow(serosurvey_noNA)
# number of participants with at least one sample tested up to 31 July 2021
num_participants <- serosurvey_noNA %>%
  distinct(pseudoID) %>%
  nrow()
num_participants

# count the number of seroconverted participants, with at least one "positive"
num_participants_with_pos <- serosurvey_noNA %>%
  filter(grepl("positive", seroresultRBD_NP) == T) %>%
  distinct(pseudoID) %>%
  nrow()
num_participants_with_pos
# percentage seroconverted
round(num_participants_with_pos/num_participants*100,1)

# count the number of infection-induced seroconverted participants, with at least one "positive after infection" 
num_infection_induced_seroconverted_participants <- serosurvey_noNA %>%
  filter(grepl("positive after infection", seroresultRBD_NP) == T) %>%
  distinct(pseudoID) %>%
  nrow()
num_infection_induced_seroconverted_participants

# identify participants with infection-induced seroconvertion followed by seroreversion 
participants_pos_neg <- serosurvey_noNA %>%
  arrange(pseudoID, datacolheita) %>%
  group_by(pseudoID) %>%
  mutate(next_result = lead(seroresultRBD_NP)) %>%
  filter(seroresultRBD_NP == "positive after infection" & next_result == "negative") %>%
  distinct(pseudoID) %>%
  nrow()
participants_pos_neg

# sort the data by participant and collection date
serosurvey_noNA <- serosurvey_noNA %>%
  arrange(pseudoID, datacolheita)

# identify and count participants with a "pos" result followed by a result at a later date
participants_with_result_after_pos <- serosurvey_noNA %>%
  group_by(pseudoID) %>%
  filter("pos" %in% Result & lead(Result) != "pos") %>%
  distinct(pseudoID)
num_participants_with_result_after_pos <- nrow(participants_with_result_after_pos)
num_participants_with_result_after_pos
# percentage
round(participants_pos_neg/num_participants_with_result_after_pos*100,1)

#### 2. CHARACTERISTICS OF TESTED SEROSURVEY PARTICIPANTS ####
# keep one line per pseudoID
serosurveyparticipants <- serosurvey_noNA %>% select(pseudoID, sex, agegr10) 
dups <- which(duplicated(serosurveyparticipants%>%select(pseudoID)))
serosurveyparticipants <- serosurveyparticipants %>% filter(!row.names(serosurveyparticipants) %in% dups) 

# age - this can't be done with the anonymized database without a variable age
serosurveyparticipants %>%
  filter(!is.na(age)) %>%
  summarise(mean=mean(age), median=median(age),q25=quantile(age,0.25), q75=quantile(age,0.75))
# agegroup distribution
agegroup_table <- table(serosurveyparticipants$agegr10, useNA = "always")
agegroup_table
round(prop.table(agegroup_table)*100,1)

# sex
sex_table <- table(serosurvey_noNA$sex, useNA = "always")
sex_table
round(prop.table(sex_table)*100,1)

#### 3. SERO-PREVALENCE ####
# create a variable month 
serosurvey_noNA$month <- format(serosurvey_noNA$datacolheita, "%Y-%m")

# 3.1 infection- and vaccination-induced sero-prevalence based on both RBP and NP positive
# crude 
result_monthlycounts_RBD_NP <- serosurvey_noNA %>%
  select(month, seroresultRBD_NP) %>%
  group_by(month, seroresultRBD_NP) %>%
  summarise(n=n()) %>%
  group_by(month) %>%
  mutate(proportion = round(n / sum(n),3))  
# add confidence intervals to proportions
result_monthlycounts_RBD_NP <- result_monthlycounts_RBD_NP %>%
  group_by(month) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(
    prop = n / total_count,
    se = sqrt(prop * (1 - prop) / total_count),
    z = qnorm(0.975),
    ci_low = round(prop - z * se,3),
    ci_high = round(prop + z * se,3)
  )
result_monthlycounts_RBD_NP <- select(result_monthlycounts_RBD_NP, -c("prop", "total_count", "se", "z"))
result_monthlycounts_RBD_NP
write.csv(result_monthlycounts_RBD_NP, "result_monthlycounts_RBD_NP.csv")

# 3.2 infection- and vaccination-induced sero-prevalence of study population based on at least 2 antigen targets positive
# crude 
result_monthlycounts_2Ag <- serosurvey_noNA %>%
  select(month, seroresult2Ag) %>%
  group_by(month, seroresult2Ag) %>%
  summarise(n=n()) %>%
  group_by(month) %>%
  mutate(proportion = round(n / sum(n),3))  
# add confidence intervals to proportions
result_monthlycounts_2Ag <- result_monthlycounts_2Ag %>%
  group_by(month) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(
    prop = n / total_count,
    se = sqrt(prop * (1 - prop) / total_count),
    z = qnorm(0.975),
    ci_low = round(prop - z * se,3),
    ci_high = round(prop + z * se,3)
  )
result_monthlycounts_2Ag <- select(result_monthlycounts_2Ag, -c("prop", "total_count", "se", "z"))
result_monthlycounts_2Ag
write.csv(result_monthlycounts_2Ag, "result_monthlycounts_2Ag.csv")

# 3.3 infection- and vaccination-induced sero-prevalence of study population based on RBD positive
# crude 
result_monthlycounts_RBD <- serosurvey_noNA %>%
  select(month, seroresultRBD) %>%
  group_by(month, seroresultRBD) %>%
  summarise(n=n()) %>%
  group_by(month) %>%
  mutate(proportion = round(n / sum(n),3))  
# add confidence intervals to proportions
result_monthlycounts_RBD <- result_monthlycounts_RBD %>%
  group_by(month) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(
    prop = n / total_count,
    se = sqrt(prop * (1 - prop) / total_count),
    z = qnorm(0.975),
    ci_low = round(prop - z * se,3),
    ci_high = round(prop + z * se,3)
  )
result_monthlycounts_RBD <- select(result_monthlycounts_RBD, -c("prop", "total_count", "se", "z"))
result_monthlycounts_RBD
write.csv(result_monthlycounts_RBD, "result_monthlycounts_RBD.csv")

# 3.4 infection- and vaccination-induced sero-prevalence based on both RBP and NP positive by age
# agegroup distribution
table(serosurvey_noNA$agegr, useNA = "always")

# calculate proportions of categories by month
result_monthlycounts_age <- serosurvey_noNA %>%
  select(agegr, month, seroresultRBD_NP) %>%
  group_by(agegr, month, seroresultRBD_NP) %>%
  summarise(n=n()) %>%
  group_by(month, agegr) %>%
  mutate(proportion = n / sum(n))  

# add confidence intervals to proportions of result (not vaccination)
result_monthlycounts_ageCI <- result_monthlycounts_age %>%
  group_by(agegr, month) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(
    prop = n / total_count,
    se = sqrt(prop * (1 - prop) / total_count),
    z = qnorm(0.975),
    ci_low = prop - z * se,
    ci_high = prop + z * se
  )
result_monthlycounts_ageCI

# histogram
# error bars only relevant for infection-induced positives, so remove SE for others
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="negative"] <- NA
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after vaccination"] <- NA
result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="negative"] <- NA
result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after vaccination"] <- NA
# error bars show the proportion which should not start at 0 but at the proportion vaccinated
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-03"&result_monthlycounts_ageCI$agegr=="18-49 years"] <- result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-03"&result_monthlycounts_ageCI$agegr=="18-49 years"] + 0.01183432
result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-03"&result_monthlycounts_ageCI$agegr=="18-49 years"] <- result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-03"&result_monthlycounts_ageCI$agegr=="18-49 years"] + 0.01183432
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-05"&result_monthlycounts_ageCI$agegr=="18-49 years"] <- result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-05"&result_monthlycounts_ageCI$agegr=="18-49 years"] + 0.01250000
result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-05"&result_monthlycounts_ageCI$agegr=="18-49 years"] <- result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-05"&result_monthlycounts_ageCI$agegr=="18-49 years"] + 0.01250000
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-06"&result_monthlycounts_ageCI$agegr=="18-49 years"] <- result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-06"&result_monthlycounts_ageCI$agegr=="18-49 years"] + 0.06666667
result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-06"&result_monthlycounts_ageCI$agegr=="18-49 years"] <- result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-06"&result_monthlycounts_ageCI$agegr=="18-49 years"] + 0.06666667
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-04"&result_monthlycounts_ageCI$agegr==">=50 years"] <- result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-04"&result_monthlycounts_ageCI$agegr==">=50 years"] + 0.01935484
result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-04"&result_monthlycounts_ageCI$agegr==">=50 years"] <- result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-04"&result_monthlycounts_ageCI$agegr==">=50 years"] + 0.01935484
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-05"&result_monthlycounts_ageCI$agegr==">=50 years"] <- result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-05"&result_monthlycounts_ageCI$agegr==">=50 years"] + 0.03333333
result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-05"&result_monthlycounts_ageCI$agegr==">=50 years"] <- result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-05"&result_monthlycounts_ageCI$agegr==">=50 years"] + 0.03333333
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-06"&result_monthlycounts_ageCI$agegr==">=50 years"] <- result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-06"&result_monthlycounts_ageCI$agegr==">=50 years"] + 0.03225806
result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-06"&result_monthlycounts_ageCI$agegr==">=50 years"] <- result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-06"&result_monthlycounts_ageCI$agegr==">=50 years"] + 0.03225806
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-07"&result_monthlycounts_ageCI$agegr==">=50 years"] <- result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-07"&result_monthlycounts_ageCI$agegr==">=50 years"] + 0.10891089
result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-07"&result_monthlycounts_ageCI$agegr==">=50 years"] <- result_monthlycounts_ageCI$ci_high[result_monthlycounts_ageCI$seroresultRBD_NP=="positive after infection"&result_monthlycounts_ageCI$month=="2021-07"&result_monthlycounts_ageCI$agegr==">=50 years"] + 0.10891089
# remove negative whiskers
result_monthlycounts_ageCI$ci_low[result_monthlycounts_ageCI$ci_low<0] <- 0.01

# reorder agegr so that thenyoungest are on the left, oldest on the right
seroprevalence_agegroup <- ggplot(result_monthlycounts_ageCI, aes(x = month, y = proportion, fill = seroresultRBD_NP)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Month", y = "Proportion", fill = "Result") +
  scale_fill_manual(values = c("darkgreen", "darkred","darkblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~ agegr, scales = "free_x") +
  geom_errorbar(aes(x = month, y = prop, ymin = ci_low, ymax = ci_high),                 
                width = 0.25, 
                color = "#2E0000",  # Color of the error bar lines
                linewidth = 1,        # Thickness of the error bars
                alpha = 0.7)     # Transparency of the error bars)
seroprevalence_agegroup
# save plot
ggsave("seroprevalence_agegroup.jpg", seroprevalence_agegroup, dpi = 250, width = 9, height = 3.5)
ggsave("seroprevalence_agegroup.tiff", device = "tiff", seroprevalence_agegroup, dpi = 250, width = 9, height = 3.5)

# 3.5 sero-prevalence by age based on at least 2 Ag positive
# agegroup distribution
table(serosurvey_noNA$agegr, useNA = "always")

# calculate proportions of categories by month
result_monthlycounts_age <- serosurvey_noNA %>%
  select(agegr, month, seroresult2Ag) %>%
  group_by(agegr, month, seroresult2Ag) %>%
  summarise(n=n()) %>%
  group_by(month, agegr) %>%
  mutate(proportion = n / sum(n))  

# add confidence intervals to proportions of result (not vaccination)
result_monthlycounts_ageCI <- result_monthlycounts_age %>%
  group_by(agegr, month) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(
    prop = n / total_count,
    se = sqrt(prop * (1 - prop) / total_count),
    z = qnorm(0.975),
    ci_low = prop - z * se,
    ci_high = prop + z * se
  )
result_monthlycounts_ageCI

# histogram
# reorder agegr so that thenyoungest are on the left, oldest on the right
seroprevalence_agegroup <- ggplot(result_monthlycounts_age, aes(x = month, y = proportion, fill = seroresult2Ag)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Month", y = "Proportion", fill = "Result") +
  scale_fill_manual(values = c("darkgreen", "darkred","darkblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~ agegr, scales = "free_x") 
# + theme(legend.position = "bottom")
seroprevalence_agegroup
ggsave("seroprevalence_agegroup_2Ag.jpg", seroprevalence_agegroup, dpi = 250, width = 9, height = 3.5)


#### 4. RISK FACTORS OF INFECTION UP TO 31 JULY ####
# remove serosurvey visits after vaccination
serosurvey_unvacc <- serosurvey_noNA %>% filter(grepl("Sim",serosurvey_noNA$vacinated)==F)

# transform serosurvey to survival data, generating for each pseudoID a variable time (observation) and a variable event (1 = serocoverted, 0 = no seroconversion) 
# generate a time variable with time since previous observation, or 90 days (3 months) if no prior visit
serosurvey_surv <- serosurvey_unvacc %>%
  group_by(pseudoID) %>%
  arrange(pseudoID, datacolheita) %>%
  mutate(
    time = c(90, diff(datacolheita))  # Calculate time since previous observation
  ) %>%
  ungroup()
# if test was positive, time is the midpoint between the previous and the current visit
# serosurvey_surv$time[serosurvey_surv$Result=='pos'] <- (serosurvey_surv$time[serosurvey_surv$Result=='pos'])/2
serosurvey_surv$time[serosurvey_surv$seroresultRBD_NP=='positive after infection'] <- (serosurvey_surv$time[serosurvey_surv$seroresultRBD_NP=='positive after infection'])/2

# remove observations of which datacolheita follows an observation with as variable Result 'pos'
serosurvey_surv_censored <- serosurvey_surv %>%
  arrange(pseudoID, datacolheita) %>%
  group_by(pseudoID) %>%
  filter(!(lag(seroresultRBD_NP, default = "") == "positive after infection" & seroresultRBD_NP == "positive after infection")) %>%
  ungroup()

# summarised to one row per pseudoID, adding a variable event which is 1 if one of the Result values is 'pos' and 0 if one of the Result variables is 'neg', 
# a variable 'time' (in months) which is the sum of time (in days) of all observations of that participant, 
# a variable 'ageyears' being the mean of age values at the time of visits,
# a variable 'inclusiondate' which is the first date of 'start', and 
# a variable 'nsurveys' which is the number of rows of that pseudoID
serosurvey_surv_bl <- serosurvey_surv_censored %>%
  group_by(pseudoID) %>%
  summarise(
    event = ifelse("positive after infection" %in% seroresultRBD_NP, 1, 0),
    time = sum(time) / 30,
#    ageyears = mean(age),
    inclusiondate = min(datacolheita),
    nsurveys = n(),
    sex = first(sex),
    ednivel_educacao = first(ednivel_educacao),
    SesScoreQnt = first(SesScoreQnt),
    mass_bus = first(mass_bus),
    main_bus = first(main_bus),
    bus_use = first(bus_use),
#    main_ocupation = first(main_ocupation),
#    main_ocupation_specicy = first(main_ocupation_specicy),
    type_cancer = first(type_cancer),
    cancer_histories = first(cancer_histories),
    other_imunodef = first(other_imunodef),
    specify_other_imunodef = first(specify_other_imunodef),
    if_diagnosed_hypert = first(if_diagnosed_hypert),
    if_diagnosed_heart_diseases = first(if_diagnosed_heart_diseases),
    hospitalized_avc = first(hospitalized_avc),
    specify_chronic_lung_disease = first(specify_chronic_lung_disease),
    specify_other_hemat = first(specify_other_hemat),
    chronic_kidney_disease = first(chronic_kidney_disease),
    chronic_neuro_disease = first(chronic_neuro_disease),
    specify_chronic_neuro_disease = first(specify_chronic_neuro_disease),
    tb = first(tb),
    smoking = first(smoking),
    other_health_condition = first(other_health_condition),
    # specify_other_health_condition = first(specify_other_health_condition),
#    other_drugs = first(other_drugs),
#    drug_available = first(drug_available),
    # if_yes_indicate = first(if_yes_indicate),
    bedroom_sharing = first(bedroom_sharing),
    toylet_sharing = first(toylet_sharing),
    handwash = first(handwash),
    water_availability = first(water_availability),
    soap_availability = first(soap_availability),
    health_care_preference = first(health_care_preference),
    health_care_preference_other = first(health_care_preference_other),
    health_worker = first(health_worker),
    # MUAC = first(MUAC),
    BMI = first(BMI),
    hiv = last(hiv),
    pregnancy_history = first(pregnancy_history),
    hypertension = first(hypertension),
    diabet = first(diabet),
    cancer = first(cancer),
    hearth_diseases = first(hearth_diseases),
    chronic_lung_disease = first(chronic_lung_disease),
    chronic_liver_disease = first(chronic_liver_disease),
    chronic_hematological_disease = first(chronic_hematological_disease),
    agegr = first(agegr), 
    agegr10 = first(agegr10),
    inside_outside_city = first(inside_outside_city),
    did_work = first(did_work),
    would_stay_home = first(would_stay_home),
    wore_mask_yesterday = first(wore_mask_yesterday),
  ) %>%
  ungroup()

# create factor variables with categories useful for analysis
serosurvey_surv_bl$agegr10 <- factor(serosurvey_surv_bl$agegr10)
serosurvey_surv_bl$sex <- factor(serosurvey_surv_bl$sex)
serosurvey_surv_bl$agegr <- factor(serosurvey_surv_bl$agegr)
serosurvey_surv_bl$lowestSES[!is.na(serosurvey_surv_bl$SesScoreQnt=="1. very low")] <- 0
serosurvey_surv_bl$lowestSES[serosurvey_surv_bl$SesScoreQnt=="1. very low"] <- 1
serosurvey_surv_bl$SesScoreQnt <- factor(serosurvey_surv_bl$SesScoreQnt)
serosurvey_surv_bl$overweight[!is.na(serosurvey_surv_bl$BMI)] <- "normal"
serosurvey_surv_bl$overweight[serosurvey_surv_bl$BMI>24.99&serosurvey_surv_bl$BMI<30] <- "overweight"
serosurvey_surv_bl$overweight[serosurvey_surv_bl$BMI>29.99&serosurvey_surv_bl$BMI<50] <- "obesity"
serosurvey_surv_bl$overweight[serosurvey_surv_bl$BMI<19&serosurvey_surv_bl$BMI>8] <- "underweight"
serosurvey_surv_bl$overweight <- factor(serosurvey_surv_bl$overweight)
serosurvey_surv_bl$education[serosurvey_surv_bl$ednivel_educacao=="1.  Primario incompleto"] <- "none completed"
serosurvey_surv_bl$education[serosurvey_surv_bl$ednivel_educacao=="8. Nenhum"] <- "none completed"
serosurvey_surv_bl$education[serosurvey_surv_bl$ednivel_educacao=="2. Primario completo"] <- "primary"
serosurvey_surv_bl$education[serosurvey_surv_bl$ednivel_educacao=="7. Pos-graduacao"] <- "higher"
serosurvey_surv_bl$education[serosurvey_surv_bl$ednivel_educacao=="6. Superior"] <- "higher"
serosurvey_surv_bl$education[serosurvey_surv_bl$ednivel_educacao=="5. Tecnico-profissional"] <- "higher"
serosurvey_surv_bl$education[grepl("cundario incom", serosurvey_surv_bl$ednivel_educacao)==T] <- "primary"
serosurvey_surv_bl$education[serosurvey_surv_bl$ednivel_educacao=="4. Secundario completo"] <- "secondary"
serosurvey_surv_bl$education <- factor(serosurvey_surv_bl$education)
serosurvey_surv_bl$hypertensionbin[grepl("Não", serosurvey_surv_bl$hypertension)==T] <- 0
serosurvey_surv_bl$hypertensionbin[grepl("Diagnosticado,", serosurvey_surv_bl$hypertension)==T] <- 1
serosurvey_surv_bl$hypertensionbin[grepl("Diagnostico", serosurvey_surv_bl$hypertension)==T] <- 1
serosurvey_surv_bl$diabetesbin[grepl("Não", serosurvey_surv_bl$diabet)==T] <- 0
serosurvey_surv_bl$diabetesbin[grepl("Diagnosticado,", serosurvey_surv_bl$diabet)==T] <- 1
serosurvey_surv_bl$hivbin[serosurvey_surv_bl$hiv=="Crianca exposta"] <- 1
serosurvey_surv_bl$hivbin[grepl("Seropositivo", serosurvey_surv_bl$hiv)==T] <- 1
serosurvey_surv_bl$hivbin[serosurvey_surv_bl$hiv=="estado desconhecido"] <- 0
serosurvey_surv_bl$hivbin[serosurvey_surv_bl$hiv=="HIV negativo (no momento do ultimo teste HIV)"] <- 0
serosurvey_surv_bl$smokingbin[grepl("fumador", serosurvey_surv_bl$smoking)==T] <- "(ex-)smoker"
serosurvey_surv_bl$smokingbin[grepl("Fumador", serosurvey_surv_bl$smoking)==T] <- "(ex-)smoker"
serosurvey_surv_bl$smokingbin[grepl("Nao", serosurvey_surv_bl$smoking)==T] <- "non smoker"
serosurvey_surv_bl$smokingbin[grepl("Nunca", serosurvey_surv_bl$smoking)==T] <- "non smoker"
serosurvey_surv_bl$publictransport_pastweek[serosurvey_surv_bl$mass_bus=="Não"] <- "none"
serosurvey_surv_bl$publictransport_pastweek[serosurvey_surv_bl$main_bus=="taxi Nao partilhado com outros"] <- "none"
serosurvey_surv_bl$publictransport_pastweek[serosurvey_surv_bl$main_bus=="minibus"] <- "bus/train"
serosurvey_surv_bl$publictransport_pastweek[serosurvey_surv_bl$main_bus=="comboio"] <- "bus/train"
serosurvey_surv_bl$publictransport_pastweek[serosurvey_surv_bl$main_bus=="autocarro publico"] <- "bus/train"
serosurvey_surv_bl$publictransport_pastweek[serosurvey_surv_bl$main_bus=="taxi partilhado"] <- "moto taxi/shared taxi"
serosurvey_surv_bl$publictransport_pastweek[serosurvey_surv_bl$main_bus=="moto taxi"] <- "moto taxi/shared taxi"
serosurvey_surv_bl$publictransport_pastweek <- factor(serosurvey_surv_bl$publictransport_pastweek)
serosurvey_surv_bl$pregnant[grepl("Sim", serosurvey_surv_bl$main_bus)==T] <- "pregnant"
serosurvey_surv_bl$pregnant[grepl("Não", serosurvey_surv_bl$main_bus)==T] <- "not"
serosurvey_surv_bl$leukemia[!is.na(serosurvey_surv_bl$cancer)] <- "not"
serosurvey_surv_bl$leukemia[grepl("malignidade hemato", serosurvey_surv_bl$cancer)==T] <- "leukemia"
serosurvey_surv_bl$heart[!is.na(serosurvey_surv_bl$hearth_diseases)] <- "not"
serosurvey_surv_bl$heart[grepl("Diagno", serosurvey_surv_bl$hearth_diseases)==T] <- "heart disease"
serosurvey_surv_bl$lungdisease[grepl("Nao", serosurvey_surv_bl$chronic_lung_disease)==T] <- "not"
serosurvey_surv_bl$lungdisease[grepl("asma", serosurvey_surv_bl$chronic_lung_disease)==T] <- "asthma"
serosurvey_surv_bl$lungdisease[grepl("outra", serosurvey_surv_bl$chronic_lung_disease)==T] <- "chronic pulmonary disease"
serosurvey_surv_bl$tbbin[!is.na(serosurvey_surv_bl$tb)] <- "not"
serosurvey_surv_bl$tbbin[grepl("tubercu", serosurvey_surv_bl$tb)==T] <- "(history of) tuberculosis"
serosurvey_surv_bl$tbbin <- factor(serosurvey_surv_bl$tbbin)
serosurvey_surv_bl$bedroom_sharing <- factor(serosurvey_surv_bl$bedroom_sharing)
serosurvey_surv_bl$toylet_sharing <- factor(serosurvey_surv_bl$toylet_sharing)
serosurvey_surv_bl$handwash <- factor(serosurvey_surv_bl$handwash)
serosurvey_surv_bl$water_availability <- factor(serosurvey_surv_bl$water_availability)
serosurvey_surv_bl$soap_availability <- factor(serosurvey_surv_bl$soap_availability)
serosurvey_surv_bl$health_worker <- factor(serosurvey_surv_bl$health_worker)
serosurvey_surv_bl$bedroom_sharing[grepl("6", serosurvey_surv_bl$bedroom_sharing)==T] <- "3 a 5"
serosurvey_surv_bl$inside_outside_city <- factor(serosurvey_surv_bl$inside_outside_city)
serosurvey_surv_bl$did_work <- factor(serosurvey_surv_bl$did_work)
serosurvey_surv_bl$would_stay_home <- factor(serosurvey_surv_bl$would_stay_home)
serosurvey_surv_bl$wore_mask_yesterday <- factor(serosurvey_surv_bl$wore_mask_yesterday)

# change category to use as reference
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(SesScoreQnt = fct_relevel(SesScoreQnt, "5. highest", after = 0)) 
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(education = fct_relevel(education, "secondary", after = 0)) 
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(sex = fct_relevel(sex, "M", after = 0)) 
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(heart = fct_relevel(heart, "not", after = 0)) 
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(leukemia = fct_relevel(leukemia, "not", after = 0)) 
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(tbbin = fct_relevel(tbbin, "not", after = 0)) 
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(lungdisease = fct_relevel(lungdisease, "not", after = 0)) 
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(water_availability = fct_relevel(water_availability, "Disponivel", after = 0)) 
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(publictransport_pastweek = fct_relevel(publictransport_pastweek, "none", after = 0)) 
serosurvey_surv_bl <- serosurvey_surv_bl %>% 
  mutate(smokingbin = fct_relevel(smokingbin, "non smoker", after = 0)) 

##### 4.1. UNIVARIABLE ANALYSIS ####
age <- serosurvey_surv_bl %>%
  group_by(agegr10) %>%
  summarise(total=n(), pcttotal=(round(n()/count(serosurvey_surv_bl)*100,1)), covid19=sum(event), pctcases=round(sum(event)/sum(serosurvey_surv_bl$event)*100,1))
sex <- serosurvey_surv_bl %>%
  group_by(sex) %>%
  summarise(total=n(), 
            pcttotal=(round(n()/count(serosurvey_surv_bl)*100,1)), 
            covid19=sum(event), 
            pctcases=round(sum(event)/sum(serosurvey_surv_bl$event)*100,1))
lowestSES <- serosurvey_surv_bl %>%
  group_by(lowestSES) %>%
  summarise(total=n(), pcttotal=(round(n()/count(serosurvey_surv_bl)*100,1)), covid19=sum(event), pctcases=round(sum(event)/sum(serosurvey_surv_bl$event)*100,1))
SES <- serosurvey_surv_bl %>%
  filter(agegr!="0-17 years") %>%
  filter(!is.na(SesScoreQnt)) %>%
  group_by(SesScoreQnt) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / count(serosurvey_surv_bl[!is.na(serosurvey_surv_bl$SesScoreQnt) & serosurvey_surv_bl$agegr!="0-17 years", ]))*100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$SesScoreQnt) & serosurvey_surv_bl$agegr!="0-17 years"]))*100,1)
  )
education <- serosurvey_surv_bl %>%
  filter(!is.na(education)) %>%
  filter(agegr!="0-17 years") %>%
  group_by(education) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / count(serosurvey_surv_bl[!is.na(serosurvey_surv_bl$education) & serosurvey_surv_bl$agegr!="0-17 years", ]))*100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$education) & serosurvey_surv_bl$agegr!="0-17 years"]))*100,1)
  )
health_worker <- serosurvey_surv_bl %>%
  filter(agegr!="0-17 years") %>%
  filter(!is.na(health_worker)) %>%
  group_by(health_worker) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / count(serosurvey_surv_bl[!is.na(serosurvey_surv_bl$health_worker) & serosurvey_surv_bl$agegr!="0-17 years", ]))*100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$health_worker) & serosurvey_surv_bl$agegr!="0-17 years"]))*100,1)
  )
hiv <- serosurvey_surv_bl %>%
  filter(!is.na(hivbin)) %>%
  group_by(hivbin) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$hivbin))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$hivbin)])) * 100,1)
  )
hypertension <- serosurvey_surv_bl %>%
  filter(!is.na(hypertensionbin)) %>%
  group_by(hypertensionbin) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$hypertensionbin))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$hypertensionbin)])) * 100,1)
  )
diabetes <- serosurvey_surv_bl %>%
  filter(!is.na(diabetesbin)) %>%
  group_by(diabetesbin) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$diabetesbin))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$diabetesbin)])) * 100,1)
  )
overweight <- serosurvey_surv_bl %>%
  filter(!is.na(overweight)) %>%
  group_by(overweight) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$overweight))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$overweight)])) * 100,1)
  )

publictransport_pastweek <- serosurvey_surv_bl %>%
  filter(!is.na(publictransport_pastweek)) %>%
  group_by(publictransport_pastweek) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$publictransport_pastweek))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$publictransport_pastweek)])) * 100,1)
  )
smokingbin <- serosurvey_surv_bl %>%
  filter(!is.na(smokingbin)) %>%
  group_by(smokingbin) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$smokingbin))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$smokingbin)])) * 100,1)
  )
pregnant <- serosurvey_surv_bl %>%
  filter(!is.na(pregnant)) %>%
  group_by(pregnant) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$pregnant))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$pregnant)])) * 100,1)
  )
tb <- serosurvey_surv_bl %>%
  filter(!is.na(tbbin)) %>%
  group_by(tbbin) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$tbbin))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$tbbin)])) * 100,1)
  )
lungdisease <- serosurvey_surv_bl %>%
  filter(!is.na(lungdisease)) %>%
  group_by(lungdisease) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$lungdisease))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$lungdisease)])) * 100,1)
  )
heart <- serosurvey_surv_bl %>%
  filter(!is.na(heart)) %>%
  group_by(heart) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$heart))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$heart)])) * 100,1)
  )

leukemia <- serosurvey_surv_bl %>%
  filter(!is.na(leukemia)) %>%
  group_by(leukemia) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$leukemia))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$leukemia)])) * 100,1)
   )
  
bedroom_sharing <- serosurvey_surv_bl %>%
  filter(!is.na(bedroom_sharing)) %>%
  group_by(bedroom_sharing) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$bedroom_sharing))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$bedroom_sharing)])) * 100,1)
  )
toylet_sharing <- serosurvey_surv_bl %>%
  filter(!is.na(toylet_sharing)) %>%
  group_by(toylet_sharing) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$toylet_sharing))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$toylet_sharing)])) * 100,1)
  )
handwash <- serosurvey_surv_bl %>%
  filter(!is.na(handwash)) %>%
  group_by(handwash) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$handwash))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$handwash)])) * 100,1)
  )
water_availability <- serosurvey_surv_bl %>%
  filter(!is.na(water_availability)) %>%
  group_by(water_availability) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$water_availability))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$water_availability)])) * 100,1)
  )
soap_availability <- serosurvey_surv_bl %>%
  filter(!is.na(soap_availability)) %>%
  group_by(soap_availability) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$soap_availability))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$soap_availability)])) * 100,1)
  )
inside_outside_city <- serosurvey_surv_bl %>%
  filter(!is.na(inside_outside_city)) %>%
  group_by(inside_outside_city) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$inside_outside_city))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$inside_outside_city)])) * 100,1)
  )
did_work <- serosurvey_surv_bl %>%
  filter(!is.na(did_work)) %>%
  group_by(did_work) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$did_work))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$did_work)])) * 100,1)
  )
would_stay_home <- serosurvey_surv_bl %>%
  filter(!is.na(would_stay_home)) %>%
  group_by(would_stay_home) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$would_stay_home))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$would_stay_home)])) * 100,1)
  )
wore_mask_yesterday <- serosurvey_surv_bl %>%
  filter(!is.na(wore_mask_yesterday)) %>%
  group_by(wore_mask_yesterday) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(serosurvey_surv_bl$wore_mask_yesterday))) * 100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(serosurvey_surv_bl$event[!is.na(serosurvey_surv_bl$wore_mask_yesterday)])) * 100,1)
  )
serosurvey_surv_bl %>% filter(!is.na(mean_n_contactos)) %>% group_by(event) %>% summarize(mean(mean_n_contactos))

##### 4.2. COX REGRESSION calculating hazard ratios, adjusted for age ####
# age groups
# Fit the Cox regression model with agegr10 as a categorical variable - SesScoreQnt taken out for 
cox_model <- coxph(Surv(time, event) ~ agegr10, data = serosurvey_surv_bl)

# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
agegrresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                             CI_Lower = rounded_conf_intervals[, 1],
                             CI_Upper = rounded_conf_intervals[, 2])
print(agegrresult)

# sex 
cox_model <- coxph(Surv(time, event) ~ agegr10 + sex, data = serosurvey_surv_bl) # analysis done with variable age in years, but replaced by age groups to anonymize data

# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
sexresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                        CI_Lower = rounded_conf_intervals[, 1],
                        CI_Upper = rounded_conf_intervals[, 2])
print(sexresult)

# SES - excluding children
cox_model <- coxph(Surv(time, event) ~ agegr10 + SesScoreQnt, data = serosurvey_surv_bl[!is.na(serosurvey_surv_bl$SesScoreQnt) & serosurvey_surv_bl$agegr != "0-17 years", ])

# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
SESresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                           CI_Lower = rounded_conf_intervals[, 1],
                           CI_Upper = rounded_conf_intervals[, 2])
print(SESresult)

# education - excluding children
cox_model <- coxph(Surv(time, event) ~ agegr10 + education, data = serosurvey_surv_bl[!is.na(serosurvey_surv_bl$education) & serosurvey_surv_bl$agegr != "0-17 years", ])

# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
edresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                        CI_Lower = rounded_conf_intervals[, 1],
                        CI_Upper = rounded_conf_intervals[, 2])
print(edresult)

# fit model for each variable, adjusting for age and sex
explanatory_variables <- c("smokingbin", "publictransport_pastweek", "bedroom_sharing", "toylet_sharing", "handwash", "water_availability", "soap_availability", "health_worker", "did_work")

for (variable in explanatory_variables) {
  # Create a formula including the variable, age, and sex
  formula <- as.formula(paste("Surv(time, event) ~ ", variable, "+ agegr10"))
  
  # Fit the Cox regression model
  cox_model <- coxph(formula, data = serosurvey_surv_bl)
  
  # Extract hazard ratios and their confidence intervals
  hr_ci <- round(exp(coef(cox_model)),2)
  ci <- round(exp(confint(cox_model)),2)
  
  # Print the results
  cat("Variable:", variable, "\n")
  cat("Hazard Ratios:\n")
  print(hr_ci)
  cat("Confidence Intervals:\n")
  print(ci)
  cat("\n")
}

# comorbidities "hivbin", "tbbin", "hypertensionbin", "diabetesbin", "heart", "lung", "pregnant"
explanatory_variables <- c("overweight", "hivbin", "tbbin", "hypertensionbin", "diabetesbin", "heart", "lungdisease", "leukemia", "pregnant")

for (variable in explanatory_variables) {
  # Create a formula including the variable, age, and sex
  formula <- as.formula(paste("Surv(time, event) ~ ", variable, "+ agegr10 + sex"))
  
  # Fit the Cox regression model
  cox_model <- coxph(formula, data = serosurvey_surv_bl)
  
  # Extract hazard ratios and their confidence intervals
  hr_ci <- round(exp(coef(cox_model)),2)
  ci <- round(exp(confint(cox_model)),2)
  
  # Print the results
  cat("Variable:", variable, "\n")
  cat("Hazard Ratios:\n")
  print(hr_ci)
  cat("Confidence Intervals:\n")
  print(ci)
  cat("\n")
}

# BMI
# filter out children
serosurvey_surv_bl_ad <- serosurvey_surv_bl %>% filter(ageyears>15.99)
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ agegr10 + sex + overweight, data = serosurvey_surv_bl_ad)

# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
overweightresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                               CI_Lower = rounded_conf_intervals[, 1],
                               CI_Upper = rounded_conf_intervals[, 2])
print(overweightresult)


# public transport  
serosurvey_surv_bl_nosharedtaxi <- serosurvey_surv_bl %>% filter(publictransport_pastweek!="moto taxi/shared taxi")
# Change the factor level and set "3. med." as the reference level
serosurvey_surv_bl <- serosurvey_surv_bl %>%
  mutate(publictransport_pastweek = relevel(publictransport_pastweek, ref = "none"))
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ agegr10 + sex + publictransport_pastweek, data = serosurvey_surv_bl)
# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
publictransport_pastweek <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                                       CI_Lower = rounded_conf_intervals[, 1],
                                       CI_Upper = rounded_conf_intervals[, 2])
print(publictransport_pastweek)
