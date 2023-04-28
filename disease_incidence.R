#############################################################################
# AFRICOVER                                                                 #
# Script to estimate disease incidence (cases from active surveillance)     #
#############################################################################

# install/load packages
pacman::p_load(readxl,lubridate, ggplot2, ggmap, httr, lmtest, scales, usethis, tidyverse, stringr, purrr, gtsummary, broom, lmtest, parameters, see)

# import data of possible cases with baseline
possiblecases_bl <- read.csv("./possiblecases_bl.csv")
# create a variable month
possiblecases_bl$month <- format(as.Date(possiblecases_bl$datacolheita, "%Y-%m-%d"), "%Y-%m")
table(possiblecases_bl$month)

# import all participant data
participants <- read.csv("participants.csv")

# import household visit data
FU <- read.csv("FU.csv")

# import geo coordinates of households
visit_geompoints <- read.csv("visit_geompoints.csv")

#### 1. HOUSEHOLD VISITS & EPICURVE ####
# number of visits
str(FU$dateHHID)

# number of visits during which at least one household member had respiratory symptoms
table(FU$respiratory_symptoms, useNA = "always")

# map of households visited, with colour depending on the frequency of visits
# look up limits of the area
min(geopoints$latitude[!is.na(geopoints$latitude)])
max(geopoints$latitude[!is.na(geopoints$latitude)])
min(geopoints$longitude[!is.na(geopoints$longitude)])
max(geopoints$longitude[!is.na(geopoints$longitude)])
# set the map extent
maputo_bbox <- c(left = 32.59, bottom = -25.95, right = 32.63, top = -25.923)
# Get the map using the Stamen source
# maputo <- get_openstreetmap(bbox = maputo_bbox) # doesn't work for now
maputo <- get_stamenmap(bbox = maputo_bbox, maptype = "toner-lite", zoom = 16) 
maputo <- get_stamenmap(bbox = maputo_bbox, maptype = "terrain", zoom = 16)
ggmap(maputo)
# api_key <- "HZlfIpAihcDRbZ95DIvK9g"
# # Get the map using Here Maps
# maputo <- get_heremap(app_id = NULL, app_code = NULL, api_key = api_key, 
#                       zoom = 14, bbox = maputo_bbox, maptype = "normal.day") # doesn't find the function for some reason

# plot the map using ggmap and add the point layer
dottedmapvisits <- ggmap(maputo) + 
  geom_point(data = visit_geompoints, aes(x = longitude, y = latitude, color = n),
             alpha = 0.3) +
  scale_color_gradient(low = "#ADD8E6", high = "#000080") +
  guides(size = FALSE, shape = FALSE) +
  labs(color = "Number of \nhousehold visits") +
  theme(legend.title = element_text(size = 9)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))
dottedmapvisits
ggsave("dottedmapvisits.jpg", dottedmapvisits, dpi = 300, width = 8, height = 6)


dottedmap <- ggmap(south_west_cameroon) +
  geom_point(data = casecountbylocationandbystart, aes(x = Longitude, y = Latitude, color = dstartoutbreak, size = n)) +
  geom_text(data = districtlocation, aes(x = meanlon, y = meanlat,  
                                         label = healthdistrict_simpl), size = 4, nudge_x = 0.03, nudge_y = 0.03) +
  labs(size = "n", color = "dstartoutbreak") +
  scale_color_date(date_labels = "%m/%d/%Y", name = "dstartoutbreak", low = "red", high = "blue") 
dottedmap
packageVersion("ggmap")




# plot of weekly visits
FU_weekly <- FU %>%
  mutate(week = as.Date(cut(contact_date, breaks = "1 week")))

histogramvisits <- ggplot(FU_weekly, aes(x = week, fill = respiratory_symptoms)) +
  geom_histogram(binwidth = 7, color = "black", position = "stack") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", 
               expand = c(0, 0)) +
  ggtitle("Frequency of Household Visits") +
  labs(fill = "At least one household member with respiratory symptoms") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.background = element_rect(fill = "white")) +
  theme(legend.position = "bottom") +
  xlab("Month") +
  ylab("Frequency")
ggsave(plot = histogramvisits,"histogramvisits.jpg", width = 10, height = 5, dpi = 300)

# epicurve of possible and confirmed cases
# make sure there's always a start date
possiblecases$dateonset <- as.Date(possiblecases$data_inicio_sintomas)
possiblecases$dateonset[is.na(possiblecases$data_inicio_sintomas)] <- as.Date(possiblecases$datacolheita.x)
possiblecases$dateonset[is.na(possiblecases$data_inicio_sintomas)&is.na(possiblecases$datacolheita.x)] <- as.Date(possiblecases$datacolheita.y)
possiblecases$dateonset[is.na(possiblecases$data_inicio_sintomas)&is.na(possiblecases$datacolheita.x)&is.na(possiblecases$datacolheita.y)] <- as.Date(possiblecases$start)
table(possiblecases$dateonset, useNA = "always")
# remove a start date before 1 December
possiblecases$dateonset[possiblecases$dateonset=="2020-11-01"] <- "2020-12-16" # replaced by data de colheita
# make sure there's always a test result
possiblecases$testresult[is.na(possiblecases$testresult)] <- "not tested"
# weekly n of cases
cases_weekly <- possiblecases %>%
  mutate(week = as.Date(cut(dateonset, breaks = "1 week")))
#histogram
histogramcases <- ggplot(cases_weekly, aes(x = week, fill = testresult)) +
  geom_histogram(binwidth = 7, color = "black", position = "stack") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", 
               expand = c(0, 0)) +
  ggtitle("Number of reported/confirmed cases") +
  labs(fill = "Nasal swab PCR test result") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.background = element_rect(fill = "white")) +
  theme(legend.position = "bottom") +
  xlab("Month") +
  ylab("Frequency")
histogramcases
ggsave(plot = histogramcases,"histogramcases.jpg", width = 10, height = 5, dpi = 300)


#### 2. DESCRIPTION OF PARTICIPANTS ####
# describe number of participants and HHs
count(participants)
table(participants$agegr, useNA = "always")
nHH <- participants %>%
  group_by(locationid) %>%
  summarise(n=n())
count(nHH) # 1489
mean(nHH$n) # mean HH size = 4.03

# mark which participant had symptoms, and which tested covid +
possiblecases$confirmed <- 0
possiblecases$confirmed[possiblecases$testresult=="positive"] <- 1

participants_with_episode <- possiblecases %>% filter(!is.na(individualid)) %>% group_by(individualid) %>% summarise(n_episodes=n(), n_confirmed=sum(confirmed))
participantstest <- merge(participants, participants_with_episode, by.x = "openhdsindividualId", by.y = "individualid", all = T)
table(participantstest$n_episodes, useNA = "always")
table(participantstest$n_confirmed, useNA = "always")
participantstest$possibleconfirmed <- "participants"
participantstest$possibleconfirmed[participantstest$n_episodes>0] <- "COVID-19 negative"
participantstest$possibleconfirmed[participantstest$n_confirmed>0] <- "confirmed COVID-19"
table(participantstest$possibleconfirmed)

# create a histogram of age distribution, facetted by the possibleconfirmed variable
participantstest$possibleconfirmed <- factor(participantstest$possibleconfirmed, 
                                         levels = c("participants", "COVID-19 negative", "confirmed COVID-19"))

agehistogram <- ggplot(participantstest, aes(x = age)) +
  geom_histogram(bins = 20, color = "white", fill = "steelblue") +
  facet_wrap(~ possibleconfirmed, scales = "free_y") +
  labs(title = "Age Distribution among participants, possible and confirmed COVID-19 cases",
       x = "Age", y = "Count")
agehistogram
ggsave(plot = agehistogram,"agehistogram.jpg", width = 7, height = 4, dpi = 300)

# sex distribution
sextable <- table(participantstest$possibleconfirmed,participantstest$GENDER)
round(prop.table(sextable, 1)*100,0)
sextable_possible <- table(possiblecases_bl$testresult,possiblecases_bl$GENDER)
round(prop.table(sextable_possible, 1)*100,0)

#### 3. DESCRIPTION OF POSSIBLE CASES ####
# number of possible cases, in number of households, & number of cases tested
possiblecases_bl %>% group_by(visitid) %>% dplyr::summarise(n=n())
761-134

# mean age participants
participants %>%
  filter(!is.na(age)) %>%
  summarise(median=median(age),q25=quantile(age,0.25), q75=quantile(age,0.75))

# mean age possible cases, by test result
possiblecases_bl %>%
  filter(!is.na(age)) %>%
  group_by(testresult) %>%
  summarise(median=median(age),q25=quantile(age,0.25), q75=quantile(age,0.75))

# age & sex distribution of possible cases
table(possiblecases_bl$agegr)
prop.table(table(possiblecases_bl$agegr))

table(possiblecases_bl$sexo)
prop.table(table(possiblecases_bl$sexo))

# symptoms and onset
table(possiblecases_bl$symptoms)
# fever
table(possiblecases_bl$fever)
prop.table(table(possiblecases_bl$fever))
# anosmia
table(possiblecases_bl$anosmia)
prop.table(table(possiblecases_bl$anosmia))
# aguesia
table(possiblecases_bl$ageusia)
prop.table(table(possiblecases_bl$ageusia))
# at least one resp symptom
possiblecases_bl$atleastonesympt <- 0
possiblecases_bl$atleastonesympt[possiblecases_bl$throat=="Sim"] <- 1
possiblecases_bl$atleastonesympt[possiblecases_bl$cough=="Sim"] <- 1
possiblecases_bl$atleastonesympt[possiblecases_bl$dyspnoea=="Sim"] <- 1
possiblecases_bl$atleastonesympt[possiblecases_bl$rhinorrhea=="Sim"] <- 1
possiblecases_bl$atleastonesympt[possiblecases_bl$throat=="Sim"] <- 1
possiblecases_bl$atleastonesympt[possiblecases_bl$O2under95=="yes"] <- 1
table(possiblecases_bl$atleastone)
prop.table(table(possiblecases_bl$atleastone))

# comorbidities
table(participants$hiv, useNA = "always")
(410+25+15)/6006

# delay between onset and nasal swab
date_heita = as.Date(possiblecases_bl$datacolheita[!is.na(possiblecases_bl$datacolheita)&!is.na(possiblecases_bl$data_inicio_sintomas)], format = "%Y-%m-%d") 
date_sinto = as.Date(possiblecases_bl$data_inicio_sintomas[!is.na(possiblecases_bl$datacolheita)&!is.na(possiblecases_bl$data_inicio_sintomas)], format = "%Y-%m-%d") 

possiblecases_bl$delay[!is.na(possiblecases_bl$datacolheita)&!is.na(possiblecases_bl$data_inicio_sintomas)] <- date_heita-date_sinto
mean(possiblecases_bl$delay[!is.na(possiblecases_bl$delay)])
median(possiblecases_bl$delay[!is.na(possiblecases_bl$delay)])
quantile(possiblecases_bl$delay[!is.na(possiblecases_bl$delay)],0.25)
quantile(possiblecases_bl$delay[!is.na(possiblecases_bl$delay)],0.50)
quantile(possiblecases_bl$delay[!is.na(possiblecases_bl$delay)],0.75)

# describe frequencies
# results
table(possiblecases_bl$testresult, useNA = "always")
round(prop.table(table(possiblecases_bl$testresult))*100,2)

# histogram possible case number
monthlypossiblecases <- possiblecases_bl %>%
  filter(testresult!="nao identificado nas duas bases"&testresult!="si") %>%
  group_by(month, testresult) %>%
  summarise(n=n())
ggplot2::theme_set(theme_classic(base_size = 18))
monthlypossiblecasesplot <- ggplot(monthlypossiblecases, aes(x=month, y=n, fill=factor(testresult))) +
  geom_col() +
  labs(title="", x = "", y="Number of possible cases") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) 
monthlypossiblecasesplot

# table cases per month
table(possiblecases_bl$month)
table(possiblecases_bl$month, possiblecases_bl$testresult)
prop.table(table(possiblecases_bl$month, possiblecases_bl$testresult),1)*100
# using a moving average over 7 days
# summarize daily possible case number

# create a vector dates to merge and make sure every date gets a value
date <- seq(as.Date("2020-12-15"), as.Date("2022-01-15"), by="days")
dates <- data.frame(date)
# create moving 7 day avg for the negative cases and for the positive cases
dailynegativecases <- merge(dailynegativecases, dates, by.x = "datacolheita", by.y = "date", all.y = TRUE)
dailynegativecases$ntotal[is.na(dailynegativecases$ntotal)] <- 0
dailynegativecasests <- zoo(dailynegativecases$ntotal, order.by = dailynegativecases$datacolheita)
dailynegativecasesMA7days <- rollapply(dailynegativecasests, width=7, FUN=mean, align='center')
dailynegativecasests <- merge(dailynegativecasests, dailynegativecasesMA7days)
dailynegativecasests_df <- data.frame(dailynegativecasests)
dailynegativecasests_df <- tibble::rownames_to_column(dailynegativecasests_df, "date")
dailynegativecasests_df$date <- as.Date(dailynegativecasests_df$date)
names(dailynegativecasests_df) <- c("date","cases","MA7days")
dailynegativecasests_df$result <- " PCR negative cases"
dailypositivecases <- merge(dailypositivecases, dates, by.x = "datacolheita", by.y = "date", all.y = TRUE)
dailypositivecases$ntotal[is.na(dailypositivecases$ntotal)] <- 0
dailypositivecasests <- zoo(dailypositivecases$ntotal, order.by = dailypositivecases$datacolheita)
dailypositivecasesMA7days <- rollapply(dailypositivecasests, width=7, FUN=mean, align='center')
dailypositivecasests <- merge(dailypositivecasests, dailypositivecasesMA7days)
dailypositivecasests_df <- data.frame(dailypositivecasests)
dailypositivecasests_df <- tibble::rownames_to_column(dailypositivecasests_df, "date")
dailypositivecasests_df$date <- as.Date(dailypositivecasests_df$date)
names(dailypositivecasests_df) <- c("date","cases","MA7days")
dailypositivecasests_df$result <- "PCR confirmed cases"

# append both
possiblecasestsMA7days <- rbind(dailynegativecasests_df, dailypositivecasests_df)

#   merge(dailynegativecasests_df, dailypositivecasests_df, all = TRUE)
# possiblecasestsMA7days$variable[possiblecasestsMA7days$variable=="dailynegativecasesMA7days"] <- "Negative cases (7-day moving average)"
# possiblecasestsMA7days$variable[possiblecasestsMA7days$variable=="dailypositivecasesMA7days"] <- "Confirmed cases cases (7-day moving average)"

# plot possible cases by test status
ggplot2::theme_set(theme_classic(base_size = 18))
possiblecasescurve <- ggplot(possiblecasestsMA7days, aes(x=date, y=MA7days, fill=factor(result))) +
  geom_col()+
  labs(title="", x = "", y="Number of possible cases (7-day moving average)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_x_date(breaks = pretty_breaks(10))
possiblecasescurve

#### 5. CT in function of AGE and DELAY ####
possiblecases_bl$collectiondate <- possiblecases_bl$datacolheita.x
possiblecases_bl$collectiondate[is.na(possiblecases_bl$datacolheita.x)] <- possiblecases_bl$datacolheita.y[is.na(possiblecases_bl$datacolheita.x)]
possiblecases_bl$delay <- as.numeric(as.Date(possiblecases_bl$collectiondate)-as.Date(possiblecases_bl$data_inicio_sintomas))
possiblecases_bl$delay <- ifelse(possiblecases_bl$delay < 0, 0, possiblecases_bl$delay)

# summarize delay
possiblecases_bl %>% filter(!is.na(delay)) %>% summarise(median=median(delay),q25=quantile(delay,0.25),q75=quantile(delay,0.75))
# by age group
possiblecases_bl %>% filter(!is.na(delay)) %>% group_by(agegr) %>% summarise(median=median(delay),q25=quantile(delay,0.25),q75=quantile(delay,0.75))

# select only confirmed cases                            
confirmed <- possiblecases_bl %>% filter(testresult=="positive")

# CT values
confirmed$ct <- as.numeric(possiblecases_bl$valorct)
# summarize
confirmed %>% filter(!is.na(ct)) %>% summarise(median=median(ct),q25=quantile(ct,0.25),q75=quantile(ct,0.75))
confirmed %>% filter(!is.na(ct)) %>% group_by(agegr) %>% summarise(median=median(ct),q25=quantile(ct,0.25),q75=quantile(ct,0.75))

# scatter plot
scatterCT <- ggplot(confirmed, aes(x = delay, y = ct)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatter plot of ct value against delay",
       x = "Delay (days)", y = "Ct value") +
  xlim(0, 18) +
#  stat_smooth(method = "lm", se = TRUE, color = "darkred") 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE)
scatterCT
ggsave(plot = scatterCT,"scatterCT.jpg", width = 5, height = 4, dpi = 300)

# facetted by age
scatterCTage <- ggplot(confirmed, aes(x = delay, y = ct)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatter plot of ct value against delay",
       x = "Delay (days)", y = "Ct value") +
  xlim(0, 20) +
  facet_wrap(~ agegr, ncol = 1)
ggsave(plot = scatterCTage,"scatterCTage.jpg", width = 5, height = 10, dpi = 300)


#### 5. FACTORS ASSOCIATED WITH POSITIVE RESULT ####
# subset database to keep only those with results
testedpossiblecases <- possiblecases_bl %>% filter(testresult=="positive" | testresult=="negative")

# frequencies confirmed
table(testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$testresult))*100,2)

# results by age
table(testedpossiblecases$agegr, testedpossiblecases$testresult,useNA = "always")
round(prop.table(table(testedpossiblecases$agegr, testedpossiblecases$testresult),2)*100,1) # missings not included now

# results by sex
table(testedpossiblecases$GENDER, testedpossiblecases$testresult,useNA = "always")
round(prop.table(table(testedpossiblecases$GENDER, testedpossiblecases$testresult),2)*100,1) # missings not included now

# symptoms
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
round(prop.table(table(testedpossiblecases$anosmia, testedpossiblecases$testresult),2)*100,2) 
# ageusia
table(testedpossiblecases$ageusia, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$ageusia, testedpossiblecases$testresult),2)*100,) 
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

# HIV
table(testedpossiblecases$hiv, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$hiv, testedpossiblecases$testresult),2)*100,1) 
# hypertension
table(testedpossiblecases$hypertension, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$hypertension, testedpossiblecases$testresult),2)*100,1) 
# diabetes
table(testedpossiblecases$diabet, testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$diabet, testedpossiblecases$testresult),2)*100,1) 



# multivar to be able to control for age
# define variables of interest 
explanatory_vars <- c("fever", "throat", "rhinorrhea", "dyspnoea", "cough", "chest_pain", "vomit", "chills", "nausea", "diarrhea", "headache", "rash", "conjunctivitis", "myalgia", "arthromyalgia", "anosmia", "ageusia", "fatigue", "O2under95")
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
## add in age_category to the explanatory vars 
explanatory_vars <- c(explanatory_vars, "agegr")

## drop rows with missing information for variables of interest 

testedpossiblecases <- testedpossiblecases %>% 
  filter(!is.na(testresult)&!is.na(agegr)) # Many NA so will remove all of them

#testedpossiblecases <- testedpossiblecases %>% 
# drop_na(any_of(c("testresult", explanatory_vars))) # can't load package

# one by one symptom, adjusted for age group
# define which age group to make reference
testedpossiblecases <- testedpossiblecases %>% 
  mutate(agegr = fct_relevel(agegr, "18-49", after = 0)) 
# agegroups
table(testedpossiblecases$agegr, testedpossiblecases$testresult)
prop.table(table(testedpossiblecases$agegr, testedpossiblecases$testresult),2)

glm(testresult ~ agegr, family = "poisson", data = testedpossiblecases) %>% summary()

glm(testresult ~ agegr, family = "poisson", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# sex
testedpossiblecases <- testedpossiblecases %>% 
  mutate(GENDER = fct_relevel(GENDER, "M", after = 0)) 

glm(testresult ~ GENDER + agegr, family = "poisson", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# SES
table(testedpossiblecases$SesScoreQnt.y, useNA = "always")
testedpossiblecases$lowestSES <- 0
testedpossiblecases$lowestSES[testedpossiblecases$SesScoreQnt.y=="1. very low"] <- 1
table(testedpossiblecases$lowestSES, testedpossiblecases$testresult)
round(prop.table(table(testedpossiblecases$lowestSES, testedpossiblecases$testresult),2)*100,1)
glm(testresult ~ lowestSES + agegr, family = "poisson", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# fever
table(testedpossiblecases$fever, testedpossiblecases$testresult)
prop.table(table(testedpossiblecases$fever, testedpossiblecases$testresult),2)

glm(testresult ~ fever + agegr, family = "poisson", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# throat
glm(testresult ~ throat+ agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ rhinorrhea + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ dyspnoea + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ cough + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chest_pain + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ vomit + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ anosmia + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chills + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ nausea + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ diarrhea + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ arthromyalgia + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ myalgia + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ fatigue + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ O2under95 + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# HIV
# among possible cases
table(possiblecases_bl$hiv)
prop.table(table(possiblecases_bl$hiv))

# among confirmed cases
table(possiblecases_bl$hiv[possiblecases_bl$testresult=="positive"])
prop.table(table(possiblecases_bl$hiv[possiblecases_bl$testresult=="positive"]))

# adjusting for age
testedpossiblecases$hivbin[testedpossiblecases$hiv=="Crianca exposta"] <- 1
testedpossiblecases$hivbin[testedpossiblecases$hiv=="Seropositivo e em tratamento anti-retroviral"] <- 1
testedpossiblecases$hivbin[testedpossiblecases$hiv=="estado desconhecido"] <- 0
testedpossiblecases$hivbin[testedpossiblecases$hiv=="HIV negativo (no momento do ultimo teste HIV)"] <- 0

glm(testresult ~ hivbin + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# hypertension
# adjusting for age
testedpossiblecases$hypertensionbin[testedpossiblecases$hypertension=="Não diagnosticado "] <- 0
testedpossiblecases$hypertensionbin[testedpossiblecases$hypertension=="Diagnostico previo ou evento mas Não acompanhado"] <- 1
testedpossiblecases$hypertensionbin[testedpossiblecases$hypertension=="Diagnosticado, em seguimento mas SEM tratamento especifico"] <- 1
testedpossiblecases$hypertensionbin[testedpossiblecases$hypertension=="Diagnosticado, em acompanhamento com tratamento especifico"] <- 1
table(testedpossiblecases$hypertension, testedpossiblecases$hypertensionbin)
glm(testresult ~ hypertensionbin + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# diabetes
# adjusting for age
testedpossiblecases$diabetesbin[testedpossiblecases$diabet=="Não diagnosticado"] <- 0
testedpossiblecases$diabetesbin[testedpossiblecases$diabet=="Diagnosticado, em acompanhamento com tratamento especifico"] <- 1
table(testedpossiblecases$diabet, testedpossiblecases$diabetesbin)
glm(testresult ~ diabetesbin + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns


# wealth
# among all participants
table(participants$SesScoreQnt)
prop.table(table(participants$SesScoreQnt))

# among confirmed cases
table(possiblecases_bl$SesScoreQnt)
prop.table(table(possiblecases_bl$SesScoreQnt[possiblecases_bl$testresult=="positive"]))

# adjusting for age

glm(testresult ~ SesScoreQnt + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# education level
# among all participants
table(participants$ednivel_educacao)
prop.table(table(participants$ednivel_educacao))

# among confirmed cases
table(possiblecases_bl$ednivel_educacao)
prop.table(table(possiblecases_bl$ednivel_educacao[possiblecases_bl$testresult=="positive"]))


#### 6. INCIDENCE OF (symptomatic) COVID-19 ####
# we will estimate the incidence of symptomatic cases (of all possible cases and of PCR confirmed cases) as number of respiratory disease episodes 
# (=possible case) per person-year active surveillance -> obtain the denominator (person-years) from F2 -> number of biweekly visits corrected to years
# then select the possible cases which have been picked up during active surveillance visits
# Next, we will estimate age-specific incidence (in age groups)
