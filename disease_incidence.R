#############################################################################
# AFRICOVER                                                                 #
# Script to estimate disease incidence (cases from active surveillance)     #
#############################################################################

# install/load packages
pacman::p_load(readxl,lubridate, ggplot2, ggmap, survival, flextable, janitor, knitr, httr, lmtest, scales, usethis, tidyverse, stringr, purrr, gtsummary, broom, lmtest, parameters, see)

# import data of possible cases
possiblecases <- read_excel("possiblecases.xlsx")
# import data of possible cases with baseline
cases_participants <- read_excel("cases_participants.xlsx")
# import data of participants with COVID-19 confirmations
confirmedcases_firstonly_participants<- read_excel("confirmedcases_firstonly_participants.xlsx")
# import all participant data
participants <- read.csv("participants.csv")
# import household visit data
FU <- read.csv("FU.csv")
# import geo coordinates of households
visit_geompoints <- read.csv("visit_geompoints.csv")

# combine confirmedcases_firstonly_participants and FU
# add the time under follow-up to make a denominator person-months
FUbyHH <- FU %>% group_by(locationId) %>% summarise(nvisits=n())
sum(FUbyHH$nvisits)
cases_participantsFU <- merge(confirmedcases_firstonly_participants, FUbyHH, by.x = "locationid", by.y = "locationId", all.x = T)
cases_participantsFU$time <- cases_participantsFU$nvisits/2 # number of person-months followed up
# STILL HAVE TO REMOVE THE PM AFTER THE EVENT OCCURRED!!
# remove those participants that haven't been followed-up
cases_participantsFU <- cases_participantsFU %>% filter(!is.na(pm))
# check and remove duplicated rows 
dups = which(duplicated(cases_participantsFU%>%select('individualid','testresult')))
cases_participantsFU <- cases_participantsFU %>% filter(!row.names(cases_participantsFU) %in% dups)

# make factors of explanatory variables
cases_participantsFU$overweight[!is.na(cases_participantsFU$BMI)] <- "normal"
cases_participantsFU$overweight[cases_participantsFU$BMI>24.99] <- "overweight"
cases_participantsFU$overweight[cases_participantsFU$BMI>29.99] <- "obesity"
cases_participantsFU$overweight[cases_participantsFU$BMI<19] <- "underweight"
cases_participantsFU$overweight <- factor(cases_participantsFU$overweight)
cases_participantsFU$education[cases_participantsFU$ednivel_educacao=="1.  Primario incompleto"] <- "none completed"
cases_participantsFU$education[cases_participantsFU$ednivel_educacao=="8. Nenhum"] <- "none completed"
cases_participantsFU$education[cases_participantsFU$ednivel_educacao=="2. Primario completo"] <- "primary"
cases_participantsFU$education[cases_participantsFU$ednivel_educacao=="7. Pos-graduacao"] <- "higher"
cases_participantsFU$education[cases_participantsFU$ednivel_educacao=="6. Superior"] <- "higher"
cases_participantsFU$education[cases_participantsFU$ednivel_educacao=="5. Tecnico-profissional"] <- "higher"
cases_participantsFU$education[grepl("cundario incom", cases_participantsFU$ednivel_educacao)==T] <- "primary"
cases_participantsFU$education[cases_participantsFU$ednivel_educacao=="4. Secundario completo"] <- "secondary"
cases_participantsFU$education <- factor(cases_participantsFU$education)
cases_participantsFU$hypertensionbin[cases_participantsFU$hypertension=="Não diagnosticado "] <- 0
cases_participantsFU$hypertensionbin[cases_participantsFU$hypertension=="Diagnostico previo ou evento mas Não acompanhado"] <- 1
cases_participantsFU$hypertensionbin[cases_participantsFU$hypertension=="Diagnosticado, em seguimento mas SEM tratamento especifico"] <- 1
cases_participantsFU$hypertensionbin[cases_participantsFU$hypertension=="Diagnosticado, em acompanhamento com tratamento especifico"] <- 1
cases_participantsFU$diabetesbin[cases_participantsFU$diabet=="Não diagnosticado"] <- 0
cases_participantsFU$diabetesbin[cases_participantsFU$diabet=="Diagnosticado, em acompanhamento com tratamento especifico"] <- 1
cases_participantsFU$lowestSES[!is.na(cases_participantsFU$SesScoreQnt=="1. very low")] <- 0
cases_participantsFU$lowestSES[cases_participantsFU$SesScoreQnt=="1. very low"] <- 1
cases_participantsFU$hivbin[cases_participantsFU$hiv=="Crianca exposta"] <- 1
cases_participantsFU$hivbin[cases_participantsFU$hiv=="Seropositivo e em tratamento anti-retroviral"] <- 1
cases_participantsFU$hivbin[cases_participantsFU$hiv=="estado desconhecido"] <- 0
cases_participantsFU$hivbin[cases_participantsFU$hiv=="HIV negativo (no momento do ultimo teste HIV)"] <- 0
cases_participantsFU$agegr <- factor(cases_participantsFU$agegr)
cases_participantsFU$sex <- factor(cases_participantsFU$GENDER)

# change category to use as reference
cases_participantsFU <- cases_participantsFU %>% 
  mutate(agegr = fct_relevel(agegr, "0-17", after = 0)) 
cases_participantsFU <- cases_participantsFU %>% 
  mutate(sex = fct_relevel(sex, "M", after = 0)) 

# smaller age groups
breaks <- c(seq(0, 60, by = 10), 70, Inf)
# Create factor variable 'agegr'
cases_participantsFU$agegr10 <- cut(cases_participantsFU$age, breaks = breaks, labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"), right = FALSE)
cases_participantsFU$agegr10 <- factor(cases_participantsFU$agegr10)
table(cases_participantsFU$agegr10)

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
maputo_bbox <- c(left = 32.59, bottom = -25.95, right = 32.625, top = -25.923)
# Get the map using the Stamen source
# maputo <- get_openstreetmap(bbox = maputo_bbox) # doesn't work for now
# maputo <- get_stamenmap(bbox = maputo_bbox, maptype = "toner-lite", zoom = 16) 
maputo <- get_stamenmap(bbox = maputo_bbox, maptype = "terrain", zoom = 15)
# api_key <- "HZlfIpAihcDRbZ95DIvK9g"
# # Get the map using Here Maps
# maputo <- get_heremap(app_id = NULL, app_code = NULL, api_key = api_key, 
#                       zoom = 14, bbox = maputo_bbox, maptype = "normal.day") # doesn't find the function for some reason

# plot the map of HH visits using ggmap and add the point layer
dottedmapvisits <- ggmap(maputo) + 
  geom_point(data = visit_geompoints, aes(x = longitude, y = latitude, color = n),
             alpha = 0.3) +
  scale_color_gradient(low = "#ADD8E6", high = "#000080") +
  guides(size = FALSE, shape = FALSE) +
  labs(color = "Number of \nhousehold visits") +
  theme(legend.title = element_text(size = 9),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))
dottedmapvisits
ggsave("dottedmapvisits.jpg", dottedmapvisits, dpi = 300, width = 6.5, height = 5)

# plot the map of HH visits using ggmap and add the point layer
dottedmapcases <- ggmap(maputo) + 
  geom_point(data = cases_geompoints, aes(x = longitude, y = latitude, color = testresult),
             alpha = 0.3) +
  guides(size = FALSE, shape = FALSE) +
  labs(color = "Possible cases \ntest result") +
  theme(legend.title = element_text(size = 9),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) 
dottedmapcases
ggsave("dottedmapcases.jpg", dottedmapcases, dpi = 300, width = 6.5, height = 5)

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
count(cases_participantsFU)
table(cases_participantsFU$agegr, useNA = "always")
nHH <- cases_participantsFU %>%
  group_by(locationid) %>%
  summarise(n=n())
count(nHH) # 1489
mean(nHH$n) # mean HH size = 4.03

# mean age participants
cases_participantsFU %>%
  filter(!is.na(age)) %>%
  summarise(median=median(age),q25=quantile(age,0.25), q75=quantile(age,0.75))

# sex distribution
table(cases_participantsFU$sex)
round(prop.table(table(cases_participantsFU$sex))*100,1)

# hypertension distribution
table(cases_participantsFU$hypertensionbin, useNA = "always")
round(prop.table(table(cases_participantsFU$hypertensionbin))*100,1)

# hypertension distribution
table(cases_participantsFU$diabetesbin, useNA = "always")
round(prop.table(table(cases_participantsFU$diabetesbin))*100,1)

# hiv distribution
table(cases_participantsFU$hivbin, useNA = "always")
round(prop.table(table(cases_participantsFU$hivbin))*100,1)

# SES distribution
table(cases_participantsFU$SesScoreQnt, useNA = "always")
round(prop.table(table(cases_participantsFU$SesScoreQnt))*100,1)

# education distribution
table(cases_participantsFU$education, useNA = "always")
round(prop.table(table(cases_participantsFU$education))*100,1)
6190-1349

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
  facet_wrap(~ possibleconfirmed, scales = "free_y", dir = "v") +
  labs(title = "Age Distribution among participants, \npossible and confirmed COVID-19 cases",
       x = "Age", y = "Count")
print(agehistogram)
ggsave(plot = agehistogram,"agehistogram.jpg", width = 5, height = 5, dpi = 300)

#### 3. DESCRIPTION OF POSSIBLE CASES ####
# number of possible cases, in number of households, & number of cases tested
cases_participants %>% group_by(visitid) %>% dplyr::summarise(n=n())
761-134

# mean age possible cases, by test result
cases_participants %>%
  filter(!is.na(age)) %>%
  group_by(testresult) %>%
  summarise(median=median(age),q25=quantile(age,0.25), q75=quantile(age,0.75))

# age & sex distribution of possible cases
table(cases_participants$agegr)
prop.table(table(cases_participants$agegr))

table(cases_participants$sexo)
prop.table(table(cases_participants$sexo))

# symptoms and onset
table(cases_participants$symptoms)
# fever
table(cases_participants$fever)
prop.table(table(cases_participants$fever))
# anosmia
table(cases_participants$anosmia)
prop.table(table(cases_participants$anosmia))
# aguesia
table(cases_participants$ageusia)
prop.table(table(cases_participants$ageusia))
# at least one resp symptom
cases_participants$atleastonesympt <- 0
cases_participants$atleastonesympt[cases_participants$throat=="Sim"] <- 1
cases_participants$atleastonesympt[cases_participants$cough=="Sim"] <- 1
cases_participants$atleastonesympt[cases_participants$dyspnoea=="Sim"] <- 1
cases_participants$atleastonesympt[cases_participants$rhinorrhea=="Sim"] <- 1
cases_participants$atleastonesympt[cases_participants$throat=="Sim"] <- 1
cases_participants$atleastonesympt[cases_participants$O2under95=="yes"] <- 1
table(cases_participants$atleastone)
prop.table(table(cases_participants$atleastone))

# comorbidities
table(participants$hiv, useNA = "always")
(410+25+15)/6006

# delay between onset and nasal swab
date_heita = as.Date(cases_participants$datacolheita[!is.na(cases_participants$datacolheita)&!is.na(cases_participants$data_inicio_sintomas)], format = "%Y-%m-%d") 
date_sinto = as.Date(cases_participants$data_inicio_sintomas[!is.na(cases_participants$datacolheita)&!is.na(cases_participants$data_inicio_sintomas)], format = "%Y-%m-%d") 

cases_participants$delay[!is.na(cases_participants$datacolheita)&!is.na(cases_participants$data_inicio_sintomas)] <- date_heita-date_sinto
mean(cases_participants$delay[!is.na(cases_participants$delay)])
median(cases_participants$delay[!is.na(cases_participants$delay)])
quantile(cases_participants$delay[!is.na(cases_participants$delay)],0.25)
quantile(cases_participants$delay[!is.na(cases_participants$delay)],0.50)
quantile(cases_participants$delay[!is.na(cases_participants$delay)],0.75)

# describe frequencies
# results
table(cases_participants$testresult, useNA = "always")
round(prop.table(table(cases_participants$testresult))*100,2)

# histogram possible case number
monthlypossiblecases <- cases_participants %>%
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
table(cases_participants$month)
table(cases_participants$month, cases_participants$testresult)
prop.table(table(cases_participants$month, cases_participants$testresult),1)*100
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
cases_participants$collectiondate <- cases_participants$datacolheita.x
cases_participants$collectiondate[is.na(cases_participants$datacolheita.x)] <- cases_participants$datacolheita.y[is.na(cases_participants$datacolheita.x)]
cases_participants$delay <- as.numeric(as.Date(cases_participants$collectiondate)-as.Date(cases_participants$data_inicio_sintomas))
cases_participants$delay <- ifelse(cases_participants$delay < 0, 0, cases_participants$delay)

# summarize delay
cases_participants %>% filter(!is.na(delay)) %>% summarise(median=median(delay),q25=quantile(delay,0.25),q75=quantile(delay,0.75))
# by age group
cases_participants %>% filter(!is.na(delay)) %>% group_by(agegr) %>% summarise(median=median(delay),q25=quantile(delay,0.25),q75=quantile(delay,0.75))

# select only confirmed cases                            
confirmed <- cases_participants %>% filter(testresult=="positive")

# CT values
confirmed$ct <- as.numeric(cases_participants$valorct)
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

#### 6. FACTORS ASSOCIATED WITH COVID-19 ####
# dataframe confirmedcases_firstonly_participants has baseline and demographic data of all participants, plus indicated those who were COVID-19 positive
# generate a variable 'event' so say who had the event
cases_participantsFU$event <- 0
cases_participantsFU$event[cases_participantsFU$testresult=="positive"] <- 1
cases_participantsFU$eventf <- factor(cases_participantsFU$event)
table(cases_participantsFU$eventf)

# univar table
age <- cases_participantsFU %>%
  group_by(agegr10) %>%
  summarise(total=n(), pcttotal=(round(n()/count(cases_participantsFU)*100,1)), covid19=sum(event), pctcases=round(sum(event)/sum(cases_participantsFU$event)*100,1))
sex <- cases_participantsFU %>%
  group_by(sex) %>%
  summarise(total=n(), pcttotal=(round(n()/count(cases_participantsFU)*100,1)), covid19=sum(event), pctcases=round(sum(event)/sum(cases_participantsFU$event)*100,1))
table(cases_participantsFU$lowestSES, useNA = "always")
SES <- cases_participantsFU %>%
  filter(!is.na(lowestSES)) %>%
  group_by(lowestSES) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$lowestSES))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$lowestSES)])) * 100
  )
education <- cases_participantsFU %>%
  filter(!is.na(education)) %>%
  group_by(education) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$education))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$education)])) * 100
  )
# cox regression calculating hazard ratios
# Fit Cox regression models for each variable
explanatory_variables <- c("agegr10", "education", "overweight", "lowestSES", "hivbin", "hypertensionbin", "diabetesbin")

for (variable in explanatory_variables) {
  # Create a formula including the variable, age, and sex
  formula <- as.formula(paste("Surv(time, event) ~ ", variable, "+ age + sex"))
  
  # Fit the Cox regression model
  cox_model <- coxph(formula, data = cases_participantsFU)
  
  # Extract hazard ratios and their confidence intervals
  hr_ci <- exp(coef(cox_model))
  ci <- exp(confint(cox_model))
  
  # Print the results
  cat("Variable:", variable, "\n")
  cat("Hazard Ratios:\n")
  print(hr_ci)
  cat("Confidence Intervals:\n")
  print(ci)
  cat("\n")
}


#### 5. SYMPTOMS ASSOCIATED WITH COVID-19 AMONG ARI CASES ####
# subset database to keep only those with results
testedpossiblecases <- cases_participants %>% filter(testresult=="positive" | testresult=="negative")

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
table(cases_participants$hiv)
prop.table(table(cases_participants$hiv))

# among confirmed cases
table(cases_participants$hiv[cases_participants$testresult=="positive"])
prop.table(table(cases_participants$hiv[cases_participants$testresult=="positive"]))

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
table(cases_participants$SesScoreQnt)
prop.table(table(cases_participants$SesScoreQnt[cases_participants$testresult=="positive"]))

# adjusting for age

glm(testresult ~ SesScoreQnt + agegr, family = "poisson", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# education level
# among all participants
table(participants$ednivel_educacao)
prop.table(table(participants$ednivel_educacao))

# among confirmed cases
table(cases_participants$ednivel_educacao)
prop.table(table(cases_participants$ednivel_educacao[cases_participants$testresult=="positive"]))


#### 6. INCIDENCE OF (symptomatic) COVID-19 ####
# we will estimate the incidence of symptomatic cases (of all possible cases and of PCR confirmed cases) as number of respiratory disease episodes 
# (=possible case) per person-year active surveillance -> obtain the denominator (person-years) from F2 -> number of biweekly visits corrected to years
# then select the possible cases which have been picked up during active surveillance visits
# Next, we will estimate age-specific incidence (in age groups)
