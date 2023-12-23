#############################################################################
# Population-based COVID-19 surveillance, Maputo, Moz, Dec2020-Mar2022      #
# Active acute respiratory illness surveillance                             #
# Script to estimate (symptomatic) COVID-19 incidence, COVID-19 risk, and   #
# symptoms associated with COVID-19                                         #
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
cases_participantsFU <- read.csv("cases_participantsFU.csv")
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

# number of individuals with at least one episode
individualswithepisode <- possiblecases %>% group_by(individualid) %>% summarise(n())
count(individualswithepisode)

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
# univar table
age <- cases_participantsFU %>%
  group_by(agegr10) %>%
  summarise(total=n(), pcttotal=(round(n()/count(cases_participantsFU)*100,1)), covid19=sum(event), pctcases=round(sum(event)/sum(cases_participantsFU$event)*100,1))
sex <- cases_participantsFU %>%
  group_by(sex) %>%
  summarise(total=n(), pcttotal=(round(n()/count(cases_participantsFU)*100,1)), covid19=sum(event), pctcases=round(sum(event)/sum(cases_participantsFU$event)*100,1))
table(cases_participantsFU$lowestSES, useNA = "always")
SES <- cases_participantsFU %>%
  filter(!is.na(SesScoreQnt)) %>%
  group_by(SesScoreQnt) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / sum(!is.na(cases_participantsFU$SesScoreQnt)))*100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$SesScoreQnt)]))*100,1)
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
hiv <- cases_participantsFU %>%
  filter(!is.na(hivbin)) %>%
  group_by(hivbin) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$hivbin))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$hivbin)])) * 100
  )
hypertension <- cases_participantsFU %>%
  filter(!is.na(hypertensionbin)) %>%
  group_by(hypertensionbin) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$hypertensionbin))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$hypertensionbin)])) * 100
  )
diabetes <- cases_participantsFU %>%
  filter(!is.na(diabetesbin)) %>%
  group_by(diabetesbin) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$diabetesbin))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$diabetesbin)])) * 100
  )
overweight <- cases_participantsFU %>%
  filter(!is.na(overweight)) %>%
  group_by(overweight) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$overweight))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$overweight)])) * 100
  )

publictransport_pastweek <- cases_participantsFU %>%
  filter(!is.na(publictransport_pastweek)) %>%
  group_by(publictransport_pastweek) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$publictransport_pastweek))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$publictransport_pastweek)])) * 100
  )

smokingbin <- cases_participantsFU %>%
  filter(!is.na(smokingbin)) %>%
  group_by(smokingbin) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$smokingbin))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$smokingbin)])) * 100
  )
pregnant <- cases_participantsFU %>%
  filter(!is.na(pregnant)) %>%
  group_by(pregnant) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$pregnant))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$pregnant)])) * 100
  )
tb <- cases_participantsFU %>%
  filter(!is.na(tbbin)) %>%
  group_by(tbbin) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$tbbin))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$tbbin)])) * 100
  )
lungdisease <- cases_participantsFU %>%
  filter(!is.na(lungdisease)) %>%
  group_by(lungdisease) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$lungdisease))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$lungdisease)])) * 100
  )
heart <- cases_participantsFU %>%
  filter(!is.na(heart)) %>%
  group_by(heart) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$heart))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$heart)])) * 100
  )
health_worker <- cases_participantsFU %>%
  filter(!is.na(health_worker)) %>%
  group_by(health_worker) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$health_worker))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$health_worker)])) * 100
  )
bedroom_sharing <- cases_participantsFU %>%
  filter(!is.na(bedroom_sharing)) %>%
  group_by(bedroom_sharing) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$bedroom_sharing))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$bedroom_sharing)])) * 100
  )
toylet_sharing <- cases_participantsFU %>%
  filter(!is.na(toylet_sharing)) %>%
  group_by(toylet_sharing) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$toylet_sharing))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$toylet_sharing)])) * 100
  )
handwash <- cases_participantsFU %>%
  filter(!is.na(handwash)) %>%
  group_by(handwash) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$handwash))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$handwash)])) * 100
  )
water_availability <- cases_participantsFU %>%
  filter(!is.na(water_availability)) %>%
  group_by(water_availability) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$water_availability))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$water_availability)])) * 100
  )
soap_availability <- cases_participantsFU %>%
  filter(!is.na(soap_availability)) %>%
  group_by(soap_availability) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$soap_availability))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$soap_availability)])) * 100
  )


# cox regression calculating hazard ratios
# sex and age
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ age + sex + SesScoreQnt, data = cases_participantsFU)

# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
sexageresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                           CI_Lower = rounded_conf_intervals[, 1],
                           CI_Upper = rounded_conf_intervals[, 2])
print(sexageresult)

# with age groups
# Fit the Cox regression model with agegr10 as a categorical variable - SesScoreQnt taken out for 
cox_model <- coxph(Surv(time, event) ~ agegr10 + sex, data = cases_participantsFU)

# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
sexagegrresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                             CI_Lower = rounded_conf_intervals[, 1],
                             CI_Upper = rounded_conf_intervals[, 2])
print(sexagegrresult)

# fit model for each variable, adjusting for age and sex
explanatory_variables <- c("education", "lowestSES", "SesScoreQnt", "smokingbin", "publictransport_pastweek", 
                           "bedroom_sharing", "toylet_sharing", "handwash", "water_availability", "soap_availability", "health_worker")

for (variable in explanatory_variables) {
  # Create a formula including the variable, age, and sex
  formula <- as.formula(paste("Surv(time, event) ~ ", variable, "+ age + sex"))
  
  # Fit the Cox regression model
  cox_model <- coxph(formula, data = cases_participantsFU)
  
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
# comorbidities "hivbin", "tbbin", "hypertensionbin", "diabetesbin", "heart", "lungdisease", "pregnant"
explanatory_variables <- c("hivbin", "tbbin", "hypertensionbin", "diabetesbin", "heart", "lungdisease", "leukemia", "pregnant")

for (variable in explanatory_variables) {
  # Create a formula including the variable, age, and sex
  formula <- as.formula(paste("Surv(time, event) ~ ", variable, "+ age + sex"))
  
  # Fit the Cox regression model
  cox_model <- coxph(formula, data = cases_participantsFU)
  
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
cases_participantsFU_ad <- cases_participantsFU %>% filter(age>15.99)
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ age + sex + overweight, data = cases_participantsFU_ad)

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

# SES
# Change the factor level and set "3. med." as the reference level
cases_participantsFU <- cases_participantsFU %>%
  mutate(SesScoreQnt = relevel(SesScoreQnt, ref = "3. med."))
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ age + sex + SesScoreQnt, data = cases_participantsFU)
# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
SES <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                               CI_Lower = rounded_conf_intervals[, 1],
                               CI_Upper = rounded_conf_intervals[, 2])
print(SES)

# lowest SES
# Change the factor level and set "3. med." as the reference level
cases_participantsFU <- cases_participantsFU %>%
  mutate(lowestSES = relevel(lowestSES, ref = 0))
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ age + sex + lowestSES, data = cases_participantsFU)
# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
SES <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                  CI_Lower = rounded_conf_intervals[, 1],
                  CI_Upper = rounded_conf_intervals[, 2])
print(SES)
# education
# Change the factor level and set "3. med." as the reference level
cases_participantsFU <- cases_participantsFU %>%
  mutate(education = relevel(education, ref = "primary"))
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ age + sex + education, data = cases_participantsFU)
# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
education <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                  CI_Lower = rounded_conf_intervals[, 1],
                  CI_Upper = rounded_conf_intervals[, 2])
print(education)

# public transport  
cases_participantsFU_nosharedtaxi <- cases_participantsFU %>% filter(publictransport_pastweek!="moto taxi/shared taxi")
# Change the factor level and set "3. med." as the reference level
cases_participantsFU <- cases_participantsFU %>%
  mutate(publictransport_pastweek = relevel(publictransport_pastweek, ref = "none"))
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ age + sex + publictransport_pastweek, data = cases_participantsFU)
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


#### 7. INCIDENCE OF (symptomatic) COVID-19 ####
# we estimate the incidence of COVID-19 and of respiratory symptom episodes (=possible case) per person-year active surveillance, then monthly, then by age group
# 7.1 yearly COVID-19 incidence rate all ages
sum(cases_participantsFU$event)
sum(cases_participantsFU$time)
personyears_followedup <- sum(cases_participantsFU$time)/12
personyears_followedup
# n events per 1000 inhabitants per month
sum(cases_participantsFU$event)/sum(cases_participantsFU$time)
# n events per 1000 inhabitants per year
yearlyincidencerate <- sum(cases_participantsFU$event)/sum(cases_participantsFU$time)*12
yearlyincidencerate
# 95%CI
standard_error <- sqrt(yearlyincidencerate * (1 - yearlyincidencerate) / personyears_followedup)
z <- qnorm(0.975)  # 0.975 corresponds to (1 - 0.05/2) for a two-tailed test
margin_of_error <- z * standard_error
lower_bound <- yearlyincidencerate - margin_of_error
upper_bound <- yearlyincidencerate + margin_of_error
lower_bound
upper_bound

# 7.2 yearly respiratory illness incidence rate all ages
nrow(possiblecases)
# n events per 1000 inhabitants per year
yearlyARIincidencerate <- nrow(possiblecases)/sum(cases_participantsFU$time)*12
yearlyARIincidencerate
# 95%CI
standard_error <- sqrt(yearlyARIincidencerate * (1 - yearlyARIincidencerate) / personyears_followedup)
z <- qnorm(0.975)  # 0.975 corresponds to (1 - 0.05/2) for a two-tailed test
margin_of_error <- z * standard_error
lower_bound <- yearlyARIincidencerate - margin_of_error
upper_bound <- yearlyARIincidencerate + margin_of_error
lower_bound
upper_bound


# 7.2 yearly incidence rate by age group
table(cases_participantsFU$agegr, useNA = "always")
# under 18
cases_participantsFU_u18 <- cases_participantsFU %>% filter(agegr=="0-17")
# personyears followed up
personyears_followedup_u18 <- sum(cases_participantsFU_u18$time)/12
personyears_followedup_u18
# n events per 1000 inhabitants per year
yearlyincidencerate_u18 <- sum(cases_participantsFU_u18$event)/personyears_followedup_u18
yearlyincidencerate_u18
# 95%CI
standard_error <- sqrt(yearlyincidencerate_u18 * (1 - yearlyincidencerate_u18) / personyears_followedup_u18)
z <- qnorm(0.975)  # 0.975 corresponds to (1 - 0.05/2) for a two-tailed test
margin_of_error <- z * standard_error
lower_bound_u18 <- yearlyincidencerate_u18 - margin_of_error
upper_bound_u18 <- yearlyincidencerate_u18 + margin_of_error
lower_bound_u18
upper_bound_u18

# 18-49
cases_participantsFU_18to49 <- cases_participantsFU %>% filter(agegr=="18-49")
# personyears followed up
personyears_followedup_18to49 <- sum(cases_participantsFU_18to49$time)/12
personyears_followedup_18to49
# n events per 1000 inhabitants per year
yearlyincidencerate_18to49 <- sum(cases_participantsFU_18to49$event)/personyears_followedup_18to49
yearlyincidencerate_18to49
# 95%CI
standard_error <- sqrt(yearlyincidencerate_18to49 * (1 - yearlyincidencerate_18to49) / personyears_followedup_18to49)
z <- qnorm(0.975)  # 0.975 corresponds to (1 - 0.05/2) for a two-tailed test
margin_of_error <- z * standard_error
lower_bound_18to49 <- yearlyincidencerate_18to49 - margin_of_error
upper_bound_18to49 <- yearlyincidencerate_18to49 + margin_of_error
lower_bound_18to49
upper_bound_18to49

# 50 or more
cases_participantsFU_over50 <- cases_participantsFU %>% filter(agegr=="50+")
# personyears followed up
personyears_followedup_over50 <- sum(cases_participantsFU_over50$time)/12
personyears_followedup_over50
# n events per 1000 inhabitants per year
yearlyincidencerate_over50 <- sum(cases_participantsFU_over50$event)/personyears_followedup_over50
yearlyincidencerate_over50
# 95%CI
standard_error <- sqrt(yearlyincidencerate_over50 * (1 - yearlyincidencerate_over50) / personyears_followedup_over50)
z <- qnorm(0.975)  # 0.975 corresponds to (1 - 0.05/2) for a two-tailed test
margin_of_error <- z * standard_error
lower_bound_over50 <- yearlyincidencerate_over50 - margin_of_error
upper_bound_over50 <- yearlyincidencerate_over50 + margin_of_error
lower_bound_over50
upper_bound_over50

#### 8. SYMPTOMS ASSOCIATED WITH COVID-19 AMONG ARI CASES ####
# subset database to keep only those with results
testedpossiblecases <- possiblecases %>% filter(testresult=="positive" | testresult=="negative")

# add age
demographics <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/africover git/demographics.txt", sep="")
testedpossiblecases <- merge(testedpossiblecases, demographics, by.x = "individualid", by.y = "openhdsindividualId", all.x = T)
testedpossiblecases$age <- round((as.numeric(as.Date("2021-06-15")) - as.numeric(as.Date(testedpossiblecases$dob)))/365.25,0)
testedpossiblecases$age[testedpossiblecases$age<0] <- 0
# Create factor variable 'agegr'
testedpossiblecases$agegr10 <- cut(testedpossiblecases$age, breaks = breaks, labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"), right = FALSE)
testedpossiblecases$agegr10 <- factor(testedpossiblecases$agegr10)
table(testedpossiblecases$agegr10)

# frequencies confirmed
table(testedpossiblecases$testresult, useNA = "always")
round(prop.table(table(testedpossiblecases$testresult))*100,1)

# age confirmed vs negative
testedpossiblecases %>%
  filter(!is.na(age)) %>%
  group_by(testresult) %>%
  summarise(median=median(age),q25=quantile(age,0.25), q75=quantile(age,0.75))
table(testedpossiblecases$agegr10, testedpossiblecases$testresult,useNA = "always")
round(prop.table(table(testedpossiblecases$agegr10, testedpossiblecases$testresult),2)*100,1) # missings not included now

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

# multivar to be able to control for age
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
  mutate(agegr = fct_relevel(agegr10, "0-9", after = 0)) 
# agegroups
table(testedpossiblecases$agegr10, testedpossiblecases$testresult)
prop.table(table(testedpossiblecases$agegr10, testedpossiblecases$testresult),2)

glm(testresult ~ agegr10, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# sex
testedpossiblecases <- testedpossiblecases %>% 
  mutate(GENDER = fct_relevel(GENDER, "M", after = 0)) 
glm(testresult ~ GENDER, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns
glm(testresult ~ GENDER + age, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# SES
table(testedpossiblecases$SesScoreQnt.y, useNA = "always")
testedpossiblecases$lowestSES <- 0
testedpossiblecases$lowestSES[testedpossiblecases$SesScoreQnt.y=="1. very low"] <- 1
table(testedpossiblecases$lowestSES, testedpossiblecases$testresult)
round(prop.table(table(testedpossiblecases$lowestSES, testedpossiblecases$testresult),2)*100,1)
glm(testresult ~ lowestSES + age, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# fever
table(testedpossiblecases$fever, testedpossiblecases$testresult)
prop.table(table(testedpossiblecases$fever, testedpossiblecases$testresult),2)
glm(testresult ~ fever, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ fever + age, family = "binomial", data = testedpossiblecases) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# throat
glm(testresult ~ throat, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ throat+ age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ rhinorrhea, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ rhinorrhea + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ dyspnoea, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ dyspnoea + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ cough, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ cough + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chest_pain, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chest_pain + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ vomit, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ vomit + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ anosmia, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ anosmia + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chills, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chills + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ nausea , family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ nausea + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ diarrhea, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ diarrhea + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ arthromyalgia, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ arthromyalgia + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ myalgia, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ myalgia + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ fatigue, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ fatigue + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ O2under95, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ O2under95 + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ appitite_loss, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ appitite_loss + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ rash, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ rash + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ headache, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ headache + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ nosebleed, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ nosebleed + age, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# compare the association with anosmia and ageusia during alpha vs. delta vs. omicron peaks
testedpossiblecases$variant[testedpossiblecases$datacolheita<"2021-06-15"] <- "alpha/beta"
testedpossiblecases$variant[testedpossiblecases$datacolheita<"2021-11-15"&testedpossiblecases$datacolheita>"2021-06-14"] <- "delta"
testedpossiblecases$variant[testedpossiblecases$datacolheita>"2021-11-14"] <- "omicron"
testedpossiblecases$variant <- factor(testedpossiblecases$variant, levels = c("alpha/beta", "delta", "omicron"))
table(testedpossiblecases$variant, useNA = "always")
                                                
glm(testresult ~ anosmia + age + variant, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + age + variant, family = "binomial", data = testedpossiblecases) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# only omicron -> check whether symptoms associated to COVID-19 above remain associated
testedpossiblecases_omicron <- testedpossiblecases %>% filter(testedpossiblecases$datacolheita>"2021-11-14")

glm(testresult ~ anosmia + age, family = "binomial", data = testedpossiblecases_omicron) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + age, family = "binomial", data = testedpossiblecases_omicron) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ appitite_loss + age, family = "binomial", data = testedpossiblecases_omicron) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ chills + age, family = "binomial", data = testedpossiblecases_omicron) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# before omicron -> check association without omicron cases
testedpossiblecases_alphabetadelta <- testedpossiblecases %>% filter(testedpossiblecases$datacolheita<"2021-11-15")

glm(testresult ~ anosmia + age, family = "binomial", data = testedpossiblecases_alphabetadelta) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + age, family = "binomial", data = testedpossiblecases_alphabetadelta) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

# delta only
testedpossiblecases_delta <- testedpossiblecases %>% filter(testedpossiblecases$datacolheita<"2021-11-15"&testedpossiblecases$datacolheita>"2021-06-14")

glm(testresult ~ anosmia + age, family = "binomial", data = testedpossiblecases_delta) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

glm(testresult ~ ageusia + age, family = "binomial", data = testedpossiblecases_delta) %>%    
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs   
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns
