#############################################################################
# Population-based COVID-19 surveillance, Maputo, Moz, Dec2020-Mar2022      #
# Active acute respiratory illness surveillance                             #
# Script to estimate (symptomatic) COVID-19 incidence and analyse factors   #
# associated with COVID-19 (symptomatic and SARS-CoV-2 confirmed)           #
#############################################################################

# install/load packages
pacman::p_load(readxl,lubridate, ggplot2, ggmap, survival, flextable, janitor, knitr, httr, lmtest, scales, usethis, tidyverse, stringr, purrr, gtsummary, broom, lmtest, parameters, see)

# import data of possible cases
possiblecases <- read.csv("possiblecases_pseudo.csv")
possiblecases$week <- as.Date(possiblecases$week)

# import all participant data: baseline, follow-up visits and recorded possible cases in survival format (event, time)
cases_participantsFU <- read.csv("cases_participantsFU_pseudo.csv")
# reformat factor variables
cases_participantsFU$agegr10 <- factor(cases_participantsFU$agegr10)
cases_participantsFU$sex <- factor(cases_participantsFU$sex)
cases_participantsFU <- cases_participantsFU %>% 
  mutate(sex = fct_relevel(sex, "M", after = 0)) 
cases_participantsFU$SesScoreQnt <- factor(cases_participantsFU$SesScoreQnt)
cases_participantsFU <- cases_participantsFU %>% 
  mutate(SesScoreQnt = fct_relevel(SesScoreQnt, "5. highest", after = 0)) 
cases_participantsFU$education <- factor(cases_participantsFU$education)
cases_participantsFU <- cases_participantsFU %>%
  mutate(education = relevel(education, ref = "secondary"))
cases_participantsFU$leukemia <- factor(cases_participantsFU$leukemia)
cases_participantsFU <- cases_participantsFU %>%
  mutate(leukemia = relevel(leukemia, ref = "not"))
cases_participantsFU$publictransport_pastweek <- factor(cases_participantsFU$publictransport_pastweek)
cases_participantsFU <- cases_participantsFU %>%
  mutate(publictransport_pastweek = relevel(publictransport_pastweek, ref = "none"))
cases_participantsFU$lungdisease <- factor(cases_participantsFU$lungdisease)
cases_participantsFU <- cases_participantsFU %>%
  mutate(lungdisease = relevel(lungdisease, ref = "not"))
cases_participantsFU$heart <- factor(cases_participantsFU$heart)
cases_participantsFU <- cases_participantsFU %>%
  mutate(heart = relevel(heart, ref = "not"))
cases_participantsFU$tbbin <- factor(cases_participantsFU$tbbin)
cases_participantsFU <- cases_participantsFU %>%
  mutate(tbbin = relevel(tbbin, ref = "not"))
cases_participantsFU$health_worker <- factor(cases_participantsFU$health_worker)
cases_participantsFU$hivbin <- factor(cases_participantsFU$hivbin)

# import household visit data (not available in open data to ensure no identification of participants) 
FU <- read.csv("FU.csv")
FU$contact_date <- as.Date(FU$contact_date)

# import geo coordinates of households (not available in open data to ensure no identification of participants) 
visit_geompoints <- read.csv("visit_geompoints.csv")

#### 1. HOUSEHOLD VISITS & EPICURVE (this section has no open data to ensure no identification of participants) ####
# number of visits
str(FU$dateHHID)

# number of visits during which at least one household member had respiratory symptoms
table(FU$respiratory_symptoms, useNA = "always")

# map of households visited, with colour depending on the frequency of visits
# set the map extent
maputo_bbox <- c(left = 32.59, bottom = -25.95, right = 32.625, top = -25.923)
# Get the map using the Stamen source
# register_stadiamaps(key = "my API key")
# maputo <- get_openstreetmap(bbox = maputo_bbox) # doesn't work for now
# maputo <- get_stamenmap(bbox = maputo_bbox, maptype = "toner-lite", zoom = 16) 
maputo <- get_stamenmap(bbox = maputo_bbox, maptype = "terrain", zoom = 15)
maputo_toner <- get_stadiamap(bbox = maputo_bbox, maptype = "stamen_toner_lite", zoom = 15,   messaging = FALSE)
# maputo_terrain <- get_stadiamap(bbox = maputo_bbox, maptype = "stamen_terrain_lite", zoom = 15,   messaging = FALSE)


# plot the map of HH visits using ggmap and add the point layer
dottedmapvisits <- ggmap(maputo_toner) + 
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
ggsave("dottedmapvisits.tiff", device = "tiff", dottedmapvisits, dpi = 300, width = 6.5, height = 5)

# plot the map of HH visits using ggmap and add the point layer
dottedmapcases <- ggmap(maputo_toner) + 
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
ggsave("dottedmapcases.tiff", device = "tiff", dottedmapcases, dpi = 300, width = 6.5, height = 5)

# plot of weekly visits
FU_weekly <- FU %>%
  mutate(week = as.Date(cut(contact_date, breaks = "1 week")))

histogramvisits <- ggplot(FU_weekly, aes(x = week, fill = respiratory_symptoms)) +
  geom_histogram(binwidth = 7, color = "black", position = "stack") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", 
               expand = c(0, 0)) +
  ggtitle("Household Visits") +
  labs(fill = "At least one household member with respiratory symptoms") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.background = element_rect(fill = "white")) +
  theme(legend.position = "bottom") +
  ylab("Frequency")
histogramvisits
ggsave(plot = histogramvisits,"histogramvisits.jpg", width = 10, height = 5, dpi = 300)
ggsave(plot = histogramvisits,"histogramvisits.tiff", device = "tiff", width = 10, height = 5, dpi = 260)

# epicurve of possible and confirmed cases
# make sure there's always a test result
possiblecases$testresult[is.na(possiblecases$testresult)] <- "not tested"
# Assuming testresult is the factor variable in your data frame possiblecases
possiblecases$testresult <- factor(possiblecases$testresult, 
                                   levels = c("not tested", "negative", "positive"))

#histogram
histogramcases <- ggplot(possiblecases, aes(x = week, fill = testresult)) +
  geom_histogram(binwidth = 7, color = "black", position = "stack") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", 
               expand = c(0, 0)) +
  ggtitle("Number of reported/confirmed COVID-19 cases") +
  xlab(NULL) +  
  scale_fill_manual(values = c("grey", "#F8766D", "#00BFC4")) +  
  labs(fill = "Nasal swab PCR test result") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.background = element_rect(fill = "white")) +
  theme(legend.position = "bottom") +
  ylab("Frequency")
histogramcases
ggsave(plot = histogramcases,"histogramcases.jpg", width = 10, height = 5, dpi = 300)
ggsave(plot = histogramcases,"histogramcases.tiff", device = "tiff", width = 10, height = 5, dpi = 260)


# monthly n of cases & positivity
table(possiblecases$testresult)
cases_monthly <- possiblecases %>%
  filter(testresult != "not tested") %>%
  group_by(monthonset) %>%
  summarize(ntested = n(), pct_positive = mean(testresult == "positive") * 100)
cases_monthly


#### 2. DESCRIPTION OF PARTICIPANTS ####
# describe number of participants and HHs
count(cases_participantsFU)
table(cases_participantsFU$agegr10, useNA = "always")
nHH <- cases_participantsFU %>%
  group_by(locationid) %>%
  summarise(n=n())
count(nHH) # 1489
mean(nHH$n) # mean HH size = 4.03

# mean age participants - age as a continuous variable not available in the open data
cases_participantsFU %>%
  filter(!is.na(age)) %>%
  summarise(median=median(age),q25=quantile(age,0.25), q75=quantile(age,0.75))

# age group distribution
table(cases_participantsFU$agegr10)
round(prop.table(table(cases_participantsFU$agegr10))*100,1)

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
# dataframe cases_participantsFU has baseline and demographic data of all participants, plus indicated those who were COVID-19 positive
#### 6.1 univar table ####
age <- cases_participantsFU %>%
  group_by(agegr10) %>%
  summarise(total=n(), 
            pcttotal = round((n() / sum(!is.na(cases_participantsFU$agegr10)))*100,1),
            covid19=sum(event), 
            pctcases=round(sum(event)/sum(cases_participantsFU$event)*100,1))
sex <- cases_participantsFU %>%
  group_by(sex) %>%
  summarise(total=n(), 
            pcttotal = round((n() / sum(!is.na(cases_participantsFU$sex)))*100,1),
            covid19=sum(event), 
            pctcases=round(sum(event)/sum(cases_participantsFU$event)*100,1))
SES <- cases_participantsFU %>%
  filter(agegr!="0-17 years") %>%
  filter(!is.na(SesScoreQnt)) %>%
  group_by(SesScoreQnt) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / count(cases_participantsFU[!is.na(cases_participantsFU$SesScoreQnt) & cases_participantsFU$agegr!="0-17 years", ]))*100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$SesScoreQnt) & cases_participantsFU$agegr!="0-17 years"]))*100,1)
  )
education <- cases_participantsFU %>%
  filter(agegr!="0-17 years") %>%
  filter(!is.na(education)) %>%
  group_by(education) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / count(cases_participantsFU[!is.na(cases_participantsFU$education) & cases_participantsFU$agegr!="0-17 years", ]))*100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$education) & cases_participantsFU$agegr!="0-17 years"]))*100,1)
  )
health_worker <- cases_participantsFU %>%
  filter(agegr!="0-17 years") %>%
  filter(!is.na(health_worker)) %>%
  group_by(health_worker) %>%
  summarise(
    total = n(),
    pcttotal = round((n() / count(cases_participantsFU[!is.na(cases_participantsFU$health_worker) & cases_participantsFU$agegr!="0-17 years", ]))*100,1),
    covid19 = sum(event),
    pctcases = round((sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$health_worker) & cases_participantsFU$agegr!="0-17 years"]))*100,1)
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
leukemia <- cases_participantsFU %>%
  filter(!is.na(leukemia)) %>%
  group_by(leukemia) %>%
  summarise(
    total = n(),
    pcttotal = (n() / sum(!is.na(cases_participantsFU$leukemia))) * 100,
    covid19 = sum(event),
    pctcases = (sum(event) / sum(cases_participantsFU$event[!is.na(cases_participantsFU$leukemia)])) * 100
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
# append all small tables
# rename first variable in each dataframe to have the same name
names(age)[1] <- "category"
names(sex)[1] <- "category"
names(SES)[1] <- "category"
names(education)[1] <- "category"
names(health_worker)[1] <- "category"
names(hiv)[1] <- "category"
names(tb)[1] <- "category"
names(hypertension)[1] <- "category"
names(diabetes)[1] <- "category"
names(lungdisease)[1] <- "category"
names(heart)[1] <- "category"
names(leukemia)[1] <- "category"
names(overweight)[1] <- "category"
names(smokingbin)[1] <- "category"
names(publictransport_pastweek)[1] <- "category"
names(bedroom_sharing)[1] <- "category"
names(toylet_sharing)[1] <- "category"
names(handwash)[1] <- "category"
names(water_availability)[1] <- "category"

table2a <- rbind(age, sex, SES, education)
table2b <- rbind(health_worker, hiv, tb, hypertension)
table2c <- rbind(diabetes, lungdisease, heart, leukemia, overweight, smokingbin, publictransport_pastweek, bedroom_sharing, toylet_sharing, handwash, water_availability)

#### 6.2 cox regression calculating hazard ratios ####
# sex and age
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ agegr10 + sex, data = cases_participantsFU)

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
# Fit the Cox regression model with agegr10 as a categorical variable
cox_model <- coxph(Surv(time, event) ~ agegr10 + sex, data = cases_participantsFU)

# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
sexagegrresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                             CI_Lower = round(rounded_conf_intervals[, 1],2),
                             CI_Upper = round(rounded_conf_intervals[, 2],2))
sexagegrresult$Combined_CI <- paste(sexagegrresult$CI_Lower, sexagegrresult$CI_Upper, sep = "-")
sexagegrresult <- sexagegrresult %>% select(-CI_Lower, -CI_Upper)
print(sexagegrresult)

# those only relevant for participanrs over 18 years old: SES, education level, HCW
# SES
cases_participantsFUad <- cases_participantsFU %>% filter(agegr!="0-17 years")
cox_model <- coxph(Surv(time, event) ~ agegr10 + SesScoreQnt, data = cases_participantsFUad)
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)
SESagegrresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                             CI_Lower = round(rounded_conf_intervals[, 1],2),
                             CI_Upper = round(rounded_conf_intervals[, 2],2))
SESagegrresult$Combined_CI <- paste(SESagegrresult$CI_Lower, SESagegrresult$CI_Upper, sep = "-")
SESagegrresult <- SESagegrresult %>% select(-CI_Lower, -CI_Upper)
print(SESagegrresult)
# education level
cox_model <- coxph(Surv(time, event) ~ agegr10 + education, data = cases_participantsFUad)
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)
edagegrresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                             CI_Lower = round(rounded_conf_intervals[, 1],2),
                             CI_Upper = round(rounded_conf_intervals[, 2],2))
edagegrresult$Combined_CI <- paste(edagegrresult$CI_Lower, edagegrresult$CI_Upper, sep = "-")
edagegrresult <- edagegrresult %>% select(-CI_Lower, -CI_Upper)
print(edagegrresult)
# hcw
cox_model <- coxph(Surv(time, event) ~ agegr10 + health_worker, data = cases_participantsFUad)
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)
hcwagegrresult <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                            CI_Lower = round(rounded_conf_intervals[, 1],2),
                            CI_Upper = round(rounded_conf_intervals[, 2],2))
hcwagegrresult$Combined_CI <- paste(hcwagegrresult$CI_Lower, hcwagegrresult$CI_Upper, sep = "-")
hcwagegrresult <- hcwagegrresult %>% select(-CI_Lower, -CI_Upper)
print(hcwagegrresult)


# fit model for each variable, adjusting for age and sex
# reformat dichotomous variables to 0/1 
cases_participantsFU <- cases_participantsFU %>% mutate(across(                                      
    .cols = all_of(c("toylet_sharing", "water_availability", "soap_availability", "health_worker", "smokingbin")),  ## for each column listed and "outcome"
    .fns = ~case_when(                              
      . %in% c("Sim", "yes", "positive", "(ex-)smoker")   ~ 1,           ## recode male, yes and death to 1
      . %in% c("Não", "Não sabe", "no",  "negative", "non smoker") ~ 0,           ## female, no and recover to 0
      TRUE                            ~ NA_real_)    ## otherwise set to missing
  )
  )
# first group of participant characteristics
explanatory_variables <- c("smokingbin", "publictransport_pastweek", 
                           "bedroom_sharing", "toylet_sharing", "handwash", "water_availability", "soap_availability")

for (variable in explanatory_variables) {
  # Create a formula including the variable, age, and sex
  formula <- as.formula(paste("Surv(time, event) ~ ", variable, "+ agegr10 + sex"))
  
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
  formula <- as.formula(paste("Surv(time, event) ~ ", variable, "+ agegr10 + sex"))
  
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
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ agegr10 + sex + SesScoreQnt, data = cases_participantsFU)
# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)

# Combine hazard ratios and confidence intervals into a data frame
SES <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                  CI_Lower = round(rounded_conf_intervals[, 1],2),
                  CI_Upper = round(rounded_conf_intervals[, 2],2))
SES$Combined_CI <- paste(SES$CI_Lower, SES$CI_Upper, sep = "-")
SES <- SES %>% select(-CI_Lower, -CI_Upper)

print(SES)

# education - exclude <18year olds
cases_participantsFU_adults <- cases_participantsFU %>% filter(agegr10!="0-9"&agegr10!="10-19")
cases_participantsFU_adults <- cases_participantsFU_adults %>% 
  mutate(agegr10 = fct_relevel(agegr10, "20-29", after = 0)) 
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ education + sex + agegr10, data = cases_participantsFU_adults)
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

# HCW
# Fit the Cox regression model
cox_model <- coxph(Surv(time, event) ~ agegr10 + sex + health_worker, data = cases_participantsFU)
# Extract hazard ratios and confidence intervals
hazard_ratios <- exp(coef(cox_model))
conf_intervals <- exp(confint(cox_model))
rounded_hazard_ratios <- round(hazard_ratios, 2)
rounded_conf_intervals <- round(conf_intervals, 2)
hcw <- data.frame(Hazard_Ratio = rounded_hazard_ratios,
                                       CI_Lower = rounded_conf_intervals[, 1],
                                       CI_Upper = rounded_conf_intervals[, 2])
print(hcw)

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

