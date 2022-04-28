#############################################################################
# AFRICOVER                                                                 #
# Script to estimate disease incidence (cases from active surveillance)     #
#############################################################################

# test


# install/load packages
pacman::p_load(readxl,dplyr,lubridate, ggplot2, ggthemes, scales, usethis, zoo)

# import data
possiblecases_participants <- read.csv("possiblecases_participants.csv")
activesurveillancevisits <- read.csv()

# create a variable month
possiblecases_participants$month <- format(as.Date(possiblecases_participants$datacolheita.x, "%Y-%m-%d"), "%Y-%m")
table(possiblecases_participants$month)

#### 1. DESCRIPTION OF POSSIBLE CASES ####
# describe frequencies
# results
table(possiblecases_participants$Resultado)
round(prop.table(table(possiblecases_participants$Resultado))*100,2)

# results by age
table(possiblecases_participants$Resultado, possiblecases_participants$agegr)
round(prop.table(table( possiblecases_participants$agegr, possiblecases_participants$Resultado),1)*100,2)

# results by month
table(possiblecases_participants$month, possiblecases_participants$agegr)

# histogram possible case number
monthlypossiblecases <- possiblecases_participants %>%
  filter(Resultado!="Nao identificado nas duas bases") %>%
  group_by(month, agegr, Resultado) %>%
  summarise(n=n())
ggplot2::theme_set(theme_classic(base_size = 18))
monthlypossiblecasesplot <- ggplot(monthlypossiblecases, aes(x=month, y=n, fill=factor(Resultado))) +
  geom_col() +
  labs(title="", x = "", y="Number of possible cases") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) 
monthlypossiblecasesplot

# using a moving average over 7 days
# summarize daily possible case number
possiblecases_participants$datacolheita <- as.Date(possiblecases_participants$datacolheita.x)
dailynegativecases <- possiblecases_participants %>%
  filter(Resultado=="Negativo") %>%
  group_by(datacolheita) %>%
  summarise(ntotal=n())
dailypositivecases <- possiblecases_participants %>%
  filter(Resultado!="Positivo") %>%
  group_by(datacolheita) %>%
  summarise(ntotal=n())
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

#### 2. INCIDENCE OF (symptomatic) COVID-19 ####
# we will estimate the incidence of symptomatic cases (of all possible cases and of PCR confirmed cases) as number of respiratory disease episodes 
# (=possible case) per person-year active surveillance -> obtain the denominator (person-years) from F2 -> number of biweekly visits corrected to years
# then select the possible cases which have been picked up during active surveillance visits
# Next, we will estimate age-specific incidence (in age groups)
