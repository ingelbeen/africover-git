#############################################################################
# AFRICOVER                                                                 #
# Script to estimate disease incidence (cases from active surveillance)     #
#############################################################################

# install/load packages
pacman::p_load(readxl,dplyr,lubridate, ggplot2, usethis)

# import data
possiblecases_participants <- read.csv("possiblecases_participants.csv")
activesurveillancevisits <- read.csv()

# create a variable month
possiblecases_participants$month <- format(as.Date(possiblecases_participants$datacolheita.x, "%Y-%m-%d"), "%Y-%m")
table(possiblecases_participants$month)

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

# incidence -> obtain from F2 the denominator = person-months under active surveillance -> number of biweekly visits corrected to month
