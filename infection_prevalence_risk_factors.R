#############################################################################
# AFRICOVER                                                                 #
# Prevalence and risk factors for SARS-CoV-2 infection                      #
#############################################################################

# install/load packages
pacman::p_load(readxl,dplyr,lubridate, ggplot2, ggthemes, scales, usethis, zoo)

# function (table and attack rate functions)
big.table <- function(data, useNA = "no") { 
  count <- table(data, useNA = useNA) 
  prop <- round(prop.table(count)*100, digits = 2) 
  cumulative <- cumsum(prop) 
  rbind(count, 
        prop, 
        cumulative) 
} 
# Function to provide counts, denominator and proportions (equivalent of attack rate) 
attack.rate <- function(table) { 
  prop <- round(prop.table(table,1),digits = 2) 
  denominator <- rowSums(table) 
  output <- cbind(Ill = table[,2], N = denominator, Proportions = prop[,2]) 
  return(output) 
}

# import data
DBS <- read.csv("DBS.csv")
participants_simplified <- read.csv("participants_simplified.csv")

#### 1. CHARACTERISTICS SERO-SURVEY PARTICIPANTS ####
# frequency table of agegroups
agegroup_table <- table(participants_simplified$agegr)
round(prop.table(agegroup_table),2)*100

# frequency table of sex
sex_table <- table(participants_simplified$sex)
prop.table(sex_table)

#### 2. SERO-PREVALENCE ####


#### 3. RISK FACTORS OF INFECTION UP TO 30 June ####
# limit to M0 and M3 rounds (which have complete data) -> removing samples collected after 30 June, or which were the third or fourth consecutive sample of a patient
DBS6mo <- subset(DBS, datacolheita < "2021-06-30" & grepl("- 3",sampleID)==F & grepl("- 4",sampleID)==F) 
# make a dataset with sero-status per participant
DBS6mo$resultbin[DBS6mo$Result=="neg"] <- 0
DBS6mo$resultbin[DBS6mo$Result=="uncertain"] <- 0
DBS6mo$resultbin[DBS6mo$Result=="undetermined"] <- 0
DBS6mo$resultbin[DBS6mo$Result=="pos"] <- 1
infection_by_participant <- DBS6mo %>%
  filter(!is.na(Result)) %>%
  group_by(participantID) %>%
  summarise(DBSsamplescollected = n(), positiveserologies = sum(resultbin))
infection_by_participant$infected <- "no"
infection_by_participant$infected[infection_by_participant$positiveserologies>0] <- "yes"
infection_by_participant

# link datasets 
infectiondb <- merge(infection_by_participant, participants_simplified, by.x = "participantID", by.y = "openhdsindividualId", all.x = T)

# UNIVARIABLE ANALYSIS
# generate an empty list to save all the output
output <- list()
# list all exposure vars
exposures <- c("agegr","sex","ecestado_civil","trabtrabalho","SesScoreQnt","empsituacao_emprego","ednivel_educacao","drugshiv","drugscancer","drugsdiabet","drugshypertension","drugschronic_kidney_disease","drugschronic_lung_disease")
#loop
for (var in exposures) {
  total <- big.table(infectiondb[,var])
  output[[var]] <- total
}
# table for outcome
big.table(infectiondb$infected)

# MULTIVARIABLE ANALYSIS using negative binomial regression
