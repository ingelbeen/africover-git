###############################################################################################################
# AFRICOVER                                                                                                   #
# Use of nasal swabs in active Covid-19 surveillance with household visits                                    #
# Diagnostic performance in function of age, clinical signs/symptoms & time symptom onset to sample collection#
###############################################################################################################
# update XX/XX/2022


# install/load packages
pacman::p_load(readxl,dplyr,lubridate, ggplot2, ggthemes, scales, usethis, zoo)


# import data
possiblecases_participants <- read.csv("possiblecases_participants.csv")

# For DESCRIPTION OF POSSIBLE CASES, see script disease_incidence.R

#### 1. COMPARISON OF CONFIRMED AND NEGATIVE CASES ####
# demographics + clinical signs and symptoms


#### 2. TIME BETWEEN SYMPTOM ONSET AND SAMPLE COLLECTION ####
# comparison of distribution by PCR result and by age

