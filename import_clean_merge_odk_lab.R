#############################################################################
# AFRICOVER                                                                 #
# Script to import, clean and merge ODK, HDSS demographic, and lab databases#
#############################################################################
# contributors: Catildo, Nilzio, Brecht
# date last update: 2023-12-06

# install/load packages
pacman::p_load(readxl,excel.link,dplyr,lubridate,ggplot2,usethis, writexl, tidyverse)

#### 1. DEMOGRAPHIC DATA ####
# full cohort databases - not used as many mismatches
# # individuals
# individuals <- xl.read.file("database/final/individuals.xlsx", password = "africover_1")
# cohort <- merge(individuals, F1b, by.x = "ID", by.y = "individualId")
# # SES
# SES <- xl.read.file("database/final/HDSS_StatusSocioEconomic_2018_full_DB.xlsx", password = "africover_1")

# Import demographic & SES data
demographicsSES <- read_excel("database/Demographics_database_unlocked.xlsx", sheet = "Sheet1") # no duplicates (checked)
demographicsSES$dob <- as.Date(demographicsSES$DOB, "%m/%d/%Y")
agesexsubset <- subset(demographicsSES, select = c("openhdsindividualId","dob", "GENDER"))
agesexsubset <- subset(agesexsubset, !is.na(dob))

# another database for those without SES data
baseline_demographics <- read_excel("database/Baseline_demographics.xlsx")
baseline_demographics$openhdsindividualId <- baseline_demographics$`individualInfo:individualId`
baseline_demographics$dob <- as.Date(baseline_demographics$`individualInfo:dateOfBirth`)
baseline_demographics$GENDER <- baseline_demographics$`individualInfo:gender` 
baseline_demographics <- baseline_demographics %>%
  filter(!is.na(dob)) %>%
  select(openhdsindividualId, dob, GENDER)

# for some age and sex were missing, which have been manually looked up
missing_age_sex <- read_excel("database/20220323/missing age_sex.xlsx", 
                              col_types = c("text", "text", "text", 
                                            "date", "numeric"))
colnames(missing_age_sex) <- c("openhdsindividualId","HHId","GENDER","dob","age")
missing_age_sex <- missing_age_sex %>% filter(!is.na(dob)) %>% select(openhdsindividualId,GENDER,dob)

# append all three
demographics <- rbind(agesexsubset,baseline_demographics)
demographics <- rbind(demographics,missing_age_sex)

# Remove duplicated rows
# full duplicates (same participant ID and DOB)
dups <- which(duplicated(demographics%>%select(openhdsindividualId, dob))) # CHECK - same participantID, different dob
demographics <- demographics %>% filter(!row.names(demographics) %in% dups)
# duplicate participant ID but different dob
dups <- which(duplicated(demographics%>%select(openhdsindividualId))) # CHECK - same participantID, different dob
dupsindemographics <- demographics %>% filter(row.names(demographics) %in% dups) # make a list of all duplicates to manually check
# remove one by one duplicate participants which are also Africover participants
demographics <- subset(demographics, dob!="1995-02-12"|openhdsindividualId!="QU1CS3012007")
# QU5NM4008009 is duplicate but no study participant
demographics <- subset(demographics, dob!="1993-11-15"|openhdsindividualId!="QU7MU1029003")
demographics <- subset(demographics, dob!="1958-06-17"|openhdsindividualId!="QUF000001001")
demographics <- subset(demographics, dob!="1973-11-10"|openhdsindividualId!="QUF000001002")
demographics <- subset(demographics, dob!="1994-10-11"|openhdsindividualId!="QUF000001003")
demographics <- subset(demographics, dob!="1994-10-11"|openhdsindividualId!="QUF000001004")
demographics <- subset(demographics, dob!="2009-07-12"|openhdsindividualId!="QUF000001005")
demographics <- subset(demographics, dob!="2012-08-06"|openhdsindividualId!="QUF000001006")
demographics <- subset(demographics, dob!="2002-01-03"|openhdsindividualId!="QUFNM1004007")
demographics <- subset(demographics, dob!="2013-04-13"|openhdsindividualId!="QULNM4002007")


# add SES for those for whom available
SES <- subset(demographicsSES, select = c("openhdsindividualId","ednivel_educacao", "SesScoreQnt"))
demographics <- merge(demographics, SES, by = "openhdsindividualId", all.x = T)

# export demographic db
write.table(demographics, 'demographics.txt')

#### 2. BASELINE AFRICOVER DATA ####
# F1 baseline
# household data
F1a <- xl.read.file("database/20221123/Africover F1a HH Households_full_DB.xlsx", password = "africover_1")

# remove entries without consent
F1a <- F1a %>%
  filter(is.na(is_consent_signed)|is_consent_signed=="Sim")
# check duplicated rows
dups = which(duplicated(F1a%>%select(openhdslocationId))) # a single duplicate with just one answer different - keep the first
length(dups) # HH QU7GJ1008 has some different values while same person interviewed twice
# Remove duplicated rows
F1a <- F1a %>% filter(!row.names(F1a) %in% dups) 

F1a_v1 <- read_excel("database/20220609cleaned/AfriCoVER_F1a_Base_Agregado_familiar_v1.0.xls")
F1a_v1$motive <- NA
F1a_v1$especify_motive <- NA
F1a_v1$is_consent_signed <- NA
names(F1a_v1) <- tolower(names(F1a_v1))
F1a_v2 <- read_excel("database/20220609cleaned/AfriCoVER_F1a_Base_Agregado_familiar_v2_0.xls")
names(F1a_v2) <- tolower(names(F1a_v2))
F1a_appended <- rbind(F1a_v1, F1a_v2)
dups = which(duplicated(F1a_appended%>%select(openhdslocationid))) # a single duplicate with just one answer different - keep the first
length(dups)
F1a_appended = F1a_appended %>% filter(!row.names(F1a_appended) %in% dups) 
F1a_mismatch = merge(F1a_appended, F1a, by.x = "openhdslocationid", by.y = "openhdslocationId", all = T) 
F1a_mismatch <- F1a_mismatch %>%
  filter(is.na(F1a_mismatch$start.x)|is.na(F1a_mismatch$start.y))
# F1a$date_enrolled <- as.Date(F1a$start,"%m/%d/%Y") # 1733 missing
# table(F1a$date_enrolled, useNA = "always")

# individual participant data 
F1b <- xl.read.file("database/20221123/Africover F1b Individuals_full_DB.xlsx", password = "africover_1")
# remove empty rows
F1b <- F1b %>%
  filter(!is.na(individualid))
# remove entries without consent
F1b <- F1b %>%
  filter(is.na(is_consent_signed)|is_consent_signed=="Sim")
# check duplicated participant IDs
dups = which(duplicated(F1b%>%select(individualid)))
length(dups) # no full duplicates
# list of duplicates to check 
# QU2PC1009007 -> probably twice same person; remove 1st, uuid:bb9d8c2c-f4b0-4ef6-a8c2-6dac0fba3d8c)
F1b <- F1b %>% filter(Key!="uuid:bb9d8c2c-f4b0-4ef6-a8c2-6dac0fba3d8c")
# QUGAM2002013 -> probably twice same person: remove 2nd
F1b <- F1b %>% filter(Key!="uuid:f96a4cb2-2967-4050-80f8-bf9120730e30")
# QUHCS1011003 -> probably twice same person: remove 2nd
F1b <- F1b %>% filter(Key!="uuid:8e6e8f1c-6efe-466b-af16-9b1d435509e9")
# QUHNM1026004
F1b$individualid[F1b$Key=="uuid:be75f15f-fc07-4cf4-9796-2b8eef688d5b"] <- "QUHNM1026005"
# QULAM2008003 -> with this household, I don't quite know what to do. could we check them still?
# QULAM2008004
# QULAM2008005
# QULAM2008006
# QULAM2008007
# QULAM2008008
# QULAM2036002 -> probably twice same person, but the second time (when also including other HH members) indicated to be HIV+
F1b <- F1b %>% filter(Key!="uuid:26d0031c-5d65-4924-829b-2cc1893be27e")
# QULNM3022012 -> twice exactly the same values
F1b <- F1b %>% filter(Key!="uuid:d08b8aa6-7b4c-4430-a66f-239919d368a6")
# QUNMU1001003 -> unlikely for this age (35 to have hypertension) while older man was missed
F1b$individualid[F1b$Key=="uuid:b15a4fa8-c106-4df3-92b7-aad747012e63"] <- "QUNMU1001001"
# check for duplicate participant IDs without all other variables
F1b <- F1b %>% filter(!row.names(F1b) %in% dups) 

# add participants of which serosurvey done but with the individual baseline data is missing
serosurvey_missingbl <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/africover git/serosurvey_missingbl.csv")
serosurvey_missingbl <- serosurvey_missingbl %>% select(participantID)
F1b <- merge(F1b, serosurvey_missingbl, by.x = "individualid", by.y = "participantID", all = T)
F1b$locationid[is.na(F1b$locationid)] <- substr(F1b$individualid[is.na(F1b$locationid)], 1, 9)

# remove unnecessary vars
F1a <- F1a %>% select(-c("start", "openhdsvisitId", "openhdsfieldWorkerId", "openhdsindividualId", "is_consent_signed", "motive","especify_motive", "key"))
F1b <- F1b %>% select(-c("visitid", "fieldworkerid", "is_consent_signed", "motive", "especify_motive", "Key", "processedbymirth","instanceid"))  

# merge F1a & F1b
F1 <- merge(F1b, F1a, by.x = "locationid", by.y = "openhdslocationId", all = T)


# check HH with no individual (F1b) data -> 431 households without any individual member?
F1awithoutF1b <- F1 %>%
  filter(is.na(mass_bus)) %>%
  select(locationid)
write.table(F1awithoutF1b, "F1awithoutF1b.txt")

# check individual with no HH data -> 35 members for which no HH data
F1bwithoutF1a <- F1 %>%
  filter(is.na(bedroom_sharing)) %>%
  select(individualid)
write.table(F1bwithoutF1a, "F1bwithoutF1a.txt")

# merge with demographic data
participants <- merge(demographics, F1, by.x = "openhdsindividualId", by.y = "individualid", all = T)

# remove lines without participant data (F1a but no F1b)
participants <- participants %>% filter(!is.na(openhdsindividualId))
str(participants)

# check those for which no demographic or SES data
missingdemographicdata <- participants %>%
  filter(is.na(dob)) %>%
  select(openhdsindividualId) # shared with Alberto to check STILL ADD THE MISSING AGES AND SEX
missingSESdata <- participants %>%
  filter(is.na(SesScoreQnt)) %>%
  select(openhdsindividualId)
write.table(missingSESdata, "missingSESdata.txt") # shared with Alberto to check

# add age groups
participants$age <- round(as.numeric(as.Date("2021-06-15") - participants$dob)/365.25,0)
participants$age[participants$age<0] <- 0
agestocheck <- participants %>%
  filter(age>90 | age<0) %>%
  select(dob, age, openhdsindividualId)
write.table(agestocheck,"agestocheck.txt") # still 6 over 100 yo, but one 101 (plausible) and 5 121 (missing)
participants$age[participants$age==121] <- NA # 121 because dob entered was 01-01-1900
table(participants$agegr, useNA = "always")
table(participants$sex, useNA = "always")

# list of those without age nor sex
missingage_sex <- participants %>%
  filter(is.na(GENDER)) %>%
  select(openhdsindividualId)
write.table(missingage_sex, "missingage_sex.txt")

# F3 weight & height
F3 <- xl.read.file("database/20221123/Africover F3 Physical measurements_full_DB.xlsx", password = "africover_1")
# remove obervations without collected weight, height, or armcircumference
F3 <- F3 %>% filter(was_height_measured=="Sim"|was_weighed=="Sim"|was_arm_circumference=="Sim")

# check if duplicated rows 
dups = which(duplicated(F3%>%select(individualid, height, arm_circumference, weight)))
length(dups)
# Remove duplicated rows
F3 <- F3 %>% filter(!row.names(F3) %in% dups)

# make a dataset that combines observations of weight, height and MUAC during different visits (if weight is collected during a different visit than height)
F3_weightheightcombined <- F3 %>%
  group_by(individualid) %>%
  summarise(weight=mean(weight), height=mean(height), MUAC=mean(arm_circumference))
hist(F3_weightheightcombined$weight)
hist(F3_weightheightcombined$height)
# BMI
F3_weightheightcombined$BMI <- round(F3_weightheightcombined$weight/((F3_weightheightcombined$height/100)^2),1)
hist(F3_weightheightcombined$BMI)

# merge participant baseline data and weight and height
participants <- merge(participants, F3_weightheightcombined, by.x = "openhdsindividualId", by.y = "individualid", all.x = T) # only 3684 out of 6807
# identify those with weight and height that doesn't make sense
list_weight_height_problems <- participants %>%
  filter((BMI<10|BMI>35) & age>4) %>%
  select(openhdsindividualId, GENDER, age, weight, height, BMI, MUAC)
write.table(list_weight_height_problems, file = "list_weight_height_problems.txt")

# add age groups
participants$agegr[participants$age<18] <- "0-17"
participants$agegr[participants$age>17&participants$age<50] <- "18-49"
participants$agegr[participants$age>49] <- "50+"

# save file
write.csv(participants, "participants.csv")
participantssimpl <- participants %>% select(locationid, openhdsindividualId)
write.csv(participantssimpl, "participantssimpl.csv")

# import updates of comorbidities (F7), such as new diagnoses of HIV, chronic conditions, etc., once we have a version with factor variables as strings
F7 <- xl.read.file("database/20221123/Africover F7 Comorbidities_full_DB.xlsx", password = "africover_1")
# check which serosurvey participants had no baseline completed but recorded at a later time
serosurvey_missingbl <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/africover git/serosurvey_missingbl.csv")
serosurvey_missingbl <- serosurvey_missingbl %>% select(participantID)
serosurvey_missingbl$missingbl <- 1
F7 <- merge(F7, serosurvey_missingbl, by.x = "individualid", by.y = "participantID", all.x = T)
sum(F7$missingbl[!is.na(F7$missingbl)])

#### 3. ACTIVE SURVEILLANCE FOR POSSIBLE CASES ####
## 3.1 ODK possible case reports
# F5_v1 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F5_Possivel_caso_v1_0.csv")
# F5_v2 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F5_Possivel_caso_v2_0.csv")
# F5 <- rbind(F5_v1, F5_v2)
F5 <- xl.read.file("database/20221123/Africover F5 Possible case_full_DB.xlsx", password = "africover_1")
# check duplicated rows
dups = which(duplicated(F5%>%select('individualid', 'symptoms_start_date', 'nasal_swab_date')))
length(dups)
# Remove duplicated rows
F5 = F5 %>% filter(!row.names(F5) %in% dups)

# remove observations of persons not recorded at baseline as participant
F5 <- F5 %>% filter(individualid!="QU3PC1009001")
F5 <- F5 %>% filter(individualid!="QUHNM1005005")
F5 <- F5 %>% filter(start!="2021-05-07 12:19:00") # recorded twice, with this record less complete
F5 <- F5 %>% filter(start!="2021-02-11 12:36:00") # recorded twice, with this record less complete

# create var for ID and date to link to results
F5$datacolheita <- as.Date(F5$nasal_swab_date) # 64 no sample collection date, in most cases because tested at hospital
table(F5$`nasal_swab_data-specify_reason_for_not_sampling`)

# corrections after checks from lab results (e.g., date missing in F5 but entered in result db) 
F5$datacolheita[F5$individualid=="QU7CS3036001"] <- "2022-02-17"
F5$datacolheita[F5$individualid=="QU7PC1029003"] <- "2021-03-15"
F5$datacolheita[F5$individualid=="QUFMU1019001"&F5$datacolheita=="2021-07-08"] <- "2021-07-26"
F5$datacolheita[F5$individualid=="QUNRT1005002"&F5$datacolheita=="2021-07-21"] <- "2021-07-25"
F5$datacolheita[F5$individualid=="QUFMU1019001"&F5$datacolheita=="2021-07-08"] <- "2021-07-26"
F5$datacolheita[F5$individualid=="QUHCS1011001"&F5$datacolheita=="2021-01-07"] <- "2021-01-08"
F5$datacolheita[F5$individualid=="QU7GJ1015001"&F5$datacolheita=="2021-02-12"] <- "2021-02-17"
F5$datacolheita[F5$individualid=="QULRT1041002"&F5$datacolheita=="2021-05-06"] <- "2021-04-12"
F5$datacolheita[F5$individualid=="QUNRT1005002"&F5$datacolheita=="2021-07-22"] <- "2021-07-25"
F5$datacolheita[F5$individualid=="QULPC1073004"&is.na(F5$datacolheita)] <- "2021-02-11"
F5$datacolheita[F5$individualid=="QUFQM1005005"] <- "2022-02-17"
F5$datacolheita[F5$individualid=="QUNRT1004003"&is.na(F5$datacolheita)] <- "2021-02-17"
F5$datacolheita[F5$individualid=="QUNRT1004006"] <- "2022-02-17"
F5$datacolheita[F5$individualid=="QUNGJ1011008"] <- "2022-02-23"
F5$datacolheita[F5$individualid=="QUNRT1003005"] <- "2022-02-23"
F5$datacolheita[F5$individualid=="QU7QM1046005"] <- "2022-02-25"
F5$datacolheita[F5$individualid=="QU7CS3024001"] <- "2022-03-08"

# if datacolheita missing, yet a sample was collected, use dstart
F5$dstart <- as.Date(F5$start)
F5$datacolheita[is.na(F5$datacolheita)&F5$sample_was_taken!="Não"] <- F5$dstart[is.na(F5$datacolheita)&F5$sample_was_taken!="Não"]

# create a var combining collection date and ID
F5$dateID <- paste(F5$datacolheita, F5$individualid)

# simplified version
F5datesIDs <- F5 %>% select(individualid, symptoms, sample_was_taken, datacolheita, dateID)

## 3.2 nasal swab lab results
# manually entered database
nasalswabresults <- read_excel("database/completed 20220420/AfriCoVER_nasalswabresults_20220420.xlsx", sheet = "Detalhes dos Resultados ", 
                                 col_types = c("text", "text", "text", "text", "date", "date", "numeric", "text", "date", "text", "date", "date", 
                                               "text", "text", "text", "date", "text", "text"))
hist(nasalswabresults$datacolheita, breaks = 20)
nasalswabresults$datacolheita <- as.Date(nasalswabresults$datacolheita)
nasalswabresults$openhdsindividualId <- nasalswabresults$`Member ID`
nasalswabresults$Resultado <- tolower(nasalswabresults$Resultado)

# discrepant codes, dates, and results
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU2CS3002006"&nasalswabresults$datacolheita=="2021-01-13"] <- "QU2CS3002005" #found in F5 HH member swabbed the previous day
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU2CS3002005"&nasalswabresults$datacolheita=="2021-01-13"]  <- "2021-01-14" #found in F5 HH member swabbed the previous day
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU3MU1010001"&nasalswabresults$datacolheita=="2021-03-17"]  <- "2021-03-16" #found in same person but swabbed the previous day
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QU3PC1019007") # no F5 record, no result, no similar code the same day
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QU4CS3010003") # no F5 record, no similar code the same day
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QUIQM1011001"&nasalswabresults$datacolheita=="2021-01-20"]  <- "2021-01-13" #found in F5 same person but swabbed few days before
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QU7MU1003005") # no F5 record, no result, no similar code the same day
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU4GJ1014003"&nasalswabresults$datacolheita=="2021-01-25"]  <- "2021-01-23" #found in F5 same person but swabbed few days before
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU4RT1008001"&nasalswabresults$datacolheita=="2021-01-07"]  <- "2021-01-06" #found in F5 same person but swabbed few days before
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU5CS3008002"&nasalswabresults$datacolheita=="2021-03-13"]  <- "2021-03-12" #found in F5 same person but swabbed few days before
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU5MU1003007"&nasalswabresults$datacolheita=="2021-05-26"]  <- "2021-05-23" #found in F5 same person but swabbed few days before
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QU5MU1035002") # no F5 record, no result, no similar code the same day
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU5PC1004003"&nasalswabresults$datacolheita=="2021-03-17"]  <- "2021-03-16" #found in F5 same person but swabbed  day before
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU5PC1068001"&nasalswabresults$datacolheita=="2021-04-13"]  <- "2021-04-01" #found in F5 same person but swabbed few days before
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU5Q1047005"]  <- "QU5QM1047005" # typo in ID
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU5QM1008001"&nasalswabresults$datacolheita=="2021-05-19"]  <- "2021-05-18" #found in F5 same person but swabbed day before
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU1NM4004"] <- "QU1NM4004002"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU4G1009006"] <- "QU4GJ1009006"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU6G51009002"] <- "QU6GJ1009002"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU7CS3026007"] <- "QU7CS3026002"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUDRT1004001"] <- "QU0RT1004001"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUHMU10005003"] <- "QUHMU1005003"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUOPC1014001"] <- "QU0PC1014001"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU2CS3002006"] <- "QU2CS3002005" # typo in ID
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU5Q1047005"] <- "QU5QM1047005" # typo in ID
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QU5QM1033005") # no F5 record, no similar code the same day
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QU5RT1102002"&nasalswabresults$datacolheita=="2021-06-29"]  <- "2021-06-25" # found in F5 same person but swabbed day before
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU7CS3023007"] <- "QU7CS3026002" # typo in ID
nasalswabresults$datacolheita[nasalswabresults$openhdsindividualId=="QUF000001002"&nasalswabresults$datacolheita=="2021-03-17"]  <- "2021-03-16" #found in F5 same person but swabbed few days before
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-07-19"&nasalswabresults$openhdsindividualId=="QU5RT1102005"] <- "2021-06-25"
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-04-06"&nasalswabresults$openhdsindividualId=="QUFQM1007002"] <- "2021-04-05"
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-08-18"&nasalswabresults$openhdsindividualId=="QUGAM2002001"] <- "2021-08-17"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU3PC1029007"] <- "QU3PC1019007" # typo in ID, same date in F5
nasalswabresults$Resultado[nasalswabresults$datacolheita=="2021-07-12"&nasalswabresults$openhdsindividualId=="QU0GJ1003001"] <- "negativo" # ugd database probably more reliable
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QUOMU1045007") # no F5 nor participant record
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUNM4006001"] <- "QUNNM4006001" # typo in ID, same date in F5
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUMPC10100002"] <- "QUMPC1010002" # typo in ID
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUAGAM2002005"] <- "QUGAM2002005" # typo in ID
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-04-14"&nasalswabresults$openhdsindividualId=="QUGAM2002005"] <- "2021-04-13"
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QUE3PC1009001") # no F5 nor participant record
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUHMU10300012"] <- "QUHMU1030012" 
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUHMU103004"] <- "QUHMU1030004" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-03-13"&nasalswabresults$openhdsindividualId=="QU7CS3031001"] <- "2021-03-12"
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-03-13"&nasalswabresults$openhdsindividualId=="QU7CS3031016"] <- "2021-03-12"
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-02-19"&nasalswabresults$openhdsindividualId=="QU7MU1017003"] <- "2021-02-18"
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QU7NM4011004") # no F5 nor results
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QU5RT1102005"|nasalswabresults$Resultado!="si") # no F5 nor results
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-03-09"&nasalswabresults$openhdsindividualId=="QU7PC1020002"] <- "2021-03-08"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU7QJ1046003"] <- "QU7GJ1046003" 
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QU7QJ1052006"] <- "QU7GJ1052006" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-03-13"&nasalswabresults$openhdsindividualId=="QU7RT1023003"] <- "2021-03-12"
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-07-27"&nasalswabresults$openhdsindividualId=="QUGQM1005006"] <- "2021-08-06" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2022-02-01"&nasalswabresults$openhdsindividualId=="QULCS3010001"] <- "2022-02-02" 
nasalswabresults$openhdsindividualId[nasalswabresults$datacolheita=="2020-12-17"&nasalswabresults$openhdsindividualId=="QUGQM1006009"] <- "QUGQM1006002" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2020-12-22"&nasalswabresults$openhdsindividualId=="QUBRT1003007"] <- "2020-12-21" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-01-06"&nasalswabresults$openhdsindividualId=="QUMMU1006002"] <- "2021-01-05" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-01-07"&nasalswabresults$openhdsindividualId=="QUHMU1002005"] <- "2021-01-06" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-01-20"&nasalswabresults$openhdsindividualId=="QUIQM2039001"] <- "2021-01-06" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-01-14"&nasalswabresults$openhdsindividualId=="QUINM1012001"] <- "2021-01-13" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-01-18"&nasalswabresults$openhdsindividualId=="QUGPC1001001"] <- "2021-01-15" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-01-25"&nasalswabresults$openhdsindividualId=="QUECS2003001"] <- "2021-01-23" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-01-25"&nasalswabresults$openhdsindividualId=="QUINM1014004"] <- "2021-01-23" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-01-28"&nasalswabresults$openhdsindividualId=="QUGQM1006002"] <- "2021-01-27" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-01-28"&nasalswabresults$openhdsindividualId=="QUNQM1001001"] <- "2021-01-27" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-02-11"&nasalswabresults$openhdsindividualId=="QULPC1073004"] <- "2021-02-10" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-02-23"&nasalswabresults$openhdsindividualId=="QULPC1039001"] <- "2021-02-22" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-03-18"&nasalswabresults$openhdsindividualId=="QU7RT1013006"] <- "2021-03-19" 
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-07-19"&nasalswabresults$openhdsindividualId=="QU5RT1102005"] <- "2021-06-25"
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-07-08"&nasalswabresults$openhdsindividualId=="QUFCS1006011"] <- "2021-07-06"
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-07-09"&nasalswabresults$openhdsindividualId=="QUFMU1019003"] <- "2021-07-08"
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2021-07-09"&nasalswabresults$openhdsindividualId=="QUFPC1007002"] <- "2021-07-08"
nasalswabresults$datacolheita[nasalswabresults$datacolheita=="2020-12-17"&nasalswabresults$openhdsindividualId=="QUIMU1005001"] <- "2020-12-16"
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUGM11005005"] <- "QUGQM1005005" 
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUFQM1037001"] <- "QUFQM1031001" 
nasalswabresults$openhdsindividualId[nasalswabresults$openhdsindividualId=="QUFQM1072001"] <- "QULQM1072001" 
nasalswabresults <- subset(nasalswabresults, nasalswabresults$openhdsindividualId!="QUFQM2004014"|nasalswabresults$datacolheita!="2022-02-18")
 

# second database (UGD)
nasalswabresultsugd <- read_excel("database/nasalswabslabUGD_oct2022/221001_Base de dados Africover_Nasal swabs_UGD.xlsx", 
                                  sheet = "Base de dados Africover 26 8 20", 
                                  col_types = c("text", "text", "text", "text", "text", "text", "date", "text", 
                                                "numeric", "text", "text", "numeric", "date", "text", "numeric", "text", 
                                                "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                "numeric", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                "text", "numeric", "text", "text", "text", "text", "text", "text", "text", 
                                                "text", "text", "numeric", "text", "text", "text", "numeric", "text", 
                                                "text", "numeric", "text", "text", "date", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text"))
nasalswabresultsugd$resdpcm <- tolower(nasalswabresultsugd$resdpcm)
# if date of collection is missing, replace with reporting date
nasalswabresultsugd$datacolheita <- as.Date(nasalswabresultsugd$ssdffcolda)
nasalswabresultsugd$datacolheita[is.na(nasalswabresultsugd$ssdffcolda)] <- as.Date(nasalswabresultsugd$datent[is.na(nasalswabresultsugd$ssdffcolda)])
nasalswabresultsugd$missingdate[is.na(nasalswabresultsugd$ssdffcolda)] <- "missing"
# remove duplicate rows
dups = which(duplicated(nasalswabresultsugd%>%select('codusepi', 'datacolheita', 'resdpcm')))
nasalswabresultsugd <- nasalswabresultsugd %>% filter(!row.names(nasalswabresultsugd) %in% dups)
# mistakes found by comparing with F5 and with the UGD database
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-07-27"&nasalswabresultsugd$codusepi=="QU5MU1019004"] <- "2021-07-22"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-02-08"&nasalswabresultsugd$codusepi=="QU5PC1059002"] <- "2021-08-02"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-07-19"&nasalswabresultsugd$codusepi=="QU5RT1102005"] <- "2021-06-25"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-07-08"&nasalswabresultsugd$codusepi=="QUFQM2097001"] <- "2021-07-13"
nasalswabresultsugd <- nasalswabresultsugd %>% filter(codusepi!="QUMPC1010004"|datacolheita!="2021-12-27"|resdpcm!="negativo")
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-12-07"&nasalswabresultsugd$codusepi=="QU3QM2058001"] <- "2021-07-12"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUSPC1027005"] <- "QU5PC1027005"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUSNM4055002"] <- "QU5NM4055002"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUSPC1027005"] <- "QU5PC1027005"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUSNM4055006"] <- "QU5NM4055006"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUSPC1016006"] <- "QU5PC1016006"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUSPC1027007"] <- "QU5PC1027007"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUSPC1059005"] <- "QU5PC1059005"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUOMU1045007"] <- "QU0MU1045007"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUONM4008001"] <- "QU0NM4008001"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUOQM1004005"] <- "QU0QM1004005"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="Sem ficha de estudo"] <- "QU7PC1029002" # the only one with F5 record but no result on that date
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QU0TR101006"] <- "QU0TR1010006" # the only one with F5 record but no result on that date
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2022-02-02"&nasalswabresultsugd$codusepi=="QU0QM1004005"] <- "2022-02-01"
nasalswabresultsugd <- nasalswabresultsugd %>% filter(codusepi!="QU3M2050005") # entered as well with code QU3M2050005
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QU51035002"] <- "QU5MU1035002" # the only one with F5 record
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUMPC10050100"] <- "QUMPC1005010" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUMN4014006"] <- "QUMNM4014006" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUCC300222004"] <- "QUCCS3002004" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUECS004014"] <- "QUECS2004014" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUEQM100402"] <- "QUEQM1004002" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFCPC1014002"] <- "QUFPC1014002" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFM1020002"] <- "QUFMU1020002" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFMQ20171001"] <- "QUFNM1017001" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFPC101409"] <- "QUFPC1014009" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFQM10122015"] <- "QUFQM1012015" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFQM103001"] <- "QUFQM1030010" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFQM103004"] <- "QUFQM1030004" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUGAHM2002001"] <- "QUGAM2002001" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUGAM200205"] <- "QUGAM2002005" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUGQMD10004"] <- "QUGQM2010004" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QULCS300300"] <- "QULCS3006007" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QULG1028002"] <- "QULGJ1028002"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QULNM404002"] <- "QULNM4014002" 
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-02-03"&nasalswabresultsugd$codusepi=="QU5MU1069002"] <- "2021-03-02"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QU5MV1019002"] <- "QU5MU1019002"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-11-19"&nasalswabresultsugd$codusepi=="QU5MU1019002"] <- "2021-10-19"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-08-24"&nasalswabresultsugd$codusepi=="QU5PC1027005"] <- "2021-08-23"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QU5QM1033005"&nasalswabresultsugd$datacolheita=="2022-01-25"] <- "QU5QM1033006"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2022-08-03"&nasalswabresultsugd$codusepi=="QU7CS3024002"] <- "2022-03-08"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2022-02-02"&nasalswabresultsugd$codusepi=="QU0QM1004005"] <- "2022-02-01"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-12-23"&nasalswabresultsugd$codusepi=="QU3QM2054001"] <- "2021-07-01" # no F2 visit on that day, and discrepant between two result databases
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-02-03"&nasalswabresultsugd$codusepi=="QU5MU1069002"] <- "2021-03-02" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QU5MV1019002"] <- "QU5MV1019002"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-05-10"&nasalswabresultsugd$codusepi=="QUEAM2006004"] <- "2021-10-01" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUENH2012009"] <- "QUENM2012009"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUEQM1007001"] <- "QUFQM1007001"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUEQM1045002"] <- "QUFQM1045002"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUEQM2077001"] <- "QUFQM2077001"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-05-28"&nasalswabresultsugd$codusepi=="QUFCS2006001"] <- "2021-05-26" 
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-05-01"&nasalswabresultsugd$codusepi=="QUFNM1005004"] <- "2021-01-05" 
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-05-01"&nasalswabresultsugd$codusepi=="QUFNM1005006"] <- "2021-01-05" 
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2022-01-01"&nasalswabresultsugd$codusepi=="QUFNM1010004"] <- "2021-12-28" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFNM4023001"] <- "QU7NM4023001"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFNM4023006"] <- "QU7NM4023006"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFOM1030005"] <- "QUFQM1030005"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFOM2085004"] <- "QUFOM2085004"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-04-08"&nasalswabresultsugd$codusepi=="QUFQM1006003"] <- "2021-08-04" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFQM1037001"] <- "QUFQM1031001"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2022-03-23"&nasalswabresultsugd$codusepi=="QUFQM2060006"] <- "2022-03-22" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUGNM1010007"] <- "QUFNM1010007"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUGNM4007015"] <- "QU6NM4007015"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-07-27"&nasalswabresultsugd$codusepi=="QUGQM1005006"] <- "2021-08-06" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUHMN1009002"] <- "QUHMU1009002"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUHQMI001003"] <- "QUHQM1001003"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUIMU1011004"] <- "QUJMU1011004"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUJBM1006001"] <- "QUJNM1006001"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QULNH3020003"] <- "QULNM3020003"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2022-02-24"&nasalswabresultsugd$codusepi=="QULNM3003002"] <- "2022-02-09" 
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-11-01"&nasalswabresultsugd$codusepi=="QULNM4010001"] <- "2022-01-11" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QULQM1672001"] <- "QULQM1072001"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QULTR1041004"] <- "QULRT1041004"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUMQM1037004"] <- "QUHQM1037004"
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-12-04"&nasalswabresultsugd$codusepi=="QUNNM4006005"] <- "2021-04-12" 
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-06-08"&nasalswabresultsugd$codusepi=="QUNRT1002001"] <- "2021-06-21" 
nasalswabresultsugd$datacolheita[nasalswabresultsugd$datacolheita=="2021-03-18"&nasalswabresultsugd$codusepi=="QU7RT1013006"] <- "2021-03-19" 
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QU0TR1010006"] <- "QU0RT1010006"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUEPC1014006"] <- "QUFPC1014006"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QUFOM2085004"] <- "QUFQM2085004"
nasalswabresultsugd$codusepi[nasalswabresultsugd$codusepi=="QU7QJ1052006"] <- "QU7GJ1052006" 

# merge both databases
# create a common variable in both databases
nasalswabresults$dateID <- paste(nasalswabresults$datacolheita, nasalswabresults$openhdsindividualId)
nasalswabresultsugd$dateID <- paste(nasalswabresultsugd$datacolheita, nasalswabresultsugd$codusepi)

# keep only useful variables
nasalswabresultssimpl <- nasalswabresults %>%
  select(dateID, Resultado, datacolheita, openhdsindividualId)
nasalswabresultsugdsimpl <- nasalswabresultsugd %>%
  select(dateID, resdpcm, valorct, datacolheita, missingdate, codusepi)

# merge
nasalswabresultsmerged <- merge(nasalswabresultssimpl, nasalswabresultsugdsimpl, by = "dateID", all = T)

# remove when NA NA
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!= "NA NA")

# remove values that appear in both databases but with slightly different dates (checking the actual date in F5)
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-07-11 QUEQM1004002")
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-11 QUEQM1004002")
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-04-14 QUGAM2002005")
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-07-12 QUFQM2064001") # 2nd test for same disease episode that was and remained negative
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-07-15 QUGMU1003007") # 1st test for same disease episode, for which a full entry two weeks later
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-06 QUMCS3002007") # no study participant/baseline record
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-04-23 QUEAM2009008") # 2nd test result for same disease episode
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-05-10 QUMRT1002005") # date turned around, another entry on 2021-10-05 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-03-05 QUMRT1001007") # another entry on 2021-03-04 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-19 QULRT1041004") # another entry on 2021-08-18 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-03-09 QULPC1005003") # another entry on 2021-03-08 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-03-13 QULMU1067002") # another entry on 2021-03-18 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2022-01-14 QULMU1015004") # another entry on 2021-01-13 in UGD db
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-03-09 QULGJ1028002") # another entry on 2021-03-08 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-07-28 QULGJ1017003") # another entry on 2021-07-29 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-07-28 QULGJ1017002") # another entry on 2021-07-29 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-05-11 QULCS3011002") # another entry on 2021-05-10 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2022-02-01 QULCS3010001") # another entry on 2022-02-02 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-18 QULCS3006005") # another entry on 20210817 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-18 QULCS2048001") # another entry on 20210817 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-04-08 QUGQM2010004") # another entry on 20210406 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-18 QUGAM2002013") # another entry on 20210817 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-18 QUGAM2002009") # another entry on 20210817 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-18 QUGAM2002004") # another entry on 20210817 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-18 QUGAM2002002") # another entry on 20210817 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-04-04 QUFQM2047005") # another entry on 20210406 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-27 QUFQM1033001") # another entry on 20210826 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-04-08 QUFQM1012001") # another entry on 20210406 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-03-15 QUFNM1008002") # another entry on 20210313in F2
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-03-15 QU7PC1029002") # another entry on 20210316 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-01-05 QUFNM1005006") # another entry corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-01-05 QUFNM1005004") # another entry corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2022-02-23 QUFMU1003005") # another entry on 20220222 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2022-02-23 QUFMU1003004") # another entry on 20220222 corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-05-27 QUFCS2006001") # another entry corresponding to F5 entry
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-08-27 QUFCS2005007") # another entry corresponding to F5 entry
# remove observations of persons not recorded at baseline as participant
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-02-24 QU3PC1019007") 
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-01-07 QUFQM2079001") 
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-01-14 QU7MU1013006") 
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-01-18 QUCCS3009012") 
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(dateID!="2021-03-13 QUFNM1008002") 

# keep a single result, ID & collection date
nasalswabresultsmerged$ID <- nasalswabresultsmerged$codusepi
nasalswabresultsmerged$ID[is.na(nasalswabresultsmerged$codusepi)] <- nasalswabresultsmerged$openhdsindividualId[is.na(nasalswabresultsmerged$codusepi)]
nasalswabresultsmerged$testresult[!is.na(nasalswabresultsmerged$resdpcm)] <- nasalswabresultsmerged$resdpcm[!is.na(nasalswabresultsmerged$resdpcm)]
nasalswabresultsmerged$testresult[is.na(nasalswabresultsmerged$resdpcm)] <- nasalswabresultsmerged$Resultado[is.na(nasalswabresultsmerged$resdpcm)]
nasalswabresultsmerged$datacolheita <- as.Date(nasalswabresultsmerged$datacolheita.y)
nasalswabresultsmerged$datacolheita[is.na(nasalswabresultsmerged$datacolheita)] <- as.Date(nasalswabresultsmerged$datacolheita.x[is.na(nasalswabresultsmerged$datacolheita)])
nasalswabresultsmerged <- nasalswabresultsmerged %>% select(dateID, ID, datacolheita, testresult, valorct)

# after merging, in case of discordant result, NA, or 'si' result, results on paper forms looked up
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2022-02-16 QUFQM1037001"] <- "negativo"
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-07-13 QUKQM1012005"] <- "positivo"
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-12-27 QUMGJ1006001"] <- "negativo"
nasalswabresultsmerged$valorct[nasalswabresultsmerged$dateID=="2021-12-27 QUMGJ1006001"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-07-25 QUMNM4002005"] <- "positivo"
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-12-29 QUMNM4012007"] <- "positivo"
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-07-13 QUKQM1012005"] <- "positivo"
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-12-27 QUMGJ1006001"] <- "negativo"
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-07-25 QUMNM4002005"] <- "positivo"
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-12-29 QUMNM4012007"] <- "positivo"
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-07-27 QULPC1073003"] <- NA # not found
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-04-12 QULRT1041002"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-01-05 QUMMU1006002"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-12-21 QUMNM4014007"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2022-02-17 QUNRT1004003"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2022-02-17 QUNRT1004006"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-01-23 QU1NM4004002"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-07-19 QU3QM1020001"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-01-06 QU4RT1008001"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-05-20 QU5RT1022007"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2022-02-17 QU7CS3036001"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-05-28 QU7PC1033004"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2021-07-13 QUCCS3002004"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2022-02-17 QUFQM1005005"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2022-01-26 QUFQM2085002"] <- NA
nasalswabresultsmerged$testresult[nasalswabresultsmerged$dateID=="2022-02-01 QULCS3010001"] <- "negativo"

# remove duplicates
# check duplicated rows
dups = which(duplicated(nasalswabresultsmerged%>%select(dateID, testresult)))
length(dups) # 4 duplicate rows still
# Remove duplicated rows
nasalswabresultsmerged <- nasalswabresultsmerged %>% filter(!row.names(nasalswabresultsmerged) %in% dups) # n=9959

table(nasalswabresultsmerged$testresult, useNA = "always")

# rename results to english
nasalswabresultsmerged$testresult[nasalswabresultsmerged$testresult=="negativo"] <- "negative"
nasalswabresultsmerged$testresult[nasalswabresultsmerged$testresult=="positivo"] <- "positive"

# tests with CT value still missing
table(nasalswabresultsmerged$valorct[nasalswabresultsmerged$testresult=="positive"], useNA = "always")
missingct <- nasalswabresultsmerged$dateID[nasalswabresultsmerged$testresult=="positive"&is.na(nasalswabresultsmerged$valorct)]
missingct
write.csv(missingct, file = "missingct.csv")

# add a variable to say how many episodes reported by that participant
nasalswabresultsbyparticipant <- nasalswabresultsmerged %>% group_by(ID) %>% summarise(n_episodes=n())
nasalswabresultsmerged <- merge(nasalswabresultsmerged, nasalswabresultsbyparticipant, by = "ID", all.x = T)
table(nasalswabresultsmerged$n_episodes) # checked for multiple results per disease episode (removing 2nd tests) on 20230426


## 3.3 merge lab results and ODK F5
possiblecases <- merge(F5, nasalswabresultsmerged, by = "dateID", all=T) 

# add ID even if no match could be done
possiblecases$individualid[is.na(possiblecases$individualid)] <- possiblecases$ID[is.na(possiblecases$individualid)]

# PCR results but no F5 entry
resultwithoutF5 <- possiblecases$dateID[is.na(possiblecases$start)]
resultwithoutF5 # all checked, STILL to create F5 entries based on symptoms & dates in UGD database
write.csv(resultwithoutF5, file = "resultwithoutF5.csv")

# F5 entry but no PCR results
F5withoutresult <- possiblecases %>% filter(is.na(possiblecases$testresult)&is.na(possiblecases$datacolheita.y)&sample_was_taken!="Não") %>% select(dateID, datacolheita.x, datacolheita.y, dstart, sample_was_taken)
F5withoutresult # 14 remaining without result after double check in paper forms
# one result found from double check
possiblecases$testresult[possiblecases$dateID=="2021-03-07 QU7NM4005002"] <- "negative"

# look for those dates which might have been wrongly entered 
F5withoutresult %>% filter(dstart!=datacolheita.x)
write.csv(F5withoutresult, file = "F5withoutresult.csv")

# remove observations of cases for which a test was done, but didn't belong to the HDSS cohort
ids_to_remove <- c("BL0QM2012004", "BLBDA1017004", "QU0RT1005008", "QU1PC1003007", "QU3NM1011001", "QU3NM1012010", "QU7NM4022005", "QU7PC1019006", "QU8NM1031002", "QU8NM1031003", "QU8NM1047004", "QU8NM1066002", "QU8NM1066003", "QU8NM1066004", "QU8NM1066005", "QU8NM1067004", "QUBNM4003009", "QUCPC1005006", "QUFNM1004003", "QUFNM1010010", "QUFNM1010012", "QUFNM1026010", "QUFPC1001002", "QUFPC1003002", "QUFPC1026005", "QUFPC1035004", "QUJPC2011008", "QULAM2067003", "QULAM2067004", "QULNM2021011", "QULNM4009008", "QULNM4009009", "QULNM4011001", "QULPC1001007", "QULPC1025008", "QULPC1037003", "QULPC1040007", "QUMNM4013001", "QUMPC1010005", "QUMPC1018002", "QUTRT1030012", "QUXQM2014004")
possiblecases <- possiblecases[!(possiblecases$individualid %in% ids_to_remove), ]

# complete missing dates
possiblecases$datacolheita <- possiblecases$datacolheita.y
possiblecases$datacolheita[is.na(possiblecases$datacolheita)] <- possiblecases$datacolheita.x[is.na(possiblecases$datacolheita)]

# link case and participant data
# confirmed cases
confirmedcases <- possiblecases %>% filter(testresult=="positive") %>% select(individualid, testresult, datacolheita)
cases_participants <- merge(confirmedcases, participants, by.x = "individualid", by.y = "openhdsindividualId", all = T)
table(cases_participants$testresult, useNA = "always")
# remove anyone without baseline data nor result
cases_participants <- cases_participants %>% filter(!is.na(testresult) | !is.na(locationid))
# remove those without baseline demographics (age, sex)
cases_participants <- cases_participants %>% filter(!is.na(age) & !is.na(GENDER))

# first COVID-19
# sort the dataframe by 'datadacolheita' in ascending order
sorted_cases <- confirmedcases %>% arrange(datacolheita)
# Keep only the first row for each 'individualid'
confirmedcases_firstonly <- sorted_cases %>% group_by(individualid) %>% slice(1)
confirmedcases_firstonly_participants <- merge(confirmedcases_firstonly, participants, by.x = "individualid", by.y = "openhdsindividualId", all = T)
# remove anyone without baseline data nor result
confirmedcases_firstonly_participants <- confirmedcases_firstonly_participants %>% filter(!is.na(testresult) | !is.na(locationid))
# remove those without baseline demographics (age, sex)
confirmedcases_firstonly_participants <- confirmedcases_firstonly_participants %>% filter(!is.na(age) & !is.na(GENDER))
# # remove unnecessary variables
# confirmedcases_firstonly_participants <- confirmedcases_firstonly_participants %>% select(-dob,                                                                                    -ID,-datacolheita.x, -datacolheita.y, -start.y, -key)
table(confirmedcases_firstonly_participants$testresult, useNA = "always") # 137 positives

# export databases
write_xlsx(possiblecases, "possiblecases.xlsx") 
write_xlsx(cases_participants, "cases_participants.xlsx") 
write_xlsx(confirmedcases_firstonly_participants, "confirmedcases_firstonly_participants.xlsx") 

## 3.4 outcome of cases
F6 <- xl.read.file("database/20221123/Africover F6 Confirmed case FU_full_DB.xlsx", password = "africover_1")
table(F6$follow_up)
table(F6$health_status)
# clean outcomes
F6$outcome[F6$health_status56=="Recuperado/Saudável "] <- "cured"
F6$outcome[F6$health_status=="Óbito"] <- "dead"
F6$outcome[is.na(F6$health_status56) & F6$health_status=="Recuperado/Saudável "] <- "cured"
F6$outcome[is.na(F6$health_status) & F6$health_status56=="Não sabe"] <- "unknown"
F6$outcome[is.na(F6$outcome) & F6$health_status56=="Outro, especificar"] <- "not cured"
F6$outcome[is.na(F6$outcome) & F6$health_status=="Outro, especificar"] <- "not cured"

# clean hospi
table(F6$hospital_admission) # no hospi recorded
table(F6$health_status56) # also no records in health status
table(F6$specify_health_status) # also no records in health status
table(F6$specify_health_status56) # also no records in health status

# Define the desired order of outcome levels
desired_order <- c("dead", "cured", "not cured", "unknown")
# Convert "outcome" to a factor with the desired order
F6$outcome <- factor(F6$outcome, levels = desired_order, ordered = TRUE)
# Sort the dataframe by outcome
F6 <- F6 %>% arrange(outcome)
# Group the data by individualid and select the first observation for each group
F6_summary <- F6 %>% group_by(individualid) %>% slice(1)
# Remove the grouping
F6_summary <- F6_summary %>% ungroup()
# link to case data
caseoutcomes <- merge(F6_summary, possiblecases, by = "individualid", all.x = T)
# exclude negative cases
table(caseoutcomes$testresult, caseoutcomes$outcome)

## 3.5 F2 active surveillance follow-up
# household part
#  F2 <- read_excel("database/20220323/F2.xlsx")
FU <- xl.read.file("database/20221123/Africover F2 Follow Up_full_DB.xlsx", password = "africover_1")

# keep only useful variables
FU <- FU %>% select(start, locationId, indivIdualId, visit_done, contact_type, contact_date, contact, respiratory_symptoms, individualId_rs, symptoms_date, health_care, health_care_center, specify_health_center, malaria_test, malaria_diagnose)

# contactdate to date format
FU$contact_date <- as.Date(FU$contact_date)

# relabel values to english
FU$respiratory_symptoms[FU$respiratory_symptoms=="Sim"] <- "yes"
FU$respiratory_symptoms[FU$respiratory_symptoms=="Não"] <- "no"
FU$respiratory_symptoms[FU$respiratory_symptoms=="Não sabe"] <- "no"

# remove planned visits that were not done
FU <- FU %>% filter(FU$visit_done=="Sim")

# remove 2nd visits during the same week (for instance, a phone visit followed by a house visit if a possible case was reported over the phone) 
# make variable that says how many vars missing
FU$missing_count <- rowSums(is.na(FU))
# create a variable with the week
FU <- FU %>%  mutate(week = as.Date(cut(contact_date, breaks = "1 week")))
FU$weekHHID <- paste(FU$locationId,FU$week)
# check rows of visits to HH in the same week, and keep the one with least missing values
# remove of duplicates that with most missing values
FU <- FU %>%
  group_by(weekHHID) %>%
  arrange(missing_count) %>%
  distinct(weekHHID, .keep_all = TRUE)

# save the FU database
write.csv(FU, file = "FU.csv")

# combine confirmedcases_firstonly_participants and FU
# add the time under follow-up to make a denominator person-months
FUbyHH <- FU %>% group_by(locationId) %>% summarise(nvisits=n())
sum(FUbyHH$nvisits)
cases_participantsFU <- merge(confirmedcases_firstonly_participants, FUbyHH, by.x = "locationid", by.y = "locationId", all.x = T)
cases_participantsFU$time <- cases_participantsFU$nvisits/2 # number of person-months followed up
# remove those participants that haven't been followed-up
cases_participantsFU <- cases_participantsFU %>% filter(!is.na(pm))
# check and remove duplicated rows 
dups = which(duplicated(cases_participantsFU%>%select('individualid','testresult')))
cases_participantsFU <- cases_participantsFU %>% filter(!row.names(cases_participantsFU) %in% dups)

# for confirmed cases, follow up time is only the time until a positive test
confirmed  <- cases_participantsFU %>% filter(testresult=="positive")
FUsimpl <- FU %>% select(locationId, indivIdualId, contact_date)
censored <- merge(confirmed, FU, by.x = "locationid", by.y = "locationId", all.x = TRUE)
censored <- censored %>%
  group_by(individualid) %>%
  filter(contact_date <= datacolheita)
FUbyHH_censored <- censored %>% group_by(individualid) %>% summarise(nvisits_censored=n())
cases_participantsFU <- merge(cases_participantsFU, FUbyHH_censored, by = "individualid", all.x = T)
cases_participantsFU$time[!is.na(cases_participantsFU$nvisits_censored)] <- cases_participantsFU$nvisits_censored[!is.na(cases_participantsFU$nvisits_censored)]/2 # number of person-months followed up

# add time with surveillance during baseline (F1) on top of follow-up visits (F2)
cases_participantsFU$time[is.na(cases_participantsFU$time)] <- 0
cases_participantsFU$time <- cases_participantsFU$time + 0.5
table(cases_participantsFU$time, useNA = "always")

# make factors of explanatory variables
cases_participantsFU$overweight[!is.na(cases_participantsFU$BMI)] <- "normal"
cases_participantsFU$overweight[cases_participantsFU$BMI>24.99&cases_participantsFU$BMI<30] <- "overweight"
cases_participantsFU$overweight[cases_participantsFU$BMI>29.99&cases_participantsFU$BMI<50] <- "obesity"
cases_participantsFU$overweight[cases_participantsFU$BMI<19&cases_participantsFU$BMI>8] <- "underweight"
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
cases_participantsFU$hypertensionbin[cases_participantsFU$hypertension=="Não diagnosticado"] <- 0
cases_participantsFU$hypertensionbin[cases_participantsFU$hypertension=="Diagnostico previo ou evento mas Não acompanhado"] <- 1
cases_participantsFU$hypertensionbin[cases_participantsFU$hypertension=="Diagnosticado, em seguimento mas SEM tratamento especifico"] <- 1
cases_participantsFU$hypertensionbin[cases_participantsFU$hypertension=="Diagnosticado, em acompanhamento com tratamento especifico"] <- 1
cases_participantsFU$diabetesbin[cases_participantsFU$diabet=="Não diagnosticado"] <- 0
cases_participantsFU$diabetesbin[cases_participantsFU$diabet=="Diagnosticado, em acompanhamento com tratamento especifico"] <- 1
cases_participantsFU$lowestSES[!is.na(cases_participantsFU$SesScoreQnt=="1. very low")] <- 0
cases_participantsFU$lowestSES[cases_participantsFU$SesScoreQnt=="1. very low"] <- 1
cases_participantsFU$SesScoreQnt <- factor(cases_participantsFU$SesScoreQnt)
cases_participantsFU$hivbin[cases_participantsFU$hiv=="Crianca exposta"] <- 1
cases_participantsFU$hivbin[cases_participantsFU$hiv=="Seropositivo e em tratamento anti-retroviral"] <- 1
cases_participantsFU$hivbin[cases_participantsFU$hiv=="estado desconhecido"] <- 0
cases_participantsFU$hivbin[cases_participantsFU$hiv=="HIV negativo (no momento do ultimo teste HIV)"] <- 0
cases_participantsFU$agegr <- factor(cases_participantsFU$agegr)
cases_participantsFU$sex <- factor(cases_participantsFU$GENDER)
cases_participantsFU$smokingbin[grepl("fumador", cases_participantsFU$smoking)==T] <- "(ex-)smoker"
cases_participantsFU$smokingbin[grepl("Fumador", cases_participantsFU$smoking)==T] <- "(ex-)smoker"
cases_participantsFU$smokingbin[grepl("Nao", cases_participantsFU$smoking)==T] <- "non smoker"
cases_participantsFU$smokingbin[grepl("Nunca", cases_participantsFU$smoking)==T] <- "non smoker"
table(cases_participantsFU$main_bus)
cases_participantsFU$publictransport_pastweek[cases_participantsFU$mass_bus=="Não"] <- "none"
cases_participantsFU$publictransport_pastweek[cases_participantsFU$main_bus=="taxi Nao partilhado com outros"] <- "none"
cases_participantsFU$publictransport_pastweek[cases_participantsFU$main_bus=="minibus"] <- "bus/train"
cases_participantsFU$publictransport_pastweek[cases_participantsFU$main_bus=="comboio"] <- "bus/train"
cases_participantsFU$publictransport_pastweek[cases_participantsFU$main_bus=="autocarro publico"] <- "bus/train"
cases_participantsFU$publictransport_pastweek[cases_participantsFU$main_bus=="taxi partilhado"] <- "moto taxi/shared taxi"
cases_participantsFU$publictransport_pastweek[cases_participantsFU$main_bus=="moto taxi"] <- "moto taxi/shared taxi"
cases_participantsFU$publictransport_pastweek <- factor(cases_participantsFU$publictransport_pastweek)
cases_participantsFU$pregnant[grepl("Sim", cases_participantsFU$main_bus)==T] <- "pregnant"
cases_participantsFU$pregnant[grepl("Não", cases_participantsFU$main_bus)==T] <- "not"
cases_participantsFU$leukemia[!is.na(cases_participantsFU$cancer)] <- "not"
cases_participantsFU$leukemia[grepl("malignidade hemato", cases_participantsFU$cancer)==T] <- "leukemia"
cases_participantsFU$heart[!is.na(cases_participantsFU$hearth_diseases)] <- "not"
cases_participantsFU$heart[grepl("Diagno", cases_participantsFU$hearth_diseases)==T] <- "heart disease"
cases_participantsFU$lung[!is.na(cases_participantsFU$chronic_lung_disease)] <- "not"
cases_participantsFU$lung[grepl("asma", cases_participantsFU$chronic_lung_disease)==T] <- "asthma"
cases_participantsFU$lung[grepl("outra", cases_participantsFU$chronic_lung_disease)==T] <- "chronic pulmonary disease"
cases_participantsFU$tbbin[!is.na(cases_participantsFU$tb)] <- "not"
cases_participantsFU$tbbin[grepl("tubercu", cases_participantsFU$tb)==T] <- "(history of) tuberculosis"
cases_participantsFU$tbbin <- factor(cases_participantsFU$tbbin)
cases_participantsFU$bedroom_sharing <- factor(cases_participantsFU$bedroom_sharing)
cases_participantsFU$toylet_sharing <- factor(cases_participantsFU$toylet_sharing)
cases_participantsFU$handwash <- factor(cases_participantsFU$handwash)
cases_participantsFU$water_availability <- factor(cases_participantsFU$water_availability)
cases_participantsFU$soap_availability <- factor(cases_participantsFU$soap_availability)
cases_participantsFU$health_worker <- factor(cases_participantsFU$health_worker)
cases_participantsFU$bedroom_sharing[grepl("6", cases_participantsFU$bedroom_sharing)==T] <- "3 a 5"

# change category to use as reference
cases_participantsFU <- cases_participantsFU %>% 
  mutate(sex = fct_relevel(sex, "M", after = 0)) 
cases_participantsFU <- cases_participantsFU %>% 
  mutate(heart = fct_relevel(heart, "not", after = 0)) 
cases_participantsFU <- cases_participantsFU %>% 
  mutate(leukemia = fct_relevel(leukemia, "not", after = 0)) 
cases_participantsFU <- cases_participantsFU %>% 
  mutate(tbbin = fct_relevel(tbbin, "not", after = 0)) 
cases_participantsFU <- cases_participantsFU %>% 
  mutate(lung = fct_relevel(lung, "not", after = 0)) 
cases_participantsFU <- cases_participantsFU %>% 
  mutate(lung = fct_relevel(water_availability, "Disponivel", after = 0)) 
cases_participantsFU <- cases_participantsFU %>% 
  mutate(lung = fct_relevel(publictransport_pastweek, "none", after = 0)) 
cases_participantsFU <- cases_participantsFU %>% 
  mutate(lung = fct_relevel(smokingbin, "non smoker", after = 0)) 

# generate a variable 'event' to say who had the event
cases_participantsFU$event <- 0
cases_participantsFU$event[cases_participantsFU$testresult=="positive"] <- 1
cases_participantsFU$eventf <- factor(cases_participantsFU$event)
table(cases_participantsFU$eventf)

# smaller age groups
breaks <- c(seq(0, 60, by = 10), 70, Inf)

# create factor variable 'agegr'
cases_participantsFU$agegr10 <- cut(cases_participantsFU$age, breaks = breaks, labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"), right = FALSE)
cases_participantsFU$agegr10 <- factor(cases_participantsFU$agegr10)
table(cases_participantsFU$agegr10)

# save this database
write.table(cases_participantsFU, file = "cases_participantsFU.txt")
write.csv(cases_participantsFU, "cases_participantsFU.csv")

# # format date of symptom onset
# FU$datacolheita <- as.Date(FU$history_symptoms_date_colected_)
# FU$datacolheita[is.na(FU$datacolheita)] <- as.Date(FU$history_symptomsdate_colected[is.na(FU$datacolheita)])
# table(FU$datacolheita, useNA = "always")
# FU$datesymptomonset <- as.Date(FU$history_symptoms_symptoms_date_)
# table(FU$datesymptomonset, useNA = "always")
# 
# # individual HH member part (episodes of illness)
# FUepisodes <- read_excel("database/20220323/F2_symptoms.xlsx")
# # check duplicated rows
# dups = which(duplicated(FUepisodes%>%select(history_symptoms_individualID_r, KEY)))
# length(dups)
# # Remove duplicated rows
# FUepisodes <- FUepisodes %>% filter(!row.names(FUepisodes) %in% dups) # only n=106 remaining, something must have gone wrong during data collection CHECK
# 
# # merge HH visit data with episodes
# FU_mergedepisodes <- merge(FU, FUepisodes, by = "KEY", all = T)
# 
# # link active surveillance visits and possible cases
# # create unique variable for the HH during that visit
# FU$HH_datacolheita <- NA
# FU$HH_datacolheita[!is.na(FU$openhdslocationId)&!is.na(FU$datacolheita)] <- paste(FU$openhdslocationId[!is.na(FU$openhdslocationId)&!is.na(FU$datacolheita)],"_",FU$datacolheita[!is.na(FU$openhdslocationId)&!is.na(FU$datacolheita)])
# # create unique var for the HH where a sample is collected
# possiblecases_participants$HH_datacolheita <- NA
# possiblecases_participants$HH_datacolheita[!is.na(possiblecases_participants$openhds.locationId)&!is.na(possiblecases_participants$datacolheita.x)] <- paste(possiblecases_participants$openhds.locationId[!is.na(possiblecases_participants$openhds.locationId)&!is.na(possiblecases_participants$datacolheita.x)],"_",possiblecases_participants$datacolheita.x[!is.na(possiblecases_participants$openhds.locationId)&!is.na(possiblecases_participants$datacolheita.x)] )
# # merge both, removing possible cases not directly linked to a visit (CHECK)
# # activesurveillance <- merge(FU, possiblecases_participants, by = "HH_datacolheita", all.x = T) SOMETHING WRONG HERE. R CRASHES

# summary with the number of visits per household
nvisitsperHH <- FU %>% group_by(locationId) %>% summarise(n=n())

# import geographical coordinates of households, in order to link to household visits
geopoints <- read_excel("database/AfriCoVER_geopoint_Vicky_20230427.xlsx", 
                        sheet = "Sheet1", col_types = c("text", 
                                                        "text", "text", "numeric", "numeric", 
                                                        "numeric"))
colnames(geopoints) <- c("locationId","indivIdualId","bairro","quarteiro","latitude","longitude")

# combine the number of household visits and the geom points to create a map of visits in the area
visit_geompoints <- merge(nvisitsperHH, geopoints, by = "locationId") # keep only those which fully merged

# keep one coordinate per HH
visit_geompoints <- visit_geompoints %>% group_by(locationId, latitude,longitude) %>% summarise(n=min(n))

write.csv(visit_geompoints, file = "visit_geompoints.csv")

#### 4. SEROSURVEILLANCE ####
# we will start from the samples, as for some samples, an ODK entry is missing, or ODK entries exit for which no sample can be found
## 4.1. import two DBS inventory files ##
## import file of the first 6 months of the study (15 Dec 20 to 28 June 21)
# DBSdec20jun21 <- read_excel("database/Africover lab DB 05072021/africover_inventario_DBS_210704.xlsx", 
#                                  sheet = "Sheet1", col_types = c("text", "text", "text", "text", "text", "text", 
#                                                                  "date", "text", "text", "text", "date", "numeric", "date"))
# DBSinventory <- read_excel("database/20220323/DBS_inventory_update230522.xlsx", 
#                                          col_types = c("text", "text", "text", 
#                                                        "text", "text", "text", "text", "text", 
#                                                        "text", "text", "text", "numeric", 
#                                                        "text", "text"))
# # rename variables
# colnames(DBSinventory) <- c("box","ziplocknr","study","studyname","openhdsindividualId", "samplename","data_da_colheita","sampletype","geolocation","province","data_congelacao","n_defrosts")
DBSinventory <- read_excel("database/updatedDBSinventoryFeb2023/230224 AfriCoVER_Inventário DBS_AL.xlsx", 
                           sheet = "all", col_types = c("numeric", "text", "text", "text", "text", "numeric", 
                                                    "date", "date", "text", "date", "text", "text", "numeric", "text","text","text"))
# rename variables
colnames(DBSinventory) <- c("nr","ziplocknr","labnr","openhdsindividualId", "sex", "age", "dob","datacolheita","sampletype","storagedate","comments","initials", "boxnr", "boxpositionnr", "placenr", "placepositionnr")
DBSinventory$datacolheita <- as.Date(DBSinventory$datacolheita)
# checked discordant ages with that in participant lists
# DBSinventory$ageyears <- round(as.numeric((as.Date(DBSinventory$data_da_colheita) - as.Date(DBSinventory$dob)))/365.25,0)
# DBSinventory$diff <- DBSinventory$age - DBSinventory$ageyears
DBSinventory$age[DBSinventory$openhdsindividualId=="QU5RT1050002"] <- 23
DBSinventory$age[DBSinventory$openhdsindividualId=="QULNM3012003"] <- 18

# # format date of collection as dates
# DBSinventory$data_da_colheita[DBSinventory$data_da_colheita=="24/062021"] <- "24/06/2021"
# DBSinventory$data_da_colheita[DBSinventory$data_da_colheita=="22/062021"] <- "22/06/2021"
# DBSinventory$datacolheita <- NA
# DBSinventory$datacolheita[grepl("44", DBSinventory$data_da_colheita)==TRUE] <- DBSinventory$data_da_colheita[grepl("44", DBSinventory$data_da_colheita)==TRUE]
# DBSinventory$datacolheita <- as.numeric(DBSinventory$datacolheita)
# DBSinventory$datacolheita <- as.Date(DBSinventory$datacolheita, origin = "1899-12-30")
# DBSinventory$datacolheita[grepl("44", DBSinventory$data_da_colheita)==F] <- as.Date(DBSinventory$data_da_colheita[grepl("44", DBSinventory$data_da_colheita)==F], "%d/%m/%Y")
# table(DBSinventory$datacolheita, useNA ="always")
# # it did not recognize "oct", so I changed manually in the excel file

# mistakes in IDs -> checked inconsistencies manually in the paper forms
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QLNM3006002"] <- "QULNM3006002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="UNMU1001001"] <- "QUJNM1001001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU0GT1001002" & DBSinventory$datacolheita=="2021-05-10"] <- "QU0GJ1001002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU0GT1001009" & DBSinventory$datacolheita=="2021-05-12"] <- "QU0GJ1001009"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU1NM1007002" & DBSinventory$datacolheita=="2021-06-28"] <- "QU1NM1007002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU1NM400801" & DBSinventory$datacolheita=="2020-12-22"] <- "QU1NM4008001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU1QM2038004" & DBSinventory$datacolheita=="2021-01-05"] <- "QUIQM2038004"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU2GT1006001" & DBSinventory$datacolheita=="2021-01-04"] <- NA # not found in the paper forms
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU3QM102202" & DBSinventory$datacolheita=="2020-12-22"] <- "QU3QM1022002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU4C10070010" & DBSinventory$datacolheita=="2021-01-06"] <- "QU4PC1007010"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU5GT1003004"] <- "QU5GJ1003004"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU7M1029001"] <- "QU7QM1029001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU7MU139001"] <- "QU7MU1039001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU7NM4004013" & DBSinventory$datacolheita=="2021-02-08"] <- "QU7NM4001013"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU7NM404002"] <- "QU7NM4046002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUACS3010001"] <- "QU4CS3010001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUCP1006004"] <- "QUCPC1006004"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUENM1028001" & DBSinventory$datacolheita=="2021-01-21"] <- "QUENM2002001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUEQM1012001"] <- "QUEQM1002001"
# DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUEQM2064001" & DBSinventory$datacolheita=="2021-01-15"] <- "QUEQM2014001" I can't retrace this one
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUEQM2077002" & DBSinventory$datacolheita=="2021-02-10"] <- "QUFQM2077002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUFAM2023009"] <- "QUFAM2032009"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUFM20970110" & DBSinventory$datacolheita=="2021-01-11"] <- "QUFQM2097011"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUFNM1002007" & DBSinventory$datacolheita=="2021-05-26"] <- "QUFNM2002007"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUFNM129005"] <- "QUFNM1029005"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUFQM1097001"] <- "QUFQM2097001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUGQM4011002"] <- "QUGQM2011002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUHM2053002"] <- "QUHQM2053002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUHQM2012001" & DBSinventory$datacolheita=="2021-01-06"] <- "QUHQM1012001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUHUM1012001"] <- "QUHMU1012001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUICS3001013"] <- "QU1CS3001013"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUKAM1008006"] <- "QUKAM1008005"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QULAM203310"] <- "QULAM2033010"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QULG1028004"] <- "QULGJ1028004"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QULG51006008"] <- "QULGJ1006008"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QULMU100602"] <- "QULMU1006002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUMCS2001006" & DBSinventory$datacolheita=="2021-04-21"] <- "QUMCS3001006"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUMCS2003001" & DBSinventory$datacolheita=="2021-04-21"] <- "QUMCS3003001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUMCS301107"] <- "QUMCS3011007"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUMM1002005"] <- "QUMMU1002005"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUNM1003002"] <- "QUNMU1003002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUOGJ1003009" & DBSinventory$datacolheita=="2021-01-13"] <- "QU0GJ1003009"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QURT10210033"] <- "QU5RT1021003"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUSAM2044003"] <- "QU5AM2044003"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUSCS3002004"] <- "QU5CS3002004"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUSCS3038006"] <- "QU5CS3038006"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUZGJ1005003" & DBSinventory$datacolheita=="2021-02-18"] <- "QU7GJ1005003"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="UNMU1001001"] <- "QUNMU1001001"
# mistakes foudn when linking to F4
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QIUCS3002001"] <- "QU1CS3002001"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QUTNM1028001"] <- "QUFNM1028001"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0CS3001002"] <- "2021-05-10"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0CS3004001"] <- "2021-04-01"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0CS3004004"] <- "2021-04-01"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0CS3004007"] <- "2021-04-01"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0GJ1003002"] <- "2021-05-07"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0GJ1008001"] <- "2021-05-05"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU0GT1001002"] <- "QU0GJ1001002"
DBSinventory$openhdsindividualId[DBSinventory$openhdsindividualId=="QU0GT1001009"] <- "QU0GJ1001009"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0NM4004001"] <- "2021-05-07"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0NM4004003"] <- "2021-05-07"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0NM4005001"] <- "2021-05-10"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0NM4007002"] <- "2021-04-01"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0NM4008001"] <- "2021-05-10"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0NM4009008"] <- "2021-05-05"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0PC1001001" & DBSinventory$seq==1] <- "2021-04-01"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0PC1001001" & DBSinventory$seq==2] <- "2021-06-21"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0PC1002003"] <- "2021-05-07"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0PC1012004"] <- "2021-05-10"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0PC1013005"] <- "2021-05-12"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0QM1002001"] <- "2021-05-10"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0QM1002007"] <- "2021-05-10"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0QM1003001"] <- "2021-05-10"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0QM1004004"] <- "2021-05-10"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0QM1005007"] <- "2021-05-10"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0QM1006001"] <- "2021-05-05"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU0RT1010001"] <- "2021-05-05"
DBSinventory$datacolheita[is.na(DBSinventory$datacolheita) & DBSinventory$openhdsindividualId=="QU1CS3002001"] <- "2021-04-06"
DBSinventory$datacolheita[DBSinventory$openhdsindividualId=="QU5PC1039002" & is.na(DBSinventory$datacolheita)] <- "2021-03-04"
DBSinventory$datacolheita[DBSinventory$openhdsindividualId=="QU5PC1039003" & is.na(DBSinventory$datacolheita)] <- "2021-03-04"
DBSinventory$datacolheita[DBSinventory$openhdsindividualId=="QU5PC1039005" & is.na(DBSinventory$datacolheita)] <- "2021-03-04"
DBSinventory$datacolheita[DBSinventory$openhdsindividualId=="QUMMU1009003" & is.na(DBSinventory$datacolheita)] <- "2021-03-04"

# remove duplicate entries
dups <- which(duplicated(DBSinventory%>%filter(!is.na(datacolheita))%>%select(openhdsindividualId,datacolheita)))
length(dups)
DBSinventory <- DBSinventory %>% filter(!row.names(DBSinventory) %in% dups)

# # limit to 28 June (from 29 June in DBS follow-up file)
# DBSdec20jun21 <- DBSinventory %>% filter(!is.na(datacolheita)&datacolheita<"2021-06-29") # only missing after 30 June
# 
# ## import file of the next 8 months of the study (29 June 21 to 22 Feb 22)
# DBSjun21feb22 <- read_excel("database/20220323/Seguimento das amostras Africover.xlsx", 
#                           sheet = "DBS")
# DBSjun21feb22$`Data de colheita`[DBSjun21feb22$`Data de colheita`=="4-Sep-2021\r\n"] <- "4-Sep-2021"
# DBSjun21feb22$`Data de colheita`<- tolower(DBSjun21feb22$`Data de colheita`)
# DBSjun21feb22$datacolheita <- NA
# DBSjun21feb22$datacolheita[grepl("44", DBSjun21feb22$`Data de colheita`)==TRUE] <- DBSjun21feb22$`Data de colheita`[grepl("44", DBSjun21feb22$`Data de colheita`)==TRUE]
# DBSjun21feb22$datacolheita <- as.numeric(DBSjun21feb22$datacolheita)
# DBSjun21feb22$datacolheita <- as.Date(DBSjun21feb22$datacolheita, origin = "1899-12-30")
# DBSjun21feb22$datacolheita[grepl("44", DBSjun21feb22$`Data de colheita`)==F] <- as.Date(DBSjun21feb22$`Data de colheita`[grepl("44", DBSjun21feb22$`Data de colheita`)==F], "%d-%b-%Y")
# # it did not recognize "oct", so I changed manually in the excel file
# # 2 dates were not converted to date format, so changing manually
# DBSjun21feb22$datacolheita[DBSjun21feb22$`Data de colheita`=="13/10/2021"] <- "2021-10-13"
# DBSjun21feb22$datacolheita[DBSjun21feb22$`Data de colheita`=="27/10/2021"] <- "2021-10-27"
# 
# # samples that were reported as not received but were received later
# DBSjun21feb22$Comentários...9[DBSjun21feb22$`ID do participante`=="QUHMU1027005"&DBSjun21feb22$datacolheita=="2021-08-02"] <- "QUHMU1027005"
# DBSjun21feb22$`Data de recepção`[DBSjun21feb22$`ID do participante`=="QUHMU1027005"&DBSjun21feb22$datacolheita=="2021-08-02"] <- "2021-09-14"
# # there is one sample of participantID QU4CS3005005 received on 2021-09-29, which I can't find back, but there is a F4 entry for QU4CS3010001 on that same day, which I can't find in the follow-up. Probably typo.
# # search the rows without datacolheita 
# DBSjun21feb22nocollectiondate <- subset(DBSjun21feb22, is.na(DBSjun21feb22$datacolheita)) # I checked them. QUHMU1027005 and QUMNM4014006 are mistakes (participants also entered a day before or after)
# DBSjun21feb22 <- subset(DBSjun21feb22, !is.na(DBSjun21feb22$datacolheita))
# 
# # rename variables
# colnames(DBSjun21feb22) <- c("datacolheita_old", "participantID_ODK", "dob", "id_origin", "id_correct", "shippingdate","comments","reception_date", "openhdsindividualId","box","ziplocknr","storagedate","unfrozen_n","blank","discrepancies_ID_ODK_&samplereception","datacolheita")
# 
# # keep the ODK participantID for those marked as 'not received', in case they still turned up
# DBSjun21feb22$openhdsindividualId[grepl("RECEBI", DBSjun21feb22$openhdsindividualId)==TRUE] <- DBSjun21feb22$participantID_ODK
# # remove the duplicate or missing ODK entries
# DBSjun21feb22 <- subset(DBSjun21feb22, grepl("DUPLIC", DBSjun21feb22$openhdsindividualId)==F)
# DBSjun21feb22 <- subset(DBSjun21feb22, grepl("NAO ENCONTR", DBSjun21feb22$openhdsindividualId)==F)
# # replace errors in participant ID
# DBSjun21feb22$openhdsindividualId[DBSjun21feb22$openhdsindividualId=="QU00NM4009008"] <- "QU0NM4009008"
# 
# # remove unnecessary variables
# DBSjun21feb22 <- DBSjun21feb22 %>% select(openhdsindividualId, dob, ziplocknr, datacolheita)
# 
# # remove duplicate rows
# dups = which(duplicated(DBSjun21feb22%>%select(openhdsindividualId,dob,datacolheita)))
# length(dups)
# DBSjun21feb22dups <- DBSjun21feb22 %>% filter(row.names(DBSjun21feb22) %in% dups)
# DBSjun21feb22 <- DBSjun21feb22 %>% filter(!row.names(DBSjun21feb22) %in% dups)
# 
# # combine DBSdec20jun21 and DBSjun21feb22 to have a single list of participant IDs with dates of sample collection
# DBSdec20jun21$dob <- NA
# DBSlist <- rbind(DBSdec20jun21,DBSjun21feb22)
# 
# count the how many-th sample it is of a single participant
DBSinventory$v1 <- 1
DBSinventory$seq <- ave(DBSinventory$v1, DBSinventory$openhdsindividualId, FUN = seq_along)
DBSinventory <- subset(DBSinventory, select = c(-v1))

## ODK F4 serosurveys (short questionnaire completed at the time of the visit for sample collection)
# F4_v1 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F4_Serovigilancia_v1_0.csv")
# F4_v2 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F4_Serovigilancia_v2_0.csv")
# F4 <- rbind(F4_v1, F4_v2)
# # change format variable date 
# F4$data_colheita <- as.Date(F4$sample_collection_date, "%b %d, %Y")
# F4 <- read_excel("database/20220323/F4.xlsx")
F4 <- xl.read.file("database/20221123/Africover F4 Serosurvey_full_DB.xlsx", password = "africover_1")
F4$datacolheita <- as.Date(F4$sample_collection_date)
F4$start <- as.Date(F4$start)

# remove entries when no sample was collected
F4 <- F4 %>% filter(sample_colected!="Não") %>% filter(visit_done!=" Não") %>% select(start, individualid, sero_survey, vacinated, doses_number, dose_1_date, dose_2_date, dose_3_date, datacolheita)

# check dates that don't make sense and replace collection date with questionnaire start date
F4$datacolheita[F4$datacolheita=="2020-03-07"] <- "2021-06-18"

# codes that were wrong after checking matches with the samples
F4$individualid[F4$individualid=="QU1CS3001002"&F4$datacolheita=="2021-04-06"] <- "QU1CS3001013" 

# find dates of collection long after the questionnaire was completed and therefore do not make sense (limit put at 7days)
F4$time_F_to_collection <- F4$datacolheita - F4$start
table(F4$time_F_to_collection)

# replace dates if difference >8. at that point, a mixup of month and day is probable
F4$data_colheita[!is.na(F4$time_F_to_collection) & as.numeric(F4$time_F_to_collection)< -8] <- F4$start[!is.na(F4$time_F_to_collection) & as.numeric(F4$time_F_to_collection) < -8]

# F4$data_colheita[(F4$time_F_to_collection*-1)>20] <- F4$data_formulario[(F4$time_F_to_collection*-1)>20]

# remove duplicate entries in F4 odk
# check duplicated rows
dups = which(duplicated(F4%>%select(individualid,datacolheita)))
length(dups)
# Remove duplicated rows
F4 = F4 %>% filter(!row.names(F4) %in% dups)


# Merge odk and lab databases based on ID and date of collection
DBSinventorysimpl <- DBSinventory %>% select(openhdsindividualId, sex, age, datacolheita, seq, boxnr, boxpositionnr)
serosurveymerged <- merge(DBSinventorysimpl, F4, by.x = c("openhdsindividualId","datacolheita"), by.y = c("individualid", "datacolheita"), all = T) # only keeping the samples in the lab though
# Group the dataframe by 'individualid'
serosurveymissing <- serosurveymerged %>% 
  filter(is.na(seq) | is.na(start)) %>% # only for observations with missing data, so either F4 or inventory missing
  group_by(openhdsindividualId)

# For each 'individualid', fill in missing values with non-missing values within the group
serosurveymissing_combined <- serosurveymissing %>% 
  mutate(across(everything(), ~ifelse(all(is.na(.)), NA, na.omit(.))))

# Drop duplicated rows that might be created
serosurveymissing_combined <- distinct(serosurveymissing_combined)
serosurveymissing_combined <- as.data.frame(serosurveymissing_combined)

# If you want to remove rows where all variables are missing for an individualid
# df_combined <- df_combined %>% filter(!all(is.na(.)))
# # Remove the grouping (optional)
# serosurveymissing_combinedungrouped <- ungroup(serosurveymissing_combined)

# combine with the observations that were immediately merged
serosurveymerged <- serosurveymerged %>% filter(!is.na(seq)&!is.na(start))
serosurveymissing_combined$datacolheita <- as.Date(serosurveymissing_combined$datacolheita, origin = "1970-01-01")
serosurveymissing_combined$start <- as.Date(serosurveymissing_combined$start, origin = "1970-01-01")
serosurveymissing_combined$dose_1_date <- as.Date(serosurveymissing_combined$dose_1_date, origin = "1970-01-01")
serosurveymissing_combined$dose_2_date <- as.Date(serosurveymissing_combined$dose_2_date, origin = "1970-01-01")
serosurveymerged$dose_1_date <- as.Date(serosurveymerged$dose_1_date, origin = "1970-01-01")
serosurveymerged$dose_2_date <- as.Date(serosurveymerged$dose_2_date, origin = "1970-01-01")
serosurvey_noresults <- rbind(serosurveymerged, serosurveymissing_combined)

# redo seq now that most ODK entries and inventory rows have been linked
serosurvey_noresults <- serosurvey_noresults %>%
  arrange(datacolheita) %>%                      # Sort the dataframe by datacolheita
  group_by(openhdsindividualId) %>%             # Group by openhdsindividualId
  mutate(seq = row_number())                    # Create a sequence number within each group

# 4.2. Import DBS results
# DBS results (do not contain data de colheita, which we will have to add afterwards)
serosurveyresults <- read_excel("database/20220323/DBSresults220318.xlsx", 
                         sheet = "All Data Info.")
# remove rows of control panel results (not needed because the raw values have already been interpreted by Joachim)
serosurveyresults <- subset(serosurveyresults, participantsample==1)

# remove other variables
serosurveyresults <- serosurveyresults %>% select(participantID, plate, Result)
# remove errors in IDs
serosurveyresults$participantID[serosurveyresults$participantID=="QUOGJ1003009"] <- "QUOGJ1003009"
serosurveyresults$participantID[serosurveyresults$participantID=="QUPC1009001"] <- "QU1PC1009001" # check later again, could also be QULPC1009001
# QURT10210033 can't find which sample this should be linked to
serosurveyresults$participantID[serosurveyresults$participantID=="QUSCS3038006"] <- "QU5CS3038006"
serosurveyresults$participantID[serosurveyresults$participantID=="QUUFCS1007005"] <- "QUFCS1007005"
serosurveyresults$participantID[serosurveyresults$participantID=="QUZGJ1005003"] <- "QU7GJ1005003"
serosurveyresults$participantID[serosurveyresults$participantID=="QU0GT1001002"] <- "QU0GJ1001002"
serosurveyresults$participantID[serosurveyresults$participantID=="QU0GT1001009"] <- "QU0GJ1001009"
serosurveyresults$participantID[serosurveyresults$participantID=="QU2GT1006001"] <- "QU2GJ1006001"
serosurveyresults$participantID[serosurveyresults$participantID=="QU3QC1034010"] <- "QU3PC1034010"
serosurveyresults$participantID[serosurveyresults$participantID=="QU4CS30033001"] <- "QU4CS3003001"
serosurveyresults$participantID[serosurveyresults$participantID=="QU5GT1003004"] <- "QU5GJ1003004"
serosurveyresults$participantID[serosurveyresults$participantID=="QU5NM405506"] <- "QU5NM4055006"
serosurveyresults$participantID[serosurveyresults$participantID=="QU7C1004006"] <- "QU7PC1004006"
serosurveyresults$participantID[serosurveyresults$participantID=="QU7PC1117003"] <- "QU7PC1017003"
serosurveyresults$participantID[serosurveyresults$participantID=="QU7QM2025006"] <- "QU7QM1025006"
serosurveyresults$participantID[serosurveyresults$participantID=="QUCP1008001"] <- "QUCPC1008001"
serosurveyresults$participantID[serosurveyresults$participantID=="QUFM209701011"] <- "QUFQM2097011"
serosurveyresults$participantID[serosurveyresults$participantID=="QUFNM103000"] <- "QUFNM1030003" # could also be QUFNM103001, sample taken the same day but no result found
serosurveyresults$participantID[serosurveyresults$participantID=="QUFQL1030002"] <- "QUFQM1030002"
serosurveyresults$participantID[serosurveyresults$participantID=="QUFQN1029007"] <- "QUFQM1029007"
serosurveyresults$participantID[serosurveyresults$participantID=="QUHM1026002"] <- "QUHQM1026002"
serosurveyresults$participantID[serosurveyresults$participantID=="QUHM2053002"] <- "QUHQM2053002"
serosurveyresults$participantID[serosurveyresults$participantID=="QUHUM1012001"] <- "QUHMU1012001"
serosurveyresults$participantID[serosurveyresults$participantID=="QUJQN1009001"] <- "QUJQM1009001"
serosurveyresults$participantID[serosurveyresults$participantID=="QUKAM1008006"] <- "QULGJ1006008"
serosurveyresults$participantID[serosurveyresults$participantID=="QULNM1006005"] <- "QULNM2006005"
serosurveyresults$participantID[serosurveyresults$participantID=="QUMM1002005"] <- "QUMMU1002005"
serosurveyresults$participantID[serosurveyresults$participantID=="QUNM1003002"] <- "QUNMU1003002"
serosurveyresults$participantID[serosurveyresults$participantID=="QUOGJ1003009"] <- "QU0GJ1003009"
serosurveyresults$participantID[serosurveyresults$participantID=="QURT10210033"] <- "QU5RT1021003"

# check duplicated rows
dups = which(duplicated(serosurveyresults%>%select(participantID, plate, Result)))
length(dups)
# Remove duplicated rows
serosurveyresults <- serosurveyresults %>% filter(!row.names(serosurveyresults) %in% dups) 

# remove capital letters in results
serosurveyresults$Result <- tolower(serosurveyresults$Result)

# count which occurrence it is of the participant
serosurveyresults$v1 <- 1
serosurveyresults$seq <- ave(serosurveyresults$v1, serosurveyresults$participantID, FUN = seq_along)
serosurveyresults <- subset(serosurveyresults, select = c(-v1))

# summarize how many samples per participant
summary_participants <- serosurveyresults %>%
  group_by(participantID) %>%
  summarise(n=n())
table(summary_participants$n)

# link DBS results to dates
serosurvey_noresults$sampleID <- paste(serosurvey_noresults$openhdsindividualId,"-",serosurvey_noresults$seq)
serosurveyresults$sampleID <- paste(serosurveyresults$participantID,"-",serosurveyresults$seq)
serosurvey <- merge(serosurveyresults, serosurvey_noresults, by = "sampleID", all = T)

# mark those with probably enter errors
serosurvey$comment[is.na(serosurvey$openhdsindividualId)] <- "check ID"

# remove unnecessary variables
serosurvey <- serosurvey %>% select(-c("seq.x","seq.y"))
# if individualid missing, then use participantID
serosurvey$participantID[is.na(serosurvey$participantID)] <- serosurvey$openhdsindividualId[is.na(serosurvey$participantID)]

# add var agegroup and agegr10
serosurvey$agegr[serosurvey$age<18] <- "0-17"
serosurvey$agegr[serosurvey$age>17&serosurvey$age<50] <- "18-49"
serosurvey$agegr[serosurvey$age>49] <- ">=50"
serosurvey$agegr <- factor(serosurvey$agegr, levels = c("0-17", "18-49", ">=50"))

# export to check
write.table(serosurvey, file = "serosurvey.txt")
write.csv(serosurvey, file = "serosurvey.csv")

# results with missing inventory or F4
serosurveymissingF4 <- serosurvey %>%
  filter(is.na(openhdsindividualId))
write_xlsx(serosurveymissingF4, "serosurveymissingF4.xlsx")
