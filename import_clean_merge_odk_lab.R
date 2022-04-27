#############################################################################
# AFRICOVER                                                                 #
# Script to merge ODK and lab databases                                     #
#############################################################################

# install/load packages
pacman::p_load(readxl,dplyr,lubridate, ggplot2, usethis)

#### 1. DEMOGRAPHIC DATA ####
# Import demographic data
demographics <- read_excel("database/Demographics_database_unlocked.xlsx", sheet = "Sheet1") # no duplicates (checked)
demographics$dob <- as.Date(demographics$DOB, "%m/%d/%Y")
dob <- subset(demographics, select = c("openhdsindividualId","DOB", "GENDER"))
baseline_demographics <- read_excel("database/Baseline_demographics.xlsx")
baseline_demographics$openhdsindividualId <- baseline_demographics$`individualInfo:individualId`
baseline_demographics$dob <- as.Date(baseline_demographics$`individualInfo:dateOfBirth`)
baseline_demographics$GENDER <- baseline_demographics$`individualInfo:gender` 
baseline_demographics <- baseline_demographics %>%
  select(openhdsindividualId, dob, GENDER)
# Remove duplicated rows
dups = which(duplicated(baseline_demographics%>%select(openhdsindividualId, dob))) # CHECK - same participantID, different dob
baseline_demographics = baseline_demographics %>% filter(!row.names(baseline_demographics) %in% dups)

#### 2. BASELINE AFRICOVER DATA ####
# F1 baseline
# household data
F1a <- read_excel("database/20220323/F1a.xlsx")
F1a$startdate <- as.Date(F1a$start)
F1a <- F1a %>%
  filter(startdate<"2021-05-01")
# F1a_v1 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F1a_Base_Agregado_familiar_v1_0.csv")
# F1a_v1$motive <- NA
# F1a_v1$especify_motive <- NA
# F1a_v1$is_consent_signed <- NA
# F1a_v2 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F1a_Base_Agregado_familiar_v2_0.csv")
# F1a <- rbind(F1a_v1, F1a_v2)
# Check if duplicated rows (i.e. same index and same HRC)
dups = which(duplicated(F1a%>%select(openhdsindividualId,is_consent_signed, household_chracteristicsbedroom, household_chracteristicswashto)))
length(dups)
# Remove duplicated rows
F1a = F1a %>% filter(!row.names(F1a) %in% dups)
# individual participant data
F1b <- read_excel("database/20220323/F1b.xlsx")
# F1b_v1 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F1b_Base_Individual_v1_0.csv")
# F1b_v1$motive <- NA
# F1b_v1$especify_motive <- NA
# F1b_v1$is_consent_signed <- NA
# F1b_v2 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F1b_Base_Individual_v2_0.csv")
# F1b <- rbind(F1b_v2, F1b_v1)
F1b$start <- as.Date(F1b$start)
# Check if duplicated rows (i.e. same index and same HRC)
dups = which(duplicated(F1b%>%select(openhdsindividualId, is_consent_signed, exposuremass_bus, drugssmoking)))
length(dups)
# STILL NEED TO REMOVE OBSERVATIONS FROM PARTICIPANTS WHO INITIALLY REFUSED TO PARTICIPATE, BUT LATER WERE APPROACHED AGAIN AND THEN A FORM WAS COMPLETED
# Remove duplicated rows
F1b = F1b %>% filter(!row.names(F1b) %in% dups)
# merge F1a & F1b
F1 <-merge(F1b, F1a, by="openhdslocationId", all.x = T)

# check HH with no individual (F1b) data
F1awithoutF1b <- F1 %>%
  filter(is.na(start.y)) %>%
  select(openhdsindividualId.x)
write.table(F1awithoutF1b, "F1awithoutF1b.txt")

# merge with demographic data
participants <- merge(demographics, F1, by.x = "openhdsindividualId", by.y = "openhdsindividualId.x", all.y = T)

# check those for which no demographic data
missingdemographicdata <- participants %>%
  filter(is.na(dob)) %>%
  select(openhdsindividualId)
write.table(missingdemographicdata, "missingdemographicdata.txt")

# when no demographic data, add date of birth and sex
participants <- merge(participants, baseline_demographics, by = "openhdsindividualId", all.x = T)
participants$dob <- participants$dob.x
participants$dob[is.na(participants$dob)] <- participants$dob.y[is.na(participants$dob)] 
participants$sex <- participants$GENDER.x
participants$sex[is.na(participants$sex)] <- participants$GENDER.y[is.na(participants$sex)]

# add age groups
participants$age <- round(as.numeric((participants$start.x - participants$dob))/365.25,0)
participants$agegr[participants$age<18] <- "0-17"
participants$agegr[participants$age>17&participants$age<50] <- "18-49"
participants$agegr[participants$age>49] <- "50+"
table(participants$agegr, useNA = "always")
table(participants$sex, useNA = "always")

# list of those without age nor sex
missingage_sex <- participants %>%
  filter(is.na(sex)) %>%
  select(openhdsindividualId)
write.table(missingage_sex, "missingage_sex.txt")

# keep only variables of interest
participants_simplified <- participants %>%
  select(openhdsindividualId, sex, age, dob, agegr, linguasportugues1, ecestado_civil, trabtrabalho, SesScoreQnt, empsituacao_emprego, ednivel_educacao, drugshiv, drugscancer, drugsdiabet, drugshypertension, drugschronic_kidney_disease, drugschronic_lung_disease) 
# more variables to add here, but it would be easier from a dataframe with strings instead of numbers for factor variables

# F3 weight & height
F3 <- read_excel("database/20220323/F3.xlsx")
# Check if duplicated rows (i.e. same index and same HRC)
dups = which(duplicated(F3%>%select(openhdsindividualId, measurementsheight, measurementsarm_circumference, measurementsweight)))
length(dups)
# Remove duplicated rows
F3 = F3 %>% filter(!row.names(F3) %in% dups)
# make a dataset that combines observations of weight, height and MUAC during different visits (if weight is collected during a different visit than height)
F3_weightheightcombined <- F3 %>%
  filter(measurementswas_weighed==1|measurementswas_arm_circumferen==1|measurementswas_height_measured==1)%>%
  group_by(openhdsindividualId) %>%
  summarize(weight=mean(measurementsweight), height=mean(measurementsheight), MUAC=mean(measurementsarm_circumference))
hist(F3_weightheightcombined$weight)
hist(F3_weightheightcombined$height)
# BMI
F3_weightheightcombined$BMI <- round(F3_weightheightcombined$weight/((F3_weightheightcombined$height/100)^2),1)

# merge participant baseline data and weight and height
participants <- merge(participants_simplified, F3_weightheightcombined, by = "openhdsindividualId", all.x = T) # only 3684 out of 6807
# identify those with weight and height that doesn't make sense
list_weight_height_problems <- participants %>%
  filter((BMI<10|BMI>35) & age>4) %>%
  select(openhdsindividualId, sex, age, weight, height, BMI, MUAC)
write.table(list_weight_height_problems, file = "list_weight_height_problems.txt")

# Check if duplicated rows (i.e. same index and same HRC)
dups = which(duplicated(participants_simplified%>%select(openhdsindividualId)))
length(dups)
# Remove duplicated rows
participants_simplified = participants_simplified %>% filter(!row.names(participants_simplified) %in% dups)

# export participant database FOR NOW SIMPLIFIED WITHOUT F3 - SHOULD BE CHANGED LATER ON
write.csv(participants_simplified, file = "participants_simplified.csv")

#### 3. ACTIVE SURVEILLANCE FOR POSSIBLE CASES ####
## 3.1 ODK possible case reports
# F5_v1 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F5_Possivel_caso_v1_0.csv")
# F5_v2 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F5_Possivel_caso_v2_0.csv")
# F5 <- rbind(F5_v1, F5_v2)
F5 <- read_excel("database/20220323/F5.xlsx")
# Check if duplicated rows (i.e. same index and same HRC)
dups = which(duplicated(F5%>%select(`openhds-individualId`, `situacao_clinica-data_inicio_sintomas`, `situacao_clinica-symptoms_start_date`, `nasal_swab_data-nasal_swab_date`)))
length(dups)
# Remove duplicated rows
F5 = F5 %>% filter(!row.names(F5) %in% dups)
# create var for ID and date to link to results
F5$datacolheita <- as.Date(F5$`nasal_swab_data-nasal_swab_date`) # 64 no sample collection date, in most cases because tested at hospital
table(F5$`nasal_swab_data-specify_reason_for_not_sampling`)

## 3.2 nasal swab lab results
nasalswabresults <- read_excel("database/completed 20220420/AfriCoVER_nasalswabresults_20220420.xlsx", sheet = "Detalhes dos Resultados ", 
                                 col_types = c("text", "text", "text", "text", "date", "date", "numeric", "text", "date", "text", "date", "date", 
                                               "text", "text", "text", "date", "text", "text"))
hist(nasalswabresults$datacolheita, breaks = 20)
nasalswabresults$datacolheita <- as.Date(nasalswabresults$datacolheita)
nasalswabresults$openhdsindividualId <- nasalswabresults$`Member ID`

## 3.3 merge lab results and ODK F4
# clean observations to match between both db
F5$datacolheita[F5$`openhds-individualId`=="QU0QM1004005"] <- "2022-02-01" #date missing in F5
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

# create common variable  
F5$individualId_date <- paste(F5$`openhds-individualId`,"_",F5$datacolheita)
nasalswabresults$individualId_date <- paste(nasalswabresults$`Member ID`,"_",nasalswabresults$datacolheita)

# merge
possiblecases <- merge(F5, nasalswabresults, by = "individualId_date", all=T) 
#export to check where merging went wrong
write.csv(possiblecases, "possiblecases.csv")
possiblecases_simplified <- possiblecases %>%
  select(individualId_date, ID, datacolheita.x, Resultado, datacolheita.y)
# remove observations without data
possiblecases <- subset(possiblecases, possiblecases$individualId_date!="NA _ 2021-02-24")

# link possible cases and participant data
possiblecases_participants <- merge(possiblecases, participants, by = "openhdsindividualId")
# export database
write.csv(possiblecases_participants, file = "possiblecases_participants.csv")

## 3.4 F2 active surveillance follow-up
# household part
F2 <- read_excel("database/20220323/F2.xlsx")
# Check if duplicated rows (i.e. same index and same HRC)
dups = which(duplicated(F2%>%select(start, openhdsindividualId, visit_confirmationcontact_type, visit_confirmationcontact_date, KEY)))
length(dups)
# Remove duplicated rows
F2 <- F2 %>% filter(!row.names(F2) %in% dups) # n=9959
# check how often symptomatic cases were reported in the HH
table(F2$exposure_symptomsrespiratory_sy) # 407 HH visits with (at least one) episode reported
# format date of symptom onset
F2$datacolheita <- as.Date(F2$history_symptoms_date_colected_)
F2$datacolheita[is.na(F2$datacolheita)] <- as.Date(F2$history_symptomsdate_colected[is.na(F2$datacolheita)])
table(F2$datacolheita, useNA = "always")
F2$datesymptomonset <- as.Date(F2$history_symptoms_symptoms_date_)
table(F2$datesymptomonset, useNA = "always")
# individual HH member part (episodes of illness)
F2episodes <- read_excel("database/20220323/F2_symptoms.xlsx")
# Check if duplicated rows (i.e. same index and same HRC)
dups = which(duplicated(F2episodes%>%select(history_symptoms_individualID_r, KEY)))
length(dups)
# Remove duplicated rows
F2episodes <- F2episodes %>% filter(!row.names(F2episodes) %in% dups) # only n=106 remaining, something must have gone wrong during data collection CHECK

# merge HH visit data with episodes
F2_mergedepisodes <- merge(F2, F2episodes, by = "KEY", all = T)

# link active surveillance visits and possible cases
# create unique variable for the HH during that visit
F2$HH_datacolheita <- NA
F2$HH_datacolheita[!is.na(F2$openhdslocationId)&!is.na(F2$datacolheita)] <- paste(F2$openhdslocationId[!is.na(F2$openhdslocationId)&!is.na(F2$datacolheita)],"_",F2$datacolheita[!is.na(F2$openhdslocationId)&!is.na(F2$datacolheita)])
# create unique var for the HH where a sample is collected
possiblecases_participants$HH_datacolheita <- NA
possiblecases_participants$HH_datacolheita[!is.na(possiblecases_participants$openhds.locationId)&!is.na(possiblecases_participants$datacolheita.x)] <- paste(possiblecases_participants$openhds.locationId[!is.na(possiblecases_participants$openhds.locationId)&!is.na(possiblecases_participants$datacolheita.x)],"_",possiblecases_participants$datacolheita.x[!is.na(possiblecases_participants$openhds.locationId)&!is.na(possiblecases_participants$datacolheita.x)] )
# merge both, removing possible cases not directly linked to a visit (CHECK)
activesurveillance <- merge(F2, possiblecases_participants, by = "HH_datacolheita", all.x = T)


#### 4. SEROSURVEILLANCE ####
# F4 serosurveys
# F4_v1 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F4_Serovigilancia_v1_0.csv")
# F4_v2 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/AfriCoVER/database/completed 20220420/AfriCoVER_F4_Serovigilancia_v2_0.csv")
# F4 <- rbind(F4_v1, F4_v2)
# # change format variable date 
# F4$data_colheita <- as.Date(F4$sample_collection_date, "%b %d, %Y")
F4 <- read_excel("database/20220323/F4.xlsx")
F4$data_colheita <- as.Date(F4$sample_confirmsample_collection)
F4$data_formulario <- as.Date(F4$start)
# check dates that don't make sense and replace collection date with questionnaire start date
F4$data_colheita[F4$data_colheita=="2020-03-07"] <- "2021-06-18"
F4$time_F_to_collection <- F4$data_colheita - F4$data_formulario
table(F4$time_F_to_collection)
# F4$data_colheita[(F4$time_F_to_collection*-1)>20] <- F4$data_formulario[(F4$time_F_to_collection*-1)>20]

# remove duplicate entries in F4 odk
# Check if duplicated rows (i.e. same index and same HRC)
dups = which(duplicated(F4%>%select(openhdsindividualId,data_colheita)))
length(dups)
# Remove duplicated rows
F4 = F4 %>% filter(!row.names(F4) %in% dups)

# Import DBS results
# DBS results (do not contain data de colheita, which we will have to add afterwards)
DBSresults <- read_excel("database/20220323/DBSresults220318.xlsx", 
                         sheet = "All Data Info.")
# remove rows of control panel results (as raw values have already been interpreted by Joachim)
DBSresults <- subset(DBSresults, participantsample==1)
# remove other variables
DBSresults <- DBSresults %>% select(participantID, plate, Result)
# Check if duplicated rows (i.e. same index and same HRC)
dups = which(duplicated(DBSresults%>%select(participantID, plate, Result)))
length(dups)
# Remove duplicated rows
DBSresults <- DBSresults %>% filter(!row.names(DBSresults) %in% dups) 
# remove capital letters in results
DBSresults$Result <- tolower(DBSresults$Result)
# count which occurrence it is of the participant
DBSresults$v1 <- 1
DBSresults$seq <- ave(DBSresults$v1, DBSresults$participantID, FUN = seq_along)
DBSresults <- subset(DBSresults, select = c(-v1))

# summarize how many samples per participant
summary_participants <- DBSresults %>%
  group_by(participantID) %>%
  summarize(n=n())
table(summary_participants$n)

# DBS inventory - for the first 6 months of the study (until 30 June)
DBS_inventario_INS <- read_excel("database/Africover lab DB 05072021/africover_inventario_DBS_210704.xlsx", 
                                 sheet = "Sheet1", col_types = c("text", "text", "text", "text", "text", "text", 
                                                                         "date", "text", "text", "text", "date", "numeric", "date"))
colnames(DBS_inventario_INS) <- c("box","ziplocknr","study","studyname","openhdsindividualId", "samplename","data_da_colheita_excelformat","sampletype","geolocation","province","data_congelacao","n_defrosts", "data_da_colheita")
DBS_inventario_INS$datacolheita <- as.Date(DBS_inventario_INS$data_da_colheita)
table(DBS_inventario_INS$data_da_colheita, useNA = "always") # 24 NA's
# mistakes in IDs
DBS_inventario_INS$openhdsindividualId[DBS_inventario_INS$openhdsindividualId=="QLNM3006002"] <- "QULNM3006002"
DBS_inventario_INS$openhdsindividualId[DBS_inventario_INS$openhdsindividualId=="UNMU1001001"] <- "QUJNM1001001"
# remove duplicate entries
dupslab = which(duplicated(DBS_inventario_INS%>%filter(!is.na(datacolheita))%>%select(openhdsindividualId,data_da_colheita)))
length(dupslab)
# Remove duplicated rows
DBS_inventario_INS <- DBS_inventario_INS %>% filter(!row.names(DBS_inventario_INS) %in% dupslab)
# remove unnecessary variables
DBS_inventario_INS <- DBS_inventario_INS %>% select(openhdsindividualId, ziplocknr, datacolheita)
# limit to 28 June (from 29 June in DBS follow-up file)
DBS_inventario_INS <- DBS_inventario_INS %>% filter(!is.na(datacolheita)&datacolheita<"2021-06-30") # only missing after 30 June

# DBS seguimento das amostras - for the second half of the study (from 29 July)
DBSfollowup <- read_excel("database/20220323/Seguimento das amostras Africover.xlsx", 
                          sheet = "DBS")
DBSfollowup$`Data de colheita`[DBSfollowup$`Data de colheita`=="4-Sep-2021\r\n"] <- "4-Sep-2021"
DBSfollowup$`Data de colheita`<- tolower(DBSfollowup$`Data de colheita`)
DBSfollowup$datacolheita <- NA
DBSfollowup$datacolheita[grepl("44", DBSfollowup$`Data de colheita`)==TRUE] <- DBSfollowup$`Data de colheita`[grepl("44", DBSfollowup$`Data de colheita`)==TRUE]
DBSfollowup$datacolheita <- as.numeric(DBSfollowup$datacolheita)
DBSfollowup$datacolheita <- as.Date(DBSfollowup$datacolheita, origin = "1899-12-30")
DBSfollowup$datacolheita[grepl("44", DBSfollowup$`Data de colheita`)==F] <- as.Date(DBSfollowup$`Data de colheita`[grepl("44", DBSfollowup$`Data de colheita`)==F], "%d-%b-%Y")
# it did not recognize "oct", so I changed manually in the excel file
# 2 dates were not converted to date format, so changing manually
DBSfollowup$datacolheita[DBSfollowup$`Data de colheita`=="13/10/2021"] <- "2021-10-13"
DBSfollowup$datacolheita[DBSfollowup$`Data de colheita`=="27/10/2021"] <- "2021-10-27"
# remove unnecessary variables and rename remaining vars
colnames(DBSfollowup) <- c("datacolheita_old", "participantID_ODK", "dob", "id_origin", "id_correct", "shippingdate","openhdsindividualId","reception_date", "comments2","box","ziplocknr","storagedate","unfrozen_n","blank","datacolheita")
DBSfollowup <- DBSfollowup %>% select(openhdsindividualId, dob, ziplocknr, datacolheita)
# remove duplicate rows
dups = which(duplicated(DBSfollowup%>%filter(!is.na(datacolheita))%>%select(openhdsindividualId,datacolheita)))
length(dups)
DBSfollowup <- DBSfollowup %>% filter(!row.names(DBSfollowup) %in% dups)
# remove samples that were in ODK but lab didn't receive
DBSfollowup$openhdsindividualId[grepl("RECEBI", DBSfollowup$openhdsindividualId)==TRUE] <- NA
DBSfollowup$openhdsindividualId[grepl("DUPLIC", DBSfollowup$openhdsindividualId)==TRUE] <- NA
DBSfollowup <- subset(DBSfollowup, !is.na(DBSfollowup$openhdsindividualId))


# combine DBS inventory and DBS follow-up to have a single list of participant IDs with dates of sample collection
DBS_inventario_INS$dob <- NA
DBSlist <- rbind(DBS_inventario_INS,DBSfollowup)

# count the how many-th sample it is of a single participant
DBSlist$v1 <- 1
DBSlist$seq <- ave(DBSlist$v1, DBSlist$openhdsindividualId, FUN = seq_along)
DBSlist <- subset(DBSlist, select = c(-v1))

# link DBS results to dates
DBSresults$sampleID <- paste(DBSresults$participantID,"-",DBSresults$seq)
DBSlist$sampleID <- paste(DBSlist$openhdsindividualId,"-",DBSlist$seq)
DBS <- merge(DBSresults, DBSlist, by = "sampleID", all = T)

# clean codes which aren't matching



# export to check
write.table(DBS, file = "DBS.txt")
write.csv(DBS, file = "DBS.csv")



# Merge odk and lab databases based on ID and date of collection
serosurveymerged <- merge(DBS_inventario_INS_nodups, F4, by = c("openhdsindividualId","data_da_colheita"), all.x = T) # only keeping the samples in the lab though
serosurveymerged_short <- subset(serosurveymerged, select = c("openhdsindividualId", "data_da_colheita", "roundodk"))

# Merge date of birth with merged odk-lab db
serosurveymerged_short_demographics <- merge(serosurveymerged_short, participants, by = "openhdsindividualId", all.x = T)
# export list
write.csv(serosurveymerged_short_demographics, "serosurveymerged_short.csv")


# Merge odk and lab databases based on ID and round
F4_nodups$round <- F4_nodups$roundodk
serosurveymerged_IDround <- merge(F4_nodups, DBS_inventorio_INS_nodups, by = c("openhdsindividualId","round"), all = T)
serosurveymerged_IDround_short <- subset(serosurveymerged_IDround, select = c("openhdsindividualId", "data_da_colheita.x", "data_da_colheita.y", "round"))
# export list
write.csv(serosurveymerged_IDround_short, "serosurveymerged_IDround_short.csv")

# Look for samples that were twice collected -> look again at the file before removing duplicates (could also be samples collected twice)
DBS_collected_several <- DBS_inventorio_INS %>%
  group_by(openhdsindividualId, round) %>%
  summarise(n=n())
# keep those with >1 sample collected per round
DBS_collected_several <- subset(DBS_collected_several, DBS_collected_several$n>1)
# add data de colheita -> merge on ID and round
DBS_collected_several_datacolheita <- merge(DBS_collected_several, DBS_inventorio_INS, by = c("openhdsindividualId","round"), all.x = T)
write.csv(DBS_collected_several_datacolheita, file = "DBS_collected_several_datacolheita.csv")

# descriptive
# DBS inclusions_by_age
serosurveymerged_short_demographics$age <- round(as.numeric((serosurveymerged_short_demographics$data_da_colheita - as.Date(serosurveymerged_short_demographics$`individualInfo:dateOfBirth`)))/365.25,0)
serosurveymerged_short_demographics$agegr[serosurveymerged_short_demographics$age<18] <- "<18"
serosurveymerged_short_demographics$agegr[serosurveymerged_short_demographics$age>17&serosurveymerged_short_demographics$age<50] <- "18-49"
serosurveymerged_short_demographics$agegr[serosurveymerged_short_demographics$age>49] <- ">/=50"

table(serosurveymerged_short_demographics$age)
# number of visits per DBS participant
DBSparticipants <- serosurveymerged_short_demographics %>%
  group_by(openhdsindividualId, age, agegr) %>%
  summarise(n=n())
DBSparticipants
count(DBSparticipants)
table(DBSparticipants$n)
# age distribution
hist(DBSparticipants$age, breaks = 20, main = NULL, xlab = "Age (years)")
prop.table(table(DBSparticipants$agegr))

# possible cases
possiblecases <- read_excel("database/Africover lab DB 05072021/Relatorio de Resultados das Amostras Nasais Agregados 01072021.xlsx", 
                            sheet = "Detalhes dos Resultados ")
possiblecases$openhdsindividualId <- possiblecases$`Member ID`
possiblecases_demographics <- merge(possiblecases, baseline_demographics, by = "openhdsindividualId", all.x = T)
possiblecases_demographics$age <- round(as.numeric((as.Date(possiblecases_demographics$`Data da Colheita`) - as.Date(possiblecases_demographics$`individualInfo:dateOfBirth`)))/365.25,0)
possiblecases_demographics$agegr[possiblecases_demographics$age<18] <- "<18"
possiblecases_demographics$agegr[possiblecases_demographics$age>17&possiblecases_demographics$age<50] <- "18-49"
possiblecases_demographics$agegr[possiblecases_demographics$age>49] <- ">/=50"
prop.table(table(possiblecases_demographics$agegr))
prop.table(table(possiblecases_demographics$agegr[possiblecases_demographics$Resultado=="Positivo"]))
table(possiblecases_demographics$Resultado)

# histogram possible cases
possiblecases_demographics$month <- month(as.Date(possiblecases_demographics$`Data da Colheita`), label = T)
possiblecases_demographics$Resultado[possiblecases_demographics$Resultado=="SI"] <- "Negativo"
monthlycount <- possiblecases_demographics %>%
  group_by(month, Resultado) %>%
  summarise(n=n())
monthlycount
possiblecaseshistogram <- ggplot(monthlycount, aes(x=month, y=n, fill=factor(Resultado))) +
  geom_col()+
  labs(title="", x = "", y="Number of possible cases") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        axis.text = element_text(size=14),
        axis.title=element_text(size=15),
        legend.text= element_text(size=10),
        title= element_text(size=15))
possiblecaseshistogram
