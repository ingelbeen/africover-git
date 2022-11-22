####################################################
# AFRICOVER                                        #
# MONTHLY SUMMARY REPORT                           #
####################################################
# last update 280521 by Brecht Ingelbeen

#### 0. import and clean data and packages ####
# required packages
pacman::p_load(readstata13, tidyverse, openxlsx, readxl,lubridate,purrr,dplyr,ggplot2, scales, ggthemes)
# import ODK data
# F1a <- read.dta13("database/DBAfricover11052021/F1A DB.dta", generate.factors=FALSE)
# F1b <- read.dta13("database/DBAfricover11052021/F1B DB.dta", generate.factors=FALSE)
# F2 <- read.dta13("database/DBAfricover11052021/F2 DB.dta", generate.factors=FALSE)
# F4 <- read.dta13("database/DBAfricover11052021/F4 DB.dta", generate.factors=FALSE)
# F5 <- read.dta13("database/DBAfricover11052021/F5 DB.dta", generate.factors=FALSE)
# F6 <- read.dta13("database/DBAfricover11052021/F6DB.dta", generate.factors=FALSE)
F1A_dups <- read_excel("database/DBAfricover11052021/F1A_duplicatedID_with_diferents_observations.xls")
F1A_no_duplicats <- read_excel("database/DBAfricover11052021/F1A_no_duplicats.xls")
F1a <- rbind(F1A_dups, F1A_no_duplicats)
F1a$participantID <- F1a$`Id do individuo`
F1B_dups <- read_excel("database/DBAfricover11052021/F1B_duplicatedID_with_diferents_observations.xls")
F1B_no_duplicats <- read_excel("database/DBAfricover11052021/F1B_noduplicats.xls")
F1b <- rbind(F1B_dups, F1B_no_duplicats)
F1b$participantID <- F1b$`Id do individuo`
# clean comorbidities
F1b$`Tem doenca pulmonar cronica ?`[F1b$`Outro estado de saude, especificar.`=="Asma"] <- "Sim, asma, mas nenhum tratamento disponivel" # one asthma entered, which should have been under chronic pulm disease
F1b$`Tem doenca pulmonar cronica ?`[F1b$`Especifique as outras imunodeficiencias conhecidas`=="Asma"] <- "Sim, asma, mas nenhum tratamento disponivel" # one asthma entered, which should have been under chronic pulm disease
F1b$`Tem cancro?`[F1b$`Especifique as outras imunodeficiencias conhecidas`=="Cancro de útero"] <- "Sim, qualquer outro cancro" 
F1b$`Tipo de cancro`[F1b$`Especifique as outras imunodeficiencias conhecidas`=="Cancro de útero"] <- "Utero" 
F1b$`Tem cancro?`[F1b$`Especifique as outras imunodeficiencias conhecidas`=="Tumor cerebral"] <- "Sim, qualquer outro cancro" 
F1b$`Tipo de cancro`[F1b$`Especifique as outras imunodeficiencias conhecidas`=="Cancro de útero"] <- "Utero" 
F1b$`Tem cancro?`[F1b$`Especifique as outras imunodeficiencias conhecidas`=="Sarcoma de caposseant"] <- "Sim, qualquer outro cancro" 
F1b$`Tipo de cancro`[F1b$`Tipo de cancro`=="Sarcoma de caposseant"] <- "Kaposi sarcoma" 
F1b$`Tipo de cancro`[F1b$`Tipo de cancro`=="Colo de utero"] <- "Utero"  
F1b$`Tipo de cancro`[F1b$`Tipo de cancro`=="Cancro do colo de útero"] <- "Utero"  
F1b$`Tipo de cancro`[F1b$`Tipo de cancro`=="Cancro do colo do útero"] <- "Utero"  
F1b$`Tipo de cancro`[F1b$`Tipo de cancro`=="Cancro de próstata"] <- "Prostata"  
# clean occupations
F1b$`Especifique a outra ocupacao` <- tolower(F1b$`Especifique a outra ocupacao`)
F1b$`Ocupacao principal`[grepl("cabele", F1b$`Especifique a outra ocupacao`)==T] <- "cabeleireiro"
F1b$`Ocupacao principal`[grepl("barbeiro", F1b$`Especifique a outra ocupacao`)==T] <- "cabeleireiro"
F1b$`Ocupacao principal`[grepl("vende", F1b$`Especifique a outra ocupacao`)==T] <- "vendedor ambulante"
F1b$`Ocupacao principal`[grepl("tem barraca em casa", F1b$`Especifique a outra ocupacao`)==T] <- "vendedor ambulante"
F1b$`Ocupacao principal`[grepl("tem banca em casa", F1b$`Especifique a outra ocupacao`)==T] <- "vendedor ambulante"
F1b$`Ocupacao principal`[grepl("comerciante", F1b$`Especifique a outra ocupacao`)==T] <- "vendedor ambulante"
F1b$`Ocupacao principal`[grepl("técnico", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador manual"
F1b$`Ocupacao principal`[grepl("tecnico informático", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("mecânico", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador manual"
F1b$`Ocupacao principal`[grepl("mecânica", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador manual"
F1b$`Ocupacao principal`[grepl("tratar documentação", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("trabalho administrativo", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("sem ocupação", F1b$`Especifique a outra ocupacao`)==T] <- "desempregado"
F1b$`Ocupacao principal`[grepl("reparador de telemóveis", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador manual"
F1b$`Ocupacao principal`[grepl("construtor", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador manual"
F1b$`Ocupacao principal`[grepl("construção", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador manual"
F1b$`Ocupacao principal`[grepl("educador", F1b$`Especifique a outra ocupacao`)==T] <- "professor"
F1b$`Ocupacao principal`[grepl("funcionária do sector privado", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("funcionário do sector privado", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("biscat", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador manual"
F1b$`Ocupacao principal`[grepl("agricultor", F1b$`Especifique a outra ocupacao`)==T] <- "agricultor"
F1b$`Ocupacao principal`[grepl("restauração", F1b$`Especifique a outra ocupacao`)==T] <- "funcionario de um restaurante, bar ou discoteca"
F1b$`Ocupacao principal`[grepl("barmen", F1b$`Especifique a outra ocupacao`)==T] <- "funcionario de um restaurante, bar ou discoteca"
F1b$`Ocupacao principal`[grepl("cosinheir", F1b$`Especifique a outra ocupacao`)==T] <- "funcionario de um restaurante, bar ou discoteca"
F1b$`Ocupacao principal`[grepl("cozinheir", F1b$`Especifique a outra ocupacao`)==T] <- "funcionario de um restaurante, bar ou discoteca"
F1b$`Ocupacao principal`[grepl("chefe de cozinha", F1b$`Especifique a outra ocupacao`)==T] <- "funcionario de um restaurante, bar ou discoteca"
F1b$`Ocupacao principal`[grepl("segurança", F1b$`Especifique a outra ocupacao`)==T] <- "segurança"
F1b$`Ocupacao principal`[grepl("seguranca", F1b$`Especifique a outra ocupacao`)==T] <- "segurança"
F1b$`Ocupacao principal`[grepl("vigilante", F1b$`Especifique a outra ocupacao`)==T] <- "segurança"
F1b$`Ocupacao principal`[grepl("guard", F1b$`Especifique a outra ocupacao`)==T] <- "segurança"
F1b$`Ocupacao principal`[grepl("guada", F1b$`Especifique a outra ocupacao`)==T] <- "segurança"
F1b$`Ocupacao principal`[grepl("secretaria", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("secretári", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("promotor", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("agente da movitel", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("contabilidade", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("contabilista", F1b$`Especifique a outra ocupacao`)==T] <- "trabalha num escritorio"
F1b$`Ocupacao principal`[grepl("reformado", F1b$`Especifique a outra ocupacao`)==T] <- "aposentada/o"
F1b$`Ocupacao principal`[grepl("medicina  tradicional", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador do sector da saude"
F1b$`Ocupacao principal`[grepl("medicina tradicional", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador do sector da saude"
F1b$`Ocupacao principal`[grepl("médica", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador do sector da saude"
F1b$`Ocupacao principal`[grepl("médico", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador do sector da saude"
F1b$`Ocupacao principal`[grepl("medico", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador do sector da saude"
F1b$`Ocupacao principal`[grepl("medica", F1b$`Especifique a outra ocupacao`)==T] <- "trabalhador do sector da saude"
F1b$`Ocupacao principal`[grepl("empregad", F1b$`Especifique a outra ocupacao`)==T] <- "empregado/a domestico/a."
F1b$`Ocupacao principal`[grepl("doméstic", F1b$`Especifique a outra ocupacao`)==T] <- "desempregado"
F1b$`Ocupacao principal`[grepl("domestic", F1b$`Especifique a outra ocupacao`)==T] <- "desempregado"
table(F1b$`Especifique a outra ocupacao`[F1b$`Ocupacao principal`=="outro (especificar):"])
table(F1b$`Ocupacao principal`)
# F2_dups <- read_excel("database/DBAfricover11052021/F2_duplicats_with_diferents_observations.xls")
# F2_no_duplicats <- read_excel("database/DBAfricover11052021/F2_noduplicats.xls")
# F2 <- rbind(F2_dups, F2_no_duplicats)
F2 <- read_excel("database/Africover DB 270521/F2.xls")
F2$participantID <- F2$`Id do individuo`
F4M0 <- read_excel("database/DBAfricover11052021/F4_M0_17052021.xls")
F4M0$participantID <- F4M0$`Id do individuo`
F4M3_dups <- read_excel("database/DBAfricover11052021/F4_M3_duplicats_whith_diferentsobservations.xls")
F4M3_no_duplicats <- read_excel("database/DBAfricover11052021/F4_M3_no_duplicats.xls")
F4M3 <- rbind(F4M3_dups, F4M3_no_duplicats)
F5_dups <- read_excel("database/DBAfricover11052021/F5_duplicatedID_whith_differents_observations.xls")
F5_no_duplicats <- read_excel("database/DBAfricover11052021/F5_no_duplicats.xls")
F5 <- rbind(F5_dups, F5_no_duplicats)
F5$participantID <- F5$`Id do individuo`
F6 <- read_excel("database/DBAfricover11052021/F6_no_duplicats.xls")
F8 <- read_excel("database/DBAfricover11052021/F8.xlsx")
nasalswabresults <- read_excel("database/IRAS_Nasal_swab_db_22052021.xlsx")
# participant ID typos
nasalswabresults$codusepi[nasalswabresults$codusepi=="QU1NM4004"] <- "QU1NM4004002"
nasalswabresults$codusepi[nasalswabresults$codusepi=="QU4G1009006"] <- "QU4GJ1009006"
nasalswabresults$codusepi[nasalswabresults$codusepi=="QU6G51009002"] <- "QU6GJ1009002"
nasalswabresults$codusepi[nasalswabresults$codusepi=="QU7CS3026007"] <- "QU7CS3026002"
nasalswabresults$codusepi[nasalswabresults$codusepi=="QUDRT1004001"] <- "QU0RT1004001"
nasalswabresults$codusepi[nasalswabresults$codusepi=="QUHMU10005003"] <- "QUHMU1005003"
nasalswabresults$codusepi[nasalswabresults$codusepi=="QUOPC1014001"] <- "QU0PC1014001"
# not found in possible case reports ODK: QU4CS3010003, 15, m, found in HDSS participants; QUFQM2079001, f, 67, found in HDSS participants; 
# QUCCS3009012, f, 26, collected 18/01/2021, not found in HDSS participants

# merge nasal swab ODK questionnaires and results
possiblecases <- merge(F5, nasalswabresults, by.x = "participantID", by.y = "codusepi", all = T)
# some for which there is a problem with the participantID
table(possiblecases$participantID[is.na(possiblecases$start)]) # participants for which there is no ODK record
table(possiblecases$participantID[is.na(possiblecases$ssdffcolda)]) # participants for which there is no result


# import dates of birth for all HDSS participants
HDSS_participants <- read_excel("database/Household members list HDSS questions HVL & answers Nilzio.xlsx", 
                                sheet = "All", range = "A1:D15898")
HDSS_participants$DOBdate <- as.Date(HDSS_participants$DOB)
HDSS_participants$ageyears <- floor((as.Date("2021-06-05") - HDSS_participants$DOBdate)/365.25) # at the middle of the study period
HDSS_participants$ageyears[HDSS_participants$ageyears>111] <- NA
table(HDSS_participants$ageyears, useNA = "always")
HDSS_participants$participantID <- HDSS_participants$`Household member id` 
HDSS_participants <- subset(HDSS_participants, select = c("participantID","Gender", "DOBdate","ageyears"))

# merge indiv participant bl data with age and with household data (F1a)
F1b <- merge(F1b, HDSS_participants, by = "participantID", all.x = T)
F1b <- merge(F1b, F1a, by = "participantID", all.x = T) # see that there are still 144 diplicates in F1a when merged to indicividuals

# merge serosurvey round M0 with baseline participant data
F4M0_bldata <- merge(F4M0, F1b, by = "participantID", all.x = T)

# merge possible cases with demographic data
possiblecases <- merge(possiblecases, HDSS_participants, by = "participantID", all.x = T)

# export to csv
write.csv(x = F1a, file = "F1a.csv")
write.csv(x = F1b, file = "F1b.csv")
write.csv(x = F2, file = "F2.csv")
write.csv(x = F4, file = "F4.csv")
write.csv(x = F5, file = "F5.csv")
write.csv(x = F6, file = "F6.csv")

#### 1. progress report ####
#### 1.1 active pop-based surveillance (monthly HH visits) ####
# baseline of entire cohort
# number of households participating
totalHHvisitedBL <- F1a %>%
  group_by(`Id da Localizacao`) %>%
  summarise(n=n()) #2251 HH visited
count(totalHHvisitedBL)

# number of HH members included F1b
totalHHmembersincludedBL <- F1b %>%
  group_by(`Id da Localizacao.x`, participantID) %>%
  summarise(n=n()) %>% #7995 members recorded 
  group_by(`Id da Localizacao.x`) %>%
  summarize(n=n())

count(totalHHmembersincludedBL) # number of HH with at least one member included: 1932 (vs 1907 in dashboard)
sum(totalHHmembersincludedBL$n) # number of HH members belonging in one of the HH, excluding duplicates: 7995 (vs 7975 in dashboard)

# age distribution participants
hist(as.numeric(F1b$ageyears), breaks = 20)

# sex distribution
table(F1b$Gender)
prop.table(table(F1b$Gender))

# occupation
table(F1b$`Ocupacao principal`)
table(F1b$`Especifique a outra ocupacao`)

# comorbidities
table(F1b$`Gravidez (se o membroe do sexo feminino e em idade reprodutiva)`) # 85 pregant women
table(F1b$`Tem cancro?`) # 16 cancers
table(F1b$`Tipo de cancro`)
table(F1b$`Tem Diabetes?`) # 108 diabetes cases, of which 80 treated
prop.table(table(F1b$`Tem Diabetes?`))
table(F1b$`Tem HIV?`)
prop.table(table(F1b$`Tem HIV?`)) # 621 HIV+; about 7%
table(F1b$`Tem tuberculose ?`) # 0.36% with active tb; 2.7% with a history of tb
table(F1b$`Especifique as outras imunodeficiencias conhecidas`)
table(F1b$`Tem Hipertensao?`)
round(prop.table(table(F1b$`Tem Hipertensao?`))*100,2) # about 10% with diagnosed hypertension
table(F1b$`Tem doencas cardiacas cronicas (com excepcao do AVc)?`)
round(prop.table(table(F1b$`Tem doencas cardiacas cronicas (com excepcao do AVc)?`))*100,2) # about 1% chronic heart disease
table(F1b$`hospitalizacao anterior por acidente vascular cerebral ?`)
prop.table(table(F1b$`hospitalizacao anterior por acidente vascular cerebral ?`)) #0,6% of respondents had prior AVC
table(F1b$`Tem doenca pulmonar cronica ?`)
round(prop.table(table(F1b$`Tem doenca pulmonar cronica ?`))*100,2) # about 5% with asthma
table(F1b$`Especifique outra doenca pulmonar cronica.`)
table(F1b$`Tem doenca hepatica cronica?`) # 0,2% w chronic liver disease
table(F1b$`Tem doenca hematologica cronica?`) # about 1% with falciforme anaemia
table(F1b$`Tem doenca renal cronica ?`) # 0,25% w CKD
table(F1b$`Tem perturbacoes neurologicas cronicas (nao perturbacoes da saude mental como, p`) # 0.30% with convulsions
table(F1b$Tabagismo) # about 2.6% smokers
table(F1b$`Outro estado de saude, especificar.`) # I checked all whether some of the conditions of interest were missed. Most are types of pain, acute illness, gastro-intestinal complaints
F1b$`Tem doenca pulmonar cronica ?`[F1b$`Outro estado de saude, especificar.`=="Asma"] <- "Sim, asma, mas nenhum tratamento disponivel" # one asthma entered, which should have been under chronic pulm disease


# use of public transport
table(F1b$`Usou transportes publicos nos ultimos 7 dias?`, useNA = "always")
# distribution of modes of public transportation
round(prop.table(table(F1b$`Se sim, qual foi o principal tipo de transporte publico usado ?`))*100,2)
# freq of public transport use
table(F1b$`com que frequencia usou transporte publico nos ultimos 7 dias?`)


# follow-up F2
totalHHvisitedFU1 <- F2 %>%
  group_by(`Id da Localizacao`) %>%
  summarise(n=n()) #
table(totalHHvisitedFU1$n)

# symptomatic episodes during follow-up
table(F2$`Nas ultimas duas semanas, algum dos membros do agregado familiar teve algum sint`)
# summarise number of visits and number of symptomatic cases
FUvisits <- F2 %>%
  group_by(`Data do contacto`, `Nas ultimas duas semanas, algum dos membros do agregado familiar teve algum sint`) %>%
  summarise(nHHvisits = n())
FUhistogram <- ggplot(FUvisits, aes(x=as.Date(`Data do contacto`), y=nHHvisits, fill=factor(`Nas ultimas duas semanas, algum dos membros do agregado familiar teve algum sint`))) +
  geom_col()+
  labs(title="", x = "", y="Number of follow-up visits") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        axis.text = element_text(size=14),
        axis.title=element_text(size=15),
        legend.text= element_text(size=10),
        title= element_text(size=15))
FUhistogram
ggsave(FUhistogram, filename = "FUhistogram.png", width = 9, height = 4)

# if/where they sought HC when ill
table(F2$`algum destes sintomas exigiu que procurasse assistencia medica? (${individualID_`) # 41 of 142 sought HC
table(F2$`Em caso afirmativo, que tipo de prestador de cuidados de saude procurou? (${indi`) # 4 in the hospital; 26 in health centre

# how frequent several HH members
table(F2$`Mais de um (1) Membro?`)
        
# contacts of those cases
table(F2$`Nas ultimas 2 semanas, algum dos membros do agregado familiar teve contacto com`)

#### 1.2 nasal swab results - F5 with ODK questionnaire and lab results excel ####
# overview: % tested, % positive
table(possiblecases$`Amostra foi colhida?`, useNA = "always")
table(possiblecases$participantID[possiblecases$`Amostra foi colhida?`=="Nao"]) # cases for which no samples were collected
table(possiblecases$participantID[is.na(possiblecases$ssdffcolda)&possiblecases$`Amostra foi colhida?`!="Nao"]) # cases for which a sample was collected according to quest, but cannot be found again in lab results
# reasons for not collecting a sample
table(possiblecases$`Por quee que a amostra nao foi colhida?`) # 14/15 were refusals

# histogram tests of possible cases
possiblecases_summary <- possiblecases %>%
  filter(!is.na(`Data de inicio dos sintomas`)) %>%
  group_by(`Data de inicio dos sintomas`, resdpcm) %>%
  summarise(npossiblecases = n())
possiblecaseshistogram <- ggplot(possiblecases_summary, aes(x=as.Date(`Data de inicio dos sintomas`), y=npossiblecases, fill=factor(`resdpcm`))) +
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
ggsave(possiblecaseshistogram, filename = "possiblecaseshistogram.png", width = 9, height = 4)

# combined histogram FU visits and possible cases
# possiblecases_summary$date <- as.Date(possiblecases_summary$`Data de inicio dos sintomas`)
# FUvisits$date <- as.Date(FUvisits$`Data do contacto`)
# activsurv <- merge(FUvisits, possiblecases_summary, by = "date", all = T)

# age distribution of possible cases
hist(as.numeric(possiblecases$ageyears), breaks = 20)
hist(as.numeric(possiblecases$ageyears[possiblecases$resdpcm=="POSITIVO"]), breaks = 20)
hist(as.numeric(possiblecases$ageyears[possiblecases$resdpcm=="NEGATIVO"]), breaks = 20)

# sex distribution of possible cases
table(possiblecases$Gender)
table(possiblecases$Gender[possiblecases$resdpcm=="POSITIVO"])

#### 1.3 baseline sero-survey participants ####
# filter on only those for whom a DBS sample was collected
DBS_M0_collected <- F4M0_bldata %>%
  filter(`A visita / evento foi realizada?`=="sim"&`Visita serovigilancia n`=="M0") # none with Visita serovigilancia n missing
nDBSperparticipant_M0 <- DBS_M0_collected %>%
  group_by(`Id do individuo`) %>%
  summarise(n=n())
count(nDBSperparticipant_M0) # n=1618 (if at least once in db) but 126 multiple times in the db -> check if duplicates

# export updated list of DBS participants
export_serosurvey_participants <- DBS_M0_collected %>% select(participantID, ageyears, Gender)
write.csv(x =export_serosurvey_participants, file = "export_serosurvey_participants.csv")

# export participant IDs who were visited but no sample was collected
DBS_M0_includedbutnosamplecollected <- F4M0_bldata %>%
  filter(`A visita / evento foi realizada?`=="sim"&`Visita serovigilancia n`=="M0"&`Amostra foi retirada?`=="nao") 
write.csv(DBS_M0_includedbutnosamplecollected, file = "DBS_M0_includedbutnosamplecollected.csv")
DBS_M3_includedbutnosamplecollected <- F4M3 %>%
  filter(`A visita / evento foi realizada?`=="sim"&`Visita serovigilancia n`=="M3"&`Amostra foi retirada?`=="nao") 
write.csv(DBS_M3_includedbutnosamplecollected, file = "DBS_M3_includedbutnosamplecollected.csv")

# age, sex and QU of the sero-participants
hist(as.numeric(DBS_M0_collected$ageyears), breaks = 20, col = 20)
table(DBS_M0_collected$Gender)
prop.table(table(DBS_M0_collected$Gender))

# date of DBS collection
hist(DBS_M0_collected$sample_confirmsample_collection_, breaks = 10)

# how many unique HH head IDs visited -> but still the duplicates in there!!
summaryDBScollection <-  DBS_M0_collected %>%
  group_by(`Id da Localizacao`) %>%
  summarise(n=n())
table(summaryDBScollection$n)

# DBS M3 collected
DBS_M3_collected <- F4M3 %>%
  filter(`A visita / evento foi realizada?`=="sim"&`Visita serovigilancia n`=="M3") 
nDBSperparticipant_M3 <- DBS_M3_collected %>%
  group_by(`Id do individuo`) %>%
  summarise(n=n())
table(nDBSperparticipant_M3$n) # 7 twice -> check if duplicates
#vaccinated DBS participants
table(F4M3$`Voce recebeu a vacina contra COVID-19?`[F4M3$`A visita / evento foi realizada?`=="sim"&F4M3$`Visita serovigilancia n`=="M3"])

# how many visits done
F4_DBScollected <- subset(F4, sample_confirmsample_colected=="sim") 
progressDBScollection <- F4 %>%
  filter(sample_confirmsample_colected=="sim") %>%
  group_by(sample_confirmsample_collection_,openhdsfieldWorkerId) %>%
  summarise(nvisits=n())
progressDBScollection

# plot by date
DBS_progress_plot <- ggplot(progressDBScollection, aes(x = sample_confirmsample_collection_,y=nvisits,fill=factor(openhdsfieldWorkerId))) + 
  geom_col() +
  labs(title="", x = "", y="Number of DBS collected") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_x_date(breaks = pretty_breaks(10))
DBS_progress_plot

# clinical FU of confirmed COVID19 cases
table(F6$`Estado actual de saude...23`, F6$`De que seguimento se trata?`) # all Outros are "Saudavel"