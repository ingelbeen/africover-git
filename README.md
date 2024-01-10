# Characterising transmission parameters of SARS-CoV-2 in a peri-urban setting in Mozambique using population-based surveillance and a high-throughput sero-assay

# Study registration 
Clinicaltrials.gov: NCT04442165 https://clinicaltrials.gov/study/NCT04442165

# Participating Organisations
- Instituut voor Tropische Geneeskunde (ITM), Belgium (Coordinating PI Brecht Ingelbeen, former: Marc-Alain Widdowson)
- Instituto Nacional de Saúde (INS), Mozambique (PI Ivalda Macicame)
- Institut de Recherche pour le Développement (IRD), France (PI Martine Peeters)

# Project summary
During December 2020-March 2022, households of a population cohort embedded in the Demographic Health Surveillance System (HDSS) of Polana Caniço, Maputo, Mozambique (16,500 people in a peri-urban neighbourhood of Maputo) were contacted biweekly. Residents reporting any respiratory sign, anosmia, or ageusia, were asked to self-administer a nasal swab, for SARS-CoV-2 PCR testing. Of a subset of participants, dried blood spots were repeatedly collected three-monthly from finger pricks at home. Antibodies against SARS-CoV-2 spike glycoprotein and nucleocapsid protein were detected using an in-house developed multiplex antibody assay. We estimated the incidence of respiratory illness and COVID-19, and SARS-CoV-2 seroprevalence. We used Cox regression models, adjusting for age and sex, to identify factors associated with first symptomatic COVID-19 and with SARS-CoV-2 sero-conversion in the first six months.

# Timeline
Data and sample collection: 15/12/2020 to 31/03/2022 (data completion up to 30/04/2022)

# Manuscripts
- Evaluation of a surrogate virus neutralization test for high-throughput serosurveillance of SARS-CoV-2 https://doi.org/10.1016/j.jviromet.2021.114228
- Mild and moderate COVID-19 during Alpha, Delta and Omikron pandemic waves in urban Maputo, Mozambique, December 2020-March 2022: a population-based surveillance study https://doi.org/10.1101/2023.12.22.23300474

# Study participants
- Number of households in the active surveillance component (biweekly visits to detect possible Covid-19 cases): 1561 
- Number of individuals in the active surveillance component: 6049
- Number of person-years followed up in the active surveillance: 1895.9 
- Number of individuals in the repeated sero-survey (with >/=1 DBS collected): 1412
- Number of individuals in the social mixing survey: (to be completed)

# Data
## Open data (allowing reproduction of results in https://doi.org/10.1101/2023.12.22.23300474)
- COVID-19 case data (all participants with acute respiratory illness test results, demographic and baseline participant data)
- sero-survey data (dried blood spots collected 15 December 2020-31 July 2021 with demographic and baseline participant data)
  
## Raw data (available if approved following request to ITMresearchdataaccess [at] itg.be)
- participant demographic data (from HDSS)
- participant socio-economic data (from HDSS)
- baseline household data (F1a)
- baseline individual participant data (F1b)
- household active surveillance (biweekly visits) data (F2)
- individual participant weight, height and MUAC (F3)
- sero-survey(F5)
- possible case report (F6)
- medical conditions update (F7) 
- social mixing survey: contacts during previous 24h (F8a)
- social mixing survey: mobility during previous 24h (F8b)
- sero-survey DBS results
- nasal swab PCR results (of possible cases) 

# Data analysis scripts
## Description of the open data scripts (anonymized data available on the github)

To run the merge_odk_lab.R script, you will need a folder "database" in your project, with the different databases, which will be put on this repository once completed, cleaned and anonymized.
To run the analysis scripts, you need first to run merge_odk_lab.R, which will generate .csv files with cleaned and linked databases, which are then imported in the analysis scripts.

## Description of the raw data scripts
- merge_odk_lab.R - importing, cleaning and merging databases to create a single participant database, a possible Covid19 case database and a sero-survey database.
- infection_prevalence_risk_factors.R - prevalence and risk factors for infection in the first pandemic year (SARS-CoV-2 Ab detected during baseline and M3 sero-survey rounds)
- nasal_swab_performance.R - positivity of PCR on nasal swab samples of possible cases by age, time between symptom onset and sample collection, variant (?), and clinical signs+symptoms
- disease_incidence.R - Covid19 possible case incidence 
