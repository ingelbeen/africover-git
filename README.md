# Characterising transmission parameters of SARS-CoV-2 in a peri-urban setting in Mozambique using population-based surveillance and a high-throughput sero-assay

# Study registration 
Clinicaltrials.gov: NCT04442165 https://clinicaltrials.gov/study/NCT04442165

# Participating organisations
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
Three dataframes that allow reproduction of results in https://doi.org/10.1101/2023.12.22.23300474. For reasons of confidentiality, the open data does not contain the geographical coordinates, nor any other demographic data (e.g. age, household structure), that could allow identification of participants.
1. possible COVID-19 case data (all participants with acute respiratory illness test results, clinical signs and symptoms, age groups, sex, and socio-economic quintile): "possiblecases_pseudo.csv"
2. active COVID-19 survceillance data (all participants with acute respiratory illness test results, demographic and baseline participant data): "cases_participantsFU_pseudo.csv"
3. sero-survey data (dried blood spots collected 15 December 2020-31 July 2021 with demographic and baseline participant data): "serosurvey_pseudo.csv"

# Data analysis scripts (can be run on the open anonymized data available on this repository)
1. "/symptoms_associated_with_COVID19.R" the code to analyse clinical signs and symptoms associated with COVID-19 confirmation (SARS-CoV-2 PCR positive) among possible COVID-19 cases (onset of at least one respiratory symptom, ageusia, or anosmia in the past 2 weeks). It can be run on the "possiblecases_pseudo.csv" data.
2. "/disease_incidence.R" the code to analyse active household follow-up: describe cohort participant characteristics, epidemiological curve, COVID-19 risk factor analysis (Cox regression). Part of it can be run on the "participant_cases_FU.csv" data. Part of it uses identifying data (e.g., geographical coordinates), which is not available as open data.
3. "/infection_prevalence_risk_factors.R" the code to analyse the serosurvey results: describe serosurvey participant characteristics, samples collected, sero-prevalence over time, and SARS-CoV-2 seroconversion risk factor analysis (Cox regression). It can be run on the "serosurvey_pseudo.csv" data.
