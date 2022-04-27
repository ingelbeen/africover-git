# Characterising transmission parameters of SARS-CoV-2 in a peri-urban setting in Mozambique using population-based surveillance and a high-throughput sero-assay (Africover)

Clinicaltrials.gov: NCT04442165

## Participating Organisations:
Instituut voor Tropische Geneeskunde (ITM), Belgium (PI Marc-Alain Widdowson) 
Instituto Nacional de Saúde (INS), Mozambique (PI Ivalda Macicame)
Institut de Recherche pour le Développement (IRD), France (PI Martine Peeters)
Utrecht University Medical Centre, The Netherlands (PI Mirjam Kretzschmar) 

## Household data and sample collection: 
15/12/2020 to 15/01/2022 (with data completion until 30/04/2022)

## Project summary: 
Approach: We conducted population-based surveillance in a Demographic Health Surveillance System (HDSS) of 16,500 people in a peri-urban neighbourhood of Maputo, Mozambique. We collected clinical information and respiratory specimens from ill persons during biweekly household visits and complement with data from local clinics.  We conducted a serosurvey of a random selection of 1450 persons (<18, 18-50y, 50+) at baseline, 3, 6 and 12 months. 

Primary Objectives
•	Estimate the attack rate, secondary infection rate, serial interval, and the basic reproduction number of SARS-CoV-2 infection in peri-urban Maputo
•	Estimate the proportion of infections that are asymptomatic, and tease out the role these play in transmission
•	Estimate the incidence rate of infection, disease, hospitalization and mortality, over a 12 month period
•	Validate a high-throughput serologic Luminex SARS-CoV-2 serology assay and compare its performance to that of  two validated commercially available serologic assays in sSA
Secondary Objectives
•	Identify risk factors for infection and for asymptomatic or clinical presentation,
•	Validate the use of dried blood spots in SARS-CoV-2 serologic surveys
•	Measure antibody titers over time and analyze the correlation between anti-coronavirus (endemic coronavirus or SARS-CoV-2) antibody titers and reinfection 
•	Assess the individual and community uptake of measures aimed to reduce transmission and assess barriers to uptake and effect on transmission.
An existing Luminex platform was used to test dried blood spots (DBS) collected within the sero-survey rounds.

## Study participants:
Number of households in the active surveillance component (biweekly visits to detect possible Covid-19 cases): 
Number of individuals in the active surveillance component (biweekly visits to detect possible Covid-19 cases): 
Number of individuals in the repeated sero-survey (with >/=1 DBS collected):
Number of individuals in the social mixing survey:

## Databases:
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

## Data analysis scripts:
- africover_merge_odk_lab.R - importing, cleaning and merging databases to create a single participant database, a possible Covid19 case database and a sero-survey database.
- infection_prevalence_risk_factors.R - prevalence and risk factors for infection in the first pandemic year (SARS-CoV-2 Ab detected during baseline and M3 sero-survey rounds)
- nasal_swab_performance.R - positivity of PCR on nasal swab samples of possible cases by age, time between symptom onset and sample collection, variant (?), and clinical signs+symptoms
- disease_incidence.R - Covid19 possible case incidence 
