## Description


This project deals with spatio-temporal modelling the weekly COVID-19 cases in Middle Layer Super Output Areas (MSOAs) in Greater London area, UK.


We have applied the daily movement/activity data from Mapbox, which is calculated by aggregating anonymized location data from mobile devices by day by geographic tiles of approximately 100-meter resolution. It provides measures of relative human activity (walking, driving, etc) over both space and time, presented as an activity index. More details of the datasets can be seen [here](https://docs.mapbox.com/data/movement/guides/)


## Data

**Boundary:** 

`MSOA_2011_London_gen_MHW.shp` are the boudaries of 983 MSOAs in Greater London Area. 

**COVID_19_dataset:** 

`cases2020.csv` is the data are obtained from [UKHSA dashboard](https://ukhsa-dashboard.data.gov.uk/covid-19-archive-data-download), we used weekly cases per MSOAs from the year 2020. 
There were missing datasets for the City of London (MSOA code: E02000001), So it was added from archives of the UKHSA dashboard and a csv manually created `cases2020_E02000001.csv`

**OD_matrix_COVID:**

`ons-des-prod-mobility-outenc-ingress_encrypt_estODMatrix_msoa_MidPandemic_df.csv` is the origin destination matrix showing the number of commutes between the MSOAs during the COVID-19 period. The source of datasets is the [Office of National Statistics, UK](https://www.ons.gov.uk/releases/estimationoftraveltoworkmatricesenglandandwales)

**other_data:**

`population_centers_london.shp` is the shapefile containing the residential places and their population. **not used in this analysis**

`population_estimate_2020.csv` is the estimated population of MSOAs prepared by [Office of national statistics](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates) 


**processed_data:**

This folder contains the pre-processed datasets.

`1000m_grids.shp` is the 1000m by 1000m grids for the Greater London area. **not used in this analysis**

`activity_in_1000m_grids.parquet` is the aggregated (mean) from the mapbox activity data upscaled to 1000m grids **not used in this analysis**


`distinct_LONLAT.parquet` is a dataframe containing all the distinct locations where the mapbox activity data are availalble in the study area during the year 2020. See [Preprocessing](R/01_Preprocessing_Duplicate_removal.R)

`london_df_mobilitylaggedactivity.parquet` is created when the origin destination matrix (explained above) is combined with the mean activity within MSOAs. See [R code creating mobility factor](R/07_Modelling_create_MobilityFactor.R). This dataframe is used in the models, so the data description is shown below:

- *areaCode*: unique ID of each MSOA
- *date*: weekly dates
- *weekly_mean_activity*: aggregated mean activity for the week 
- *week*: week number 
- *weekly_cases*: weekly COVID-19 cases reported
- *caserate*: weekly_cases per 100000 population in the MSOA
- *Population*: population estimate of the MSOA for the year 2020
- *wave*: COVID-19 waves and its events (depicting the decisions made during the period)
- *weekly_mean_activity_lag1*: *weekly_mean_activity* one week prior
- *weekly_mean_activity_lag2*: *weekly_mean_activity* two weeks prior
- *MobilityLag*: *weekly_mean_activity* which is combined with the neighbors and their weights from the OD matrices
- *MobilityLag_weeklag_1*: *weekly_mean_activity_lag1* combined with the neighbors and their weights from the OD matrices
- *MobilityLag_weeklag_2*: *weekly_mean_activity_lag2* combined with the neighbors and their weights from the OD matrices


`origin_destination_matrix.parquet` is the processed origin destination matrix dataset (explained above). See [Preprocessing of OD matrices](R/04_Preprocessing_OD_matrices.R)

`pre_processed_movement.parquet` is the pre-processed activity data created from the activity data provided by mapbox. See [Preprocessing of activity data](R/01_Preprocessing_Duplicate_removal.R), this step mainly involves duplicate averaging.

`weekly_mean_by_msoa.parquet` is the aggregated activity data created from the activity datasets. The aggregation is spatially at the level of MSOAs and weekly. See [Aggregation](R/02_Preprocessing_Aggregation.R) 

## Code
The code are structured in the way below:

`01_Preprocessing_Duplicate_removal.R` reads the raw activity datasets (**not included here due to its large size**), identifies the duplicated datasets in the same grids,  averages them and saves as [pre_processed_movement.parquet](data/processed_data/pre_processed_movement.parquet)

`02_Preprocessing_Aggregation.R` reads the pre-processed movement data and aggregates them into the level of MSOAs spatially as well as weekly and saves them as [weekly_mean_by_msoa.parquet](data/processed_data/weekly_mean_by_msoa.parquet). This R file also includes an experimental code where the activity data is aggregated into other grid sizes (for example 1000m)

`03_Preprocessing_Covid_data.R` reads the COVID-19 cases datasets, population estimates, aggregated activity data combines them to form to a single dataset.


`04_Preprocessing_OD_matrices.R` reads the origin destination matrix, changes the MSOAs codes by serial numbers in the datasets, adds a new column named origin and saves as [origin_destination_matrix.parquet](data/processed_data/origin_destination_matrix.parquet) 

`05_Exploratory_analysis.R` prepares various exploratory charts and maps

`06_Modelling_create_graph.R` creates the graph based on geography and the graph based on OD-matrices

`07_Modelling_create_MobilityFactor.R` combines the graph created from the origin destination matrix and the aggregated activities in the MSOAs to create a mobility factor and saves the dataframe as [london_df_mobilitylaggedactivity.parquet](data/processed_data/london_df_mobilitylaggedactivity.parquet)

`08_Modelling_INLA_models.R` models based on the prepared data and graphs using R-INLA packages and plots the summary results.


## Libraries used
