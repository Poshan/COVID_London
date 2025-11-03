## Description


This project deals with spatio-temporal modelling the weekly COVID-19 cases in Middle Layer Super Output Areas (MSOAs) in Greater London area, UK.


We have applied the daily movement/activity data from Mapbox, which is calculated by aggregating anonymized location data from mobile devices by day by geographic tiles of approximately 100-meter resolution. It provides measures of relative human activity (walking, driving, etc) over both space and time, presented as an activity index. More details of the datasets can be seen [here](https://docs.mapbox.com/data/movement/guides/)


# Spatiotemporal Analysis of COVID-19 Spread and Human Mobility in London

## Overview

This repository contains the R code for analyzing the relationship between human mobility patterns and COVID-19 incidence rates in London during 2020. The analysis uses Bayesian spatiotemporal models implemented in R-INLA to understand how movement patterns influenced disease spread across Middle Super Output Areas (MSOAs).

## Project Structure

```
├── 01_preprocessing/           # Data preprocessing scripts
│   ├── 01_duplicate_removal.R  # Remove duplicates from raw mobility data
│   ├── 02_spatial_aggregation.R # Aggregate data to MSOA level
│   ├── 03_covid_data_prep.R    # Process COVID-19 case data
│   ├── 04_od_matrix_prep.R     # Process Origin-Destination matrices
│
├── 02_modeling/                # Core modeling scripts
│   ├── create_graph.R          # Create spatial neighborhood graphs
│   ├── create_indicators.R     # Create activity and mobility indicators
│   ├── helper_functions.R      # Utility functions for modeling
|
│
├── 03_analysis/                # Analysis and visualization
│   └── exploratory_analysis.R  # Create plots for publication
│
├── data/                      # Data directory 
│   ├──Boundary: are the boudaries of 983 MSOAs in Greater London Area. 
│   ├──COVID_19_dataset: data are obtained from [UKHSA dashboard](https://ukhsa-dashboard.data.gov.uk/covid-19-archive-data-download), we used weekly cases per MSOAs from the year 2020. 
│   ├──mobility_data: preprocessed mapbox activitydata
│   ├──OD_matrix_COVID:origin destination matrix showing the number of commutes between the MSOAs during the COVID-19 period. The source of datasets is the [Office of National Statistics, UK](https://www.ons.gov.uk/releases/estimationoftraveltoworkmatricesenglandandwales)
│   ├──other_data: estimated population of MSOAs prepared by [Office of national statistics](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates) 
│   ├──processed_data: 
│   
└── model_run.Rmd              # Main model implementation
```


## Dependencies

### Required R Packages

```r
# Core packages
library(sf)          # Spatial data handling
library(dplyr)       # Data manipulation
library(tidyr)       # Data tidying
library(arrow)       # Parquet file support
library(lubridate)   # Date handling

# Spatial analysis
library(spdep)       # Spatial dependence
library(INLA)        # Bayesian inference

# Visualization
library(ggplot2)     # Plotting
library(viridis)     # Color scales
library(patchwork)   # Combine plots

# Additional
library(fuzzyjoin)   # Fuzzy matching for time periods
library(Matrix)      # Sparse matrices
library(classInt)    # Classification intervals
```

<!-- ## Data Requirements

### Input Data

1. **Mobility Data**: Activity indices from Mapbox Movement dataset
   - Format: CSV with columns: `XLON`, `XLAT`, `AGG_DAY_PERIOD`, `ACTIVITY_INDEX_TOTAL`
   - Temporal coverage: January-December 2020
   - Spatial coverage: Greater London

2. **COVID-19 Data**: Weekly case counts by MSOA
   - Sources: UK Health Security Agency

3. **Boundary Data**: MSOA and LAD shapefiles
   - MSOA 2011 boundaries for London
   - Local Authority District boundaries

4. **Origin-Destination Matrices**: Travel patterns during COVID-19
   - Source: ONS travel-to-work estimates

5. **Population Data**: Mid-year population estimates 2020
   - Source: ONS population estimates by MSOA -->

## Workflow

### Step 1: Data Preprocessing

Run preprocessing scripts in order:

```r

# Run preprocessing pipeline
source("01_preprocessing/01_duplicate_removal.R")
source("01_preprocessing/02_spatial_aggregation.R") 
source("01_preprocessing/03_covid_data_prep.R")
source("01_preprocessing/04_od_matrix_prep.R")
```

### Step 2: Model Setup

Create spatial graphs and prepare data:

```r
source("02_Modelling/create_graph.R")
source("02_Modelling/create_indicators.R")
source("02_Modelling/helper_functions.R")
```

### Step 3: Run Models

Execute the main model in [RMarkdown](/model_run.Rmd):

```r
rmarkdown::render("model_run.Rmd")
```

### Step 4: Generate Visualizations

Create publication figures:

```r
source("03_analysis/exploratory_analysis.R")
```

<!-- ## Key Features

### Spatial Components

- **Contiguity-based graphs**: Queen contiguity for MSOA neighbors
- **Mobility-based graphs**: Weighted by Origin-Destination flows
- **Multi-level structure**: MSOA nested within LAD for hierarchical effects

### Temporal Components

- Weekly aggregation of daily mobility data
- COVID-19 waves identified and labeled
- Temporal random effects with AR(1) structure

### Model Specifications

- **Family**: Extended Poisson (xPoisson) for overdispersion
- **Offset**: Log population scaled to per 100,000
- **Random Effects**:
  - BYM2 spatial model (structured + unstructured)
  - RW1 temporal trend
  - Type III space-time interaction

### Visualization Outputs

- Temporal trends of cases and mobility
- Spatial distribution maps
- Model diagnostics and validation plots
- Observed vs fitted comparisons


## Output Files

- `processed_activity.parquet`: Weekly aggregated mobility
- `processed_data_with_covid_cases.parquet`: Combined dataset
- `origin_destination_matrix.parquet`: Processed OD matrix
- `df_lagged.parquet`: Dataset with spatial lag indicators -->

## Model Diagnostics

The analysis includes several diagnostic measuress:

- **CPO** (Conditional Predictive Ordinate): Model fit assessment
- **WAIC** (Watanabe-Akaike Information Criterion): Model comparison
- **R²**: Variance explained

## Reproducibility

To ensure reproducibility:

1. Use R version 4.0+ with specified package versions
2. Maintain directory structure as outlined
3. Process data in sequential order



## Citation

<!-- If you use this code, please cite:

```
[Your Paper Citation Here]
``` -->


## Acknowledgments

- Mapbox for mobility data
- UK Health Security Agency for COVID-19 surveillance data
- Office for National Statistics for population and boundary data
- R-INLA development team for the modeling framework
