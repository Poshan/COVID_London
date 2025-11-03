#!/usr/bin/env Rscript
# ==============================================================================
# 03_covid_data_prep.R
# Process COVID-19 case data and merge with mobility data
#
# Input: COVID-19 cases, population estimates, processed activity data
# Output: Combined dataset with cases, rates, and mobility
# ==============================================================================

# Load required libraries ------------------------------------------------------
library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(fuzzyjoin)

# Load processed activity data ------------------------------------------------
message("Loading processed activity data...")
weekly_mean <- read_parquet("data/processed_data/processed_activity.parquet")

# Load COVID-19 case data -----------------------------------------------------
message("Loading COVID-19 case data...")

# Main COVID dataset for London MSOAs

# Source: https://ukhsa-dashboard.data.gov.uk/covid-19-archive-data-download
df_covid_2020 <- read.csv('data/COVID_19_dataset/cases2020.csv')

# Filter for London and rename columns
df_covid_2020 <- df_covid_2020 |>
  filter(regionName == "London") |>
  mutate(areaCode = MSOA11CD) |>
  select(areaCode, date, cases, Population)

# Add City of London data (E02000001) ----------------------------------------

df_city_of_london <- read.csv("data/COVID_19_dataset/cases2020_E02000001.csv")
df_city_of_london <- df_city_of_london |>
  mutate(areaCode = MSOA11CD) |>
  select(-MSOA11CD)

# Combine datasets
df_covid_2020 <- rbind(df_covid_2020, df_city_of_london)

message("Loaded cases for ", n_distinct(df_covid_2020$areaCode), " MSOAs")

# Ensure complete date-area coverage ------------------------------------------
message("Filling missing date-area combinations...")

# Create all possible combinations
all_combinations <- expand_grid(
  date = unique(df_covid_2020$date),
  areaCode = unique(df_covid_2020$areaCode)
)

# Join with actual data
df_covid_2020 <- all_combinations |>
  left_join(df_covid_2020, by = c("date", "areaCode"))

# Replace NA cases with 0 (no reported cases)
df_covid_2020$cases[is.na(df_covid_2020$cases)] <- 0

# Convert date to Date type
df_covid_2020$date <- as.Date(df_covid_2020$date)

# Align week definitions with activity data -----------------------------------
message("Aligning week definitions...")

# Activity data uses week ending on specific days
# COVID data needs adjustment to match

# Handle year-end edge case
weekly_mean$AGG_DAY_PERIOD[weekly_mean$AGG_DAY_PERIOD == as.Date('2020-12-31')] <- 
  as.Date('2020-12-29')

# Adjust COVID dates to align with activity week endings
# Add 3 days to COVID dates to match week-ending system
df_covid_2020$date <- df_covid_2020$date + days(3)

# Merge COVID and activity data -----------------------------------------------
message("Merging COVID and activity data...")

weekly_final_data <- inner_join(
  df_covid_2020, 
  weekly_mean,
  by = c("areaCode" = "MSOA11CD", "date" = "AGG_DAY_PERIOD")
)

weekly_final_data$weekly_cases <- weekly_final_data$cases

# Calculate case rates per 100,000 population ---------------------------------
message("Calculating case rates...")

# Load population estimates
# Source: ONS Mid-year population estimates by MSOA
df_population <- read.csv('data/other_data/population_estimate_2020.csv')

# Clean population data
df_population <- df_population |>
  mutate_all(~gsub(",", "", .)) |>  # Remove commas from numbers
  mutate(
    areaCode = MSOA.Code,
    All.Ages = as.numeric(All.Ages)
  ) |>
  select(areaCode, All.Ages)

# Add population and calculate rates
weekly_final_data <- weekly_final_data |>
  inner_join(df_population, by = "areaCode") |>
  mutate(
    caserate = 100000 * weekly_cases / All.Ages,
    Population = All.Ages,
    weekly_mean_activity = mean_weekly_activity
  )

# Define COVID-19 waves and restriction periods -------------------------------
message("Adding COVID wave indicators...")

# Define key periods based on UK government restrictions
# Source: https://en.wikipedia.org/wiki/COVID-19_pandemic_in_England
waves_df <- data.frame(
  start_date = c("2020-03-16", "2020-03-23", "2020-05-10", "2020-06-01", 
                 "2020-07-04", "2020-09-14", "2020-11-05"),
  end_date = c("2020-03-23", "2020-05-10", "2020-06-01", "2020-07-04", 
               "2020-09-14", "2020-11-05", "2020-12-31"),
  wave = c("Before lockdown", "Lockdown 1", "Easedown 1", "Easedown 2", 
           "Easedown 3", "Voluntary restrictions", "Lockdown 2"),
  description = c(
    "1500 cases, death toll reaches 55",
    "First national lockdown (legally enforced from March 26)",
    "Return to workplace for those unable to work from home",
    "Schools reopen, non-essential shops open, sports resume",
    "Pubs, restaurants, theatres, leisure facilities reopen",
    "Rule of six, work from home guidance, local restrictions",
    "Second national lockdown"
  )
)

# Add wave information using fuzzy date matching
london_df <- weekly_final_data |>
  fuzzy_left_join(
    waves_df,
    by = c("date" = "start_date", "date" = "end_date"),
    match_fun = c(`>=`, `<`)
  ) |>
  select(-start_date, -end_date)

# Final data preparation -------------------------------------------------------
message("Finalizing dataset...")

# Select and rename final columns
london_df <- london_df |>
  select(
    areaCode,
    date,
    week,
    weekly_mean_activity,
    weekly_cases,
    caserate,
    Population,
    wave,
    description
  )

# Save processed data ----------------------------------------------------------
output_file <- "data/processed_data/processed_data_with_covid_cases.parquet"
write_parquet(london_df, output_file)

