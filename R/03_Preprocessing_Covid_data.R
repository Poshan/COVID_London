library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(fuzzyjoin)


## weekly mean activity data by msoa -------------------------------------------
weekly_mean <-read_parquet("England/data/processed_data/weekly_mean_by_msoa.parquet")
# daily_mean_activity <- read_parquet("mean_by_msoa.parquet")


## COVID-19 weekly cases dataset -----------------------------------------------
###another covid dataset containing the central london city
##source: https://github.com/XueqingYin/COVID-19/blob/main/cases2020.csv
df_covid_2020 <- read.csv('England/data/COVID_19_dataset/cases2020.csv')
df_covid_2020$areaCode <- df_covid_2020$MSOA11CD
df_covid_2020 |> 
  filter(regionName == "London") |> 
  select(any_of(c("areaCode","date","cases","Population"))) -> 
  df_covid_2020


## Additional dataset for missing region --------------------------------------
##adding data for E02000001 -City of London

###https://ukhsa-dashboard.data.gov.uk/covid-19-archive-data-download
df_city_of_london <- read.csv("England/data/COVID_19_dataset/cases2020_E02000001.csv")
df_city_of_london$areaCode <- df_city_of_london$MSOA11CD
df_city_of_london |> select(-MSOA11CD) -> df_city_of_london
rbind(df_covid_2020, df_city_of_london) -> df_covid_2020



## Expand data to have all dates and all areaCode ------------------------------

##to make sure all areaCode and all the dates have data
df_comb <- expand_grid(date=unique(df_covid_2020$date), areaCode=unique(df_covid_2020$areaCode))
##join df_comb with df_covid
df_comb |> left_join(df_covid_2020, by = 
                       c("date", "areaCode")) -> df_covid_2020

##replace the cases values na by 0 
df_covid_2020$cases[is.na(df_covid_2020$cases)] <-0

df_covid_2020$date <- as.Date(df_covid_2020$date)

## Match the dates in the activity data ---------------------------------------
##there is a two days difference between the week ending system in the COVID_19 dataset 
##and the activity dataset
##adding two days to the covid-19 cases date
df_covid_2020$date <- df_covid_2020$date + days(2)


## Combine with the weekly mean activity data ----------------------------------
###join with the weekly_mean df
weekly_final_data <- 
  inner_join(df_covid_2020, weekly_mean, by = 
               c("areaCode" = "MSOA11CD", "date" = "date"))

weekly_final_data$weekly_cases <- weekly_final_data$cases



## Caserate per 100000 Population per msoa- ------------------------------------
### population data ---------
##data source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates

df_population_estimate <- read.csv('England/data/other_data/population_estimate_2020.csv')
##remove , on the population number
df_population_estimate <- df_population_estimate %>% 
  mutate_all(~gsub(",", "", .))

##select only required columns
df_population_estimate$areaCode <- df_population_estimate$MSOA.Code
df_population_estimate$All.Ages <- as.numeric(df_population_estimate$All.Ages)
df_population_estimate |> select(any_of(c("areaCode", "All.Ages"))) -> df_population_estimate

### compute caserate ------
##join the weekly final data with the population estimate
weekly_final_data |> 
  left_join(df_population_estimate, by = c("areaCode")) -> 
  weekly_final_data


###caserate computation
weekly_final_data |> mutate(caserate=100000*weekly_cases/All.Ages) -> weekly_final_data

weekly_final_data$weekly_mean_activity <- weekly_final_data$weekly_mean

weekly_final_data |> 
  select(any_of(c("areaCode","date","weekly_mean_activity", "week",
                                       "weekly_cases", "caserate", "Population"))) -> 
  london_df


## Various waves of COVID ------------------------------------------------------

waves.df <-data.frame(start_date = c("2020-03-16", "2020-03-23","2020-05-10", "2020-06-01", "2020-07-04", "2020-09-14", "2020-11-05"),
                      end_date = c("2020-03-23", "2020-05-10", "2020-06-01", "2020-07-04", "2020-09-14", "2020-11-05","2020-12-31"),
                      wave = c("Before lockdown", "lockdown 1", "Easedown 1", "Easedown 2", "Easedown 3", "Volunteered restrictions", "lockdown 2"),
                      wave_events= c("1500 cases death toll reaches 55", 
                                     "first lockdown, legalized on 26th March", 
                                     "Easedowned by people who cannot work from home should return to the workplace but avoid public transport ",
                                     "Schools reopened, non-essential shops open, football starts, relaxation of the social distancing",
                                     "Pubs, restaurants, indoor theatre, bowling alleys open",
                                     "Rule of six, restricted outdoor social gathering, work from home again, curfew in hospital zones",
                                     "lockdown 2")
)



# join the dataset with the waves.df
london_df |> fuzzy_left_join(waves.df, 
                             by=c("date" = "start_date", "date" = "end_date"),
                             match_fun = c(`>=`, `<`)) -> london_df



write_parquet(london_df, "England/data/processed_data/london_weekly_data_with_caserate.parquet")
