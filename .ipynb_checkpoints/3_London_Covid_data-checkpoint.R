library(sf)
library(dplyr)
library(tidyr)
# library(gstat)
# library(spacetime)
library(sp)
library(arrow)
# library(stars)
library(reshape)
library(lubridate)

setwd('/palma/scratch/tmp/pniraula/London/')




# ###############################################################################
# ###############################################################################
# ##################covid cases #############################################
# ###############################################################################
# ###############################################################################
###weekly mean activity data by msoa
weekly_mean <-read_parquet("weekly_mean_by_msoa.parquet")
# daily_mean_activity <- read_parquet("mean_by_msoa.parquet")

# df_covid <- read.csv('COVID_19_dataset/London_msoa_2023-12-14.csv')
# df_covid$date <- as.Date(df_covid$date)
# df_covid |> filter(date < '2020-12-31') -> df_covid_2020


###another covid dataset containing the central london city
##source: https://github.com/XueqingYin/COVID-19/blob/main/cases2020.csv
df_covid <- read.csv('COVID_19_dataset/cases2020.csv')
df_covid |> filter(regionName == "London") |> select(any_of(c("MSOA11CD","date","cases","Population"))) -> df_covid

df_covid_2020 <- df_covid
df_covid_2020$areaCode <- df_covid_2020$MSOA11CD
###https://ukhsa-dashboard.data.gov.uk/covid-19-archive-data-download


##to make sure all areaCode and all the dates have data
df_comb <- expand_grid(date=unique(df_covid_2020$date), areaCode=unique(df_covid_2020$areaCode))
##join df_comb with df_covid
df_comb |> left_join(df_covid_2020, by = 
                       c("date", "areaCode")) -> df_covid_2020_1

##replace the cases values na by 0 
df_covid_2020_1$cases[is.na(df_covid_2020_1$cases)] <-0

df_covid_2020_1$date <- as.Date(df_covid_2020_1$date)
##TO matching the nearest date in the activity data
df_covid_2020_1$date <- df_covid_2020_1$date + days(2)

###join with the weekly_mean df
weekly_final_data <- 
  inner_join(df_covid_2020_1, weekly_mean, by = 
               c("areaCode" = "MSOA11CD", "date" = "date"))

weekly_final_data$weekly_cases <- weekly_final_data$cases

# weekly_final_data |> select(-one_of(c("regionCode","regionName","UtlaCode","UtlaName","LtlaCode","LtlaName","areaName","areaType", "newCasesBySpecimenDateRollingSum"))) -> 
#   weekly_final_data 




df_covid_2020[!(unique(df_covid_2020$areaCode) %in% unique(weekly_final_data$areaCode)),]

weekly_final_data <- read_parquet("weekly_msoa_aggregated_data.parquet")

# weekly_final_data$caserate[weekly_final_data$caserate == 0] <- 0.00001
# weekly_final_data$log_caserate <- log(weekly_final_data$caserate)


##population per msoa required
##data source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates
df_population_estimate <- read.csv('other_data/population_estimate_2020.csv')

##join the weekly final data with the population estimate
inner_join(weekly_final_data, df_population_estimate, by = join_by(areaCode==MSOA.Code)) -> weekly_final_data_1

##remove , on the population number
weekly_final_data_1 <- weekly_final_data_1 %>% 
  mutate_all(~gsub(",", "", .))

weekly_final_data_1$All.Ages <- as.numeric(weekly_final_data_1$All.Ages)
weekly_final_data_1$weekly_cases <- as.numeric(weekly_final_data_1$weekly_cases)
###caserate computation
weekly_final_data_1 |> mutate(caserate=100000*weekly_cases/All.Ages) -> weekly_final_data_1

weekly_final_data_1$weekly_mean_activity <- weekly_final_data_1$weekly_mean

weekly_final_data_1 |> select(any_of(c("areaCode","date","weekly_mean_activity", "week",
                                       "weekly_cases", "MSOA.Name", "caserate"))) -> 
  london_df

write_parquet(london_df, "london_weekly_data_with_caserate.parquet")


# ###############################################################################
# ###############################################################################
# ######OLS
# ###############################################################################
# ###############################################################################
# 
# london_df <- read_parquet("london_weekly_data_with_caserate.parquet")
# 
# model_ols_1 <- lm(caserate~1+ as.factor(week), data = london_df)
# model_ols_2 <- lm(caserate~1+as.factor(areaCode), data=london_df)
# model_ols_3 <- lm(caserate~1+as.factor(areaCode)+as.factor(week), data = london_df)
# 
# ##computing the residuals
# residuals_ols_3 <- resid(model_ols_3)
# ##plot residuals against the fitted values
# plot(model_ols_3$fitted.values, residuals_ols_3, 
#      xlab = "Fitted values", ylab = "Residuals",
#      main = "Residuals vs Fitted")
# 
# model_ols_4 <- lm(caserate~1+as.factor(areaCode)+as.factor(week)+weekly_mean, 
#                   data = weekly_final_data_1)
# 
# 
# 
# abline(h = 0, col = "red")  
# plot(model_ols, 1)
# boxplot(caserate ~ as.factor(week), london_df)
# 
# ##see if straight line in QQ plot
# qqnorm(residuals_ols)
# qqline(residuals_ols)
# 
# ##plot residuals against the predicor mean activity
# plot(weekly_final_data$weekly_mean, residuals_ols, 
#      xlab = "weekly_mean_activity", ylab = "Residuals",
#      main = "OLS: Residuals vs weekly_mean_activity")
# abline(h = 0, col = "red")  



# ###############################################################################
# ###############################################################################
# ######Explore the activity and covid 19 data
# ###############################################################################
# ###############################################################################

####plot the data with 

###############

library(ggplot2)

# 
london_df <- read_parquet("london_weekly_data_with_caserate.parquet")

# lapply(c("weekly_cases","caserate"), function(c){
#   london_df[c] <- as.numeric(london_df[c])
# })

london_df$weekly_mean_activity <- as.numeric(london_df$weekly_mean_activity)

# Calculate overall weekly mean caserate
weekly_mean_activitys <- london_df %>%
  group_by(date) %>%
  summarise(mean_caserate = mean(caserate, na.rm = TRUE))

london_df <- left_join(london_df, weekly_mean_activitys, by = "date")
# Calculate overall weekly mean caserate
weekly_mean_activitys_act <- london_df %>%
  group_by(date) %>%
  summarise(mean_activity = mean(weekly_mean_activity, na.rm = TRUE))

london_df <- left_join(london_df, weekly_mean_activitys_act, by = "date")

# Convert date to Date format
london_df$date <- as.Date(london_df$date)

# Plot using ggplot
ggplot(london_df, aes(x = date, y = caserate, group = areaCode, color = areaCode)) +
  geom_line() +
  geom_line(aes(y = mean_caserate), color = "black", linetype = "dashed") +
  labs(x = "Week Ending", y = "Case Rate") +
  ggtitle("Temporal Line Plot of Case Rate and Weekly Mean Case Rate") +
  theme_minimal()+
  theme(legend.position = "none")+
  scale_x_date(date_breaks = "1 month")



# Plot using ggplot
ggplot(london_df, aes(x = date, y = weekly_mean_activity, group = areaCode, color = areaCode)) +
  geom_line() +
  geom_line(aes(y = mean_activity), color = "black", linetype = "dashed") +
  labs(x = "Week Ending", y = "Activity") +
  ggtitle("Temporal Line Plot of Activity and Weekly Mean Activity") +
  theme_minimal()+
  theme(legend.position = "none")+
  scale_x_date(date_breaks = "1 month")


###spatial plots
read_sf('MSOA_boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform('OGC:CRS84') |> 
  filter(MSOA11CD %in% unique(london_df$areaCode)) -> 
  msoa 
# 
plot(msoa$geometry)

##join geometry in the london_df df
london_df |> inner_join(msoa, by = c("areaCode"="MSOA11CD")) -> london_df
st_as_sf(london_df) -> london_df

#
date_from <- "2020-10-01"
date_upto <- "2021-10-01"

ggplot(subset(london_df, date > date_from & date < date_upto)) +
  geom_sf(aes(fill = weekly_mean_activity), linewidth=0.0001, alpha=0.9) + 
  scale_fill_viridis_c(
    # trans = "sqrt",
    # breaks=c(0, 100, 200, 500, 1700),
    # name = "caserate"
  )+
  facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
  labs(title = "caserate") +
  theme_minimal() +
  theme(legend.position.inside = c(0.5, 0),
        axis.text = element_blank(),  # Remove axis text for better visualization
        axis.title = element_blank())   # Remove axis titles


ggplot(subset(london_df, date > date_from & date < date_upto)) +
  geom_sf(aes(fill = caserate), linewidth=0.0001, alpha=0.9) + 
  scale_fill_viridis_c(
    # trans = "sqrt",
    # breaks=c(0, 100, 200, 500, 1700),
    # name = "caserate"
  )+
  facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
  labs(title = "caserate") +
  theme_minimal() +
  theme(legend.position.inside = c(0.5, 0),
        axis.text = element_blank(),  # Remove axis text for better visualization
        axis.title = element_blank())   # Remove axis titles

