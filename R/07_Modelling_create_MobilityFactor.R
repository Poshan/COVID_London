##run 06_Modeling_create_graph.R before running this R file
source("England/R/06_Modeling_create_graph.R")
## Create mobility factor from Mobility graph and weekly_mean_activity -----

### read the neighborhood file --------
msoa_wl_m <- nb2listw(neighbors_list_i, glist = weights_list_i, style = "W", zero.policy = TRUE)

### Create covairance matrix -------
con_m <- as.matrix(listw2mat(msoa_wl_m))

### Repeat the covariate matrix to multiple time period -----
num_time_periods <- length(unique(london_df$date))

k_m <- kronecker(diag(1,num_time_periods), con_m)


### Create a factor by multiplying the covariate matrix and weekly activity mean ----
london_df$MobilityLag <- k_m%*%london_df$weekly_mean_activity 

### Introduce temporal lags in the activities by 1 and 2 weeks -----
#### Create temporally lagged columns of weekly mean activity -------------
lag_df <- london_df |>
  group_by(areaCode) |>
  arrange(date) |>
  mutate(
    weekly_mean_activity_lag1 = lag(weekly_mean_activity, 1),
    weekly_mean_activity_lag2 = lag(weekly_mean_activity, 2)
  ) |>
  ungroup() |>
  select(any_of(c("areaCode","date", "weekly_mean_activity_lag1", "weekly_mean_activity_lag2")))

london_df |> left_join(lag_df, by = c("areaCode" = "areaCode", "date"="date")) -> london_df

##set na as 0
london_df |>
  mutate(weekly_mean_activity_lag1 = if_else(is.na(weekly_mean_activity_lag1), 0, weekly_mean_activity_lag1)) |>
  mutate(weekly_mean_activity_lag2 = if_else(is.na(weekly_mean_activity_lag2), 0, weekly_mean_activity_lag2)) ->
  london_df

#### Create a factor by multiplying the covariate matrix and weekly activity mean (temporally lagged by one and two weeks) ----
london_df$MobilityLag_weeklag_1 <- k_m%*%london_df$weekly_mean_activity_lag1 
london_df$MobilityLag_weeklag_2 <- k_m%*%london_df$weekly_mean_activity_lag2 

##save teh mobility_lag_df
write_parquet(london_df, "England/data/processed_data/london_df_mobilitylaggedactivity.parquet")


