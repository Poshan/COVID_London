library(sf)
library(dplyr)
library(tidyr)
library(spdep)
library(arrow)
library(INLA)


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

##save teh mobility_lag_df
write_parquet(london_df, "England/data/processed_data/london_df_mobilitylaggedactivity.parquet")
