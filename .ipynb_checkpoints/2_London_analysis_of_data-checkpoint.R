# -*- coding: utf-8 -*-
library(sf)
library(dplyr)
library(tidyr)

library(spacetime)
library(sp)
library(arrow)

library(spdep)
setwd('/palma/scratch/tmp/pniraula/London/')



##read the pre-processed parquet file
merged_data <- read_parquet("pre_processed_movement.parquet")
distinct_LONLAT <- read_parquet("distinct_LONLAT.parquet")
distinct_LONLAT |> drop_na(XLAT) -> distinct_LONLAT


# ##################Mean and aggregation at the MSOA level


##read file
##CRS transform to 
read_sf('MSOA_boundary/MSOA_2011_London_gen_MHW.shp') |> st_transform(crs = "OGC:CRS84") -> msoa 


sf_use_s2(FALSE)


##make sf from the unique geography
st_as_sf(distinct_LONLAT, crs="OGC:CRS84", coords = 
           c("XLON", "XLAT")) -> distinct_LONLAT.sf 


##aggregate spatially
##spatial join
joined_df_unique <- st_join(st_make_valid(msoa), st_make_valid(distinct_LONLAT.sf))

joined_df_unique <- drop_na(joined_df_unique)

joined_df_unique |> select(one_of("MSOA11CD", "LONLAT_ID")) -> joined_df_unique

##join the spatially joined result with the activity merged data
merged_df_new <- left_join(merged_data, joined_df_unique, by=c("LONLAT_ID"))

# merged_df_new <- select(merged_df_new, -one_of(c("geometry", "GlobalID", "MSOA01NMW", "MSOA01NM")))

# # ################################Spatial autocorrelation measures###############
# # ##Analysing the number of grids in the zipcodes ###############################
# # ###############################################################################
# 
# merged_df_new |> drop_na() |> group_by(week) |> count(MODZCTA) -> count_per_MODZCTA
# 
# hist(count_per_MODZCTA$n)
# 
# 
# 
# 
# # #computing the global spatial autocorrelation 
# # #get randomly selected points
# # Create the random points (here, 5 random points for each polygon)
# # set.seed(452)
# # points_thomas <- st_sample(distinct_LONLAT.sf, size = 100, type = "Thomas")
# # points_random <- st_sample(distinct_LONLAT.sf, size = 100, type = "Random")
# # st_set_crs(points_thomas, st_crs(modzcta)) -> points_thomas
# # st_set_crs(points_random, st_crs(modzcta)) -> points_random
# # st_crop(points_random, modzcta) -> points_random
# 
# 
# ###randomly sampling such that there are 5 points in each modzcta
# st_sample(modzcta, size = c(4,4), type="random", replace=TRUE) ->points_random
# ##join wiht the actual latlong data
# 
# st_join(st_as_sf(points_random), distinct_LONLAT.sf, join = st_is_within_distance, dist = 100) -> points_random_samples
# 
# 
# ##sample values from the data
# inner_join(points_random_samples, merged_df_new, by=join_by(LONLAT_ID == LONLAT_ID)) -> random_samples
# 
# ##remove the na in columns mean_column and label
# random_samples |> filter(complete.cases(mean_column, label)) -> random_samples
# ##randomly select 30 days 
# sampled_dates <- sample(unique(merged_df_new$AGG_DAY_PERIOD), 150, replace = FALSE)
# random_samples |> filter(AGG_DAY_PERIOD %in% sampled_dates) -> random_samples
# 
# 
# computeMorans <- function (date, nn = 10){
#   ### compute moran's I for each day
#   ### date = date in string
#   ### nn = number of nearest neighbors
#   # print(date)
#   random_samples |> filter(AGG_DAY_PERIOD == date) -> random_sample_of_day
#   knea <- knearneigh(random_sample_of_day, k =nn, longlat = TRUE)
#   neib <- knn2nb(knea)
#   
#   # neib_thomas <- knn2nb(knearneigh(st_cast(thomas_samples$geometry, "POINT"), k = 4, longlat=TRUE))
#   
#   ##neighbors based on distance upto 1 km
#   ##neib <- dnearneigh(random_samples, 0, 1, longlat = TRUE)
#   
#   ##weights assigned for each neighbor 
#   lw <- nb2listw(neib, style="W", zero.policy=TRUE)
#   
#   ##lagged activity rate
#   rate.lag <- lag.listw(lw, random_sample_of_day$mean_column)
#   
#   ##plot and see the lagged value
#   # plot(rate.lag ~ random_sample_of_day$mean_column, pch=16, asp=1)
#   M1 <- lm(rate.lag ~ random_sample_of_day$mean_column)
#   print(moran.test(random_sample_of_day$mean_column, lw, alternative = "greater"))
#   # abline(M1, col="blue")
#   # print(coef(M1)[2])
#   return(M1)
# }
# 
# Ms <- lapply(unique(random_samples$AGG_DAY_PERIOD), FUN = computeMorans)
# returncoef <- function(M){
#   x = (coef(M)[2])
#   return(unname(as.numeric(x)))
# } 
# 
# Mcoefs<-lapply(Ms, returncoef)
# hist(unlist(Mcoefs))
# boxplot(unlist(Mcoefs))
# 
# summary(unlist(Mcoefs))
# 
# # ####Daily morans i computation 
# # ##Min. 1st Qu.  Median  Mean   3rd Qu.   Max. 
# # #0.3549  0.4661  0.5069  0.4977  0.5520  0.5791 
# 


# ################################AGGREGATION###################################

######################1. Simple mean##########################################
##aggregate by the modzcta and day
aggregate(mean_column ~ MSOA11CD + AGG_DAY_PERIOD, data = merged_df_new, mean) -> mean_by_msoa

# Convert AGG_DAY_PERIOD to Date object
mean_by_msoa$AGG_DAY_PERIOD <- as.Date(mean_by_msoa$AGG_DAY_PERIOD)

write_parquet(mean_by_msoa, "mean_by_msoa.parquet")

# #######multiple aggregations

# merged_df_new |> 
#   group_by(MODZCTA, AGG_DAY_PERIOD) |> 
#   summarise_at(vars(mean_column), c(mean, var)) -> 
#   mean_var_by_modzcta


# Extract week of the year
mean_by_msoa$week <- lubridate::week(mean_by_msoa$AGG_DAY_PERIOD)

##for week 53 set it to week 52
mean_by_msoa <- mean_by_msoa |>
  mutate(week = if_else(week == 53, 52, week))

# Group by MODZCTA and week, then summarize to get the mean
weekly_mean <- mean_by_msoa %>%
  group_by(MSOA11CD, week) %>%
  summarise(weekly_mean = mean(mean_column))

weekly_mean$date <- as.Date(paste(2020, weekly_mean$week, 1, sep="-"), "%Y-%U-%u")

write_parquet(weekly_mean, "weekly_mean_by_msoa.parquet")


# ###############################################################################
# #####Analysing the spatial autocorrelation on the aggregated units#############
# ###############################################################################
# weekly_mean_joined <- inner_join(weekly_mean, modzcta, by = join_by(MODZCTA == MODZCTA))
# st_as_sf(weekly_mean_joined) -> weekly_mean_joined
# 
# 
# # ##one structure to re-compute the moran's I later
# 
# computeMorans_modzcta <- function (weeks){
# 
#   weekly_mean_joined |> filter(week == weeks) -> s
# 
#   nb <- poly2nb(s, queen=TRUE)
#   #
#   # ##weights assigned for each neighbor
#   lw <- nb2listw(nb, style="W", zero.policy=TRUE)
# 
#   # ##lagged activity rate
#   rate.lag <- lag.listw(lw, s$weekly_mean)
#   #
#   # ##plot and see the lagged value
#   # # plot(rate.lag ~ random_sample_of_day$mean_column, pch=16, asp=1)
#   M1 <- lm(rate.lag ~ s$weekly_mean)
#   # # abline(M1, col="blue")
#   # # print(coef(M1)[2])
#   print(moran.test(s$weekly_mean, lw, alternative = "greater"))
#   return(M1)
# }
# 
# Ms_agg <- lapply(unique(weekly_mean_joined$week), FUN = computeMorans_modzcta)
# 
# M_agg.coefs<-lapply(Ms_agg, returncoef)
# hist(unlist(M_agg.coefs))
# boxplot(unlist(M_agg.coefs))
# summary(unlist(M_agg.coefs))
# 
# weekly_mean_joined |> filter(week == 8) -> s
# nb <- poly2nb(s, queen=TRUE)
# 
# ##weights assigned for each neighbor 
# lw <- nb2listw(nb, style="W", zero.policy=TRUE)
# moran.test(s$weekly_mean,lw, alternative="greater")
# 


# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##aggregate into larger grids

# ##############################################################################
# ##############################################################################
# ##############################################################################

# Function to aggregate data into larger grid sizes
# Function to aggregate data into larger grid sizes
aggregate_data <- function(merged_data, distinct_LONLAT.sf, grid_size = 1000) {
  
  #transform to local UTM grids
  distinct_LONLAT.sf |> st_transform(crs = 2263) -> distinct_LONLAT_proj
  
  # Create a regular grid covering the extent of the study area
  st_make_grid(distinct_LONLAT_proj, cellsize = c(grid_size, grid_size)) |> st_transform(crs=4326) |> st_as_sf() -> grid
  
  #asisng unique id -sequential
  grid$grid_ID <- seq_along(grid$x)
  
  #intersect/spatial join the grid with teh distinct_sf
  st_join(distinct_LONLAT.sf, grid) -> joined_latlong_grid
  
  #drop geometry to make computations faster
  joined_latlong_grid <- st_drop_geometry(joined_latlong_grid)
  
  ##merge the dataset containing the  activity data with the grid and longlat  
  merge(merged_data, joined_latlong_grid, by = "LONLAT_ID", all.x = TRUE) -> merged_all

    
  ##aggregate on the grids
  grid_mean <- merged_all %>%
    group_by(grid_ID, AGG_DAY_PERIOD) %>%
    summarise(grid_mean = mean(mean_column))
  
  ##drop all the rows with out the value in grid_mean
  grid_mean |> drop_na(any_of("grid_mean")) -> grid_mean
  
  ##return the grid and the grid mean
  return (c(grid, grid_mean))
}

merged_data |> drop_na() -> merged_data_enw

activity_grid_1000 <- aggregate_data(merged_data_enw, distinct_LONLAT.sf, 1000)
