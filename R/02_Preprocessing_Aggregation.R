# -*- coding: utf-8 -*-
library(sf)
library(dplyr)
library(tidyr)
library(spacetime)
library(sp)
library(arrow)
library(spdep)

##set the workplace to Sciebo as the data is large
setwd("/cloud/sciebo")

## Read 

## duplicates mean data from step 1
# merged_data <- read_parquet("England/pre_processed_movement.parquet")

##distinct locations
# distinct_LONLAT <- read_parquet("England/distinct_LONLAT.parquet")



## Read Polygon file to which we aggregate--------------------------------------

# msoa <- read_sf('England/MSOA_boundary/MSOA_2011_London_gen_MHW.shp') |> 
#   st_transform(crs = "OGC:CRS84") 

id <- "MSOA11CD"


##when sciebo is not working
setwd('/home/p/pniraula')
merged_data <- read_parquet("England/data/processed_data/pre_processed_movement.parquet")
distinct_LONLAT <- read_parquet("England/data/processed_data/distinct_LONLAT.parquet")
msoa <- read_sf('England/data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform(crs = "OGC:CRS84")


#Spherical geometry (s2) switched off
sf_use_s2(FALSE)

##remove na in distinct locations if any
distinct_LONLAT |> drop_na(XLAT) -> distinct_LONLAT

##make sf from the distinct locations
st_as_sf(distinct_LONLAT, crs="OGC:CRS84", coords = 
           c("XLON", "XLAT")) -> distinct_LONLAT.sf 

##In which polygon does the distinct location lie? ----------------------------- 

### Create mapping df between the polygon and the activity location-----------
##spatial join
activitylocation_msoa <- st_join(st_make_valid(msoa), 
                            st_make_valid(distinct_LONLAT.sf))

##remove all points that don't fall in any polygons
activitylocation_msoa <- drop_na(activitylocation_msoa)

activitylocation_msoa |> select(one_of(id, "LONLAT_ID")) -> activitylocation_msoa

### merge activity data with the mapping df -----------------------------------
##join the spatially joined result with the activity merged data
df <- left_join(merged_data, activitylocation_msoa, by=c("LONLAT_ID")) |> drop_na()
write_parquet(df, "England/df_activity_withMSOA.parquet")
## AGGREGATION------------------------------------------------------------------

### Spatial aggregation -------------------------------------------------------

##aggregate by the each polygon and day
mean_by_msoa <- aggregate(mean_column ~ MSOA11CD + AGG_DAY_PERIOD, 
                          data = df, mean) 


# Convert AGG_DAY_PERIOD to Date object
mean_by_msoa$AGG_DAY_PERIOD <- as.Date(mean_by_msoa$AGG_DAY_PERIOD)

write_parquet(mean_by_msoa, "England/mean_by_msoa.parquet")

### Temporal aggregation ------------------------------------------------------
mean_by_msoa <- read_parquet("England/mean_by_msoa.parquet")
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

write_parquet(weekly_mean, "England/weekly_mean_by_msoa.parquet")


## OPTIONAL: Aggregate to larger grids -----------------------------------------
# ##aggregate into larger grids 
# no temporal aggregation

# Function to aggregate data into larger grid sizes
aggregate_data <- function(msoa, merged_data, distinct_LONLAT.sf, grid_size) {
  ##parameters of the function
  #msoa: type sf, outer boundary of the region, in 4326 CRS 
  
  #merged_data: type df, the activity_data should contain columns 
  ## LONLAT_ID, AGG_DAY_PERIOD, mean_column
  
  # distinct_LONLAT.sf: type sf, related to merged_data or the geometry of the 
  ## activities. i.e points in which there are activity
  
  # grid_size: type numeric, size of the grid (larger than 100m)
  
  # Transform to local UTM grids for metric computations
  distinct_LONLAT_proj <- st_transform(distinct_LONLAT.sf, crs = 32630)
  msoa_proj <- st_transform(msoa, crs = 32630)
  
  # Create a regular grid covering the extent of the study area
  grid <- st_make_grid(msoa_proj, cellsize = c(grid_size, grid_size)) |>
    st_as_sf() |>
    st_intersection(msoa_proj) |> 
    st_as_sf() |>
    select(any_of(c("MSOA11CD", "x")))
  
  # Assign unique IDs to the grid cells
  grid$grid_ID <- seq_len(nrow(grid))
  
  # Spatial join the grid with the point data
  joined_latlong_grid <- st_join(distinct_LONLAT_proj, grid, left = FALSE, join = st_intersects)
  
  # Drop geometry to make computations faster
  joined_latlong_grid <- st_drop_geometry(joined_latlong_grid)
  
  #merge activity data with the grid id and activity latlng id
  merged_data |> 
    left_join(joined_latlong_grid, by = c("LONLAT_ID")) |>
    drop_na() ->
    merged_data
  
  # Aggregate the data by grid cells
  grid_mean <- merged_data %>%
    group_by(grid_ID, AGG_DAY_PERIOD) %>%
    summarise(grid_mean = mean(mean_column, na.rm = TRUE), .groups = 'drop')
  
  # Drop rows without values in grid_mean
  grid_mean <- drop_na(grid_mean, any_of("grid_mean"))
  
  # Return the grid and the aggregated data
  return(list(grid = grid, grid_mean = grid_mean))
}

activity_grid_1000 <- aggregate_data(msoa, merged_data, distinct_LONLAT.sf, 1000)

grid_1000 <- activity_grid_1000$grid |> select(any_of(c("grid_ID","x")))

write_parquet(activity_grid_1000$grid_mean, "England/data/processed_data/activity_in_1000m_grids.parquet")
st_write(grid_1000, "England/data/processed_data/1000m_grids.shp")
