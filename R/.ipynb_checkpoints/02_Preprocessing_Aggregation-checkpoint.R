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
merged_data <- read_parquet("England/pre_processed_movement.parquet")

##distinct locations
distinct_LONLAT <- read_parquet("England/distinct_LONLAT.parquet")



## Read Polygon file to which we aggregate--------------------------------------

msoa <- read_sf('England/MSOA_boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform(crs = "OGC:CRS84") 

id <- "MSOA11CD"

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
df <- left_join(merged_data, activitylocation_msoa, by=c("LONLAT_ID"))
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
