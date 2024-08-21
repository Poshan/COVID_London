library(sf)
library(dplyr)
library(arrow)

# Duplicate removal ------------------------------------------------------------
## Load data -------------------------------------------------------------------

##set the workplace to Sciebo as the data is large
setwd("/cloud/sciebo")

##the dataset contains activity values with larger decimal places
options(digits = 19)


##load the activity data
##downloaded from the mapbox provided dataset
df <- read.csv("England/Activity_data/London_activity_data.csv")


## Distinct locations of the activity data ----------------------------------------------

##
##using the combination of the lon and lat to create a new dataframe and
##unique id LONLAT_ID

distinct_LONLAT <- df |> distinct(XLON, XLAT)

#give id to the distinct locations
distinct_LONLAT$LONLAT_ID <- 1: length(distinct_LONLAT$XLON)

#write it in the directory
write_parquet(distinct_LONLAT, "England/distinct_LONLAT.parquet")

##add this id to the original dataframe
df_2 <- inner_join(df, distinct_LONLAT, by = c("XLON", "XLAT"))

## (optional) Compare the duplicate activity data in same location  -------------
## check the difference between the two/more activities in same locations

## checking the duplicates ()
duplicates <- df_1[duplicated(df_2[c("AGG_DAY_PERIOD", "LONLAT_ID")]) |
                     duplicated(df_2[c("AGG_DAY_PERIOD", "LONLAT_ID")],
                                fromLast = TRUE), ]

# ##finding how different is the duplicate values
grouped_data <- duplicates %>%
  group_by(AGG_DAY_PERIOD, XLAT, XLON) %>%
  mutate(activity_range = max(ACTIVITY_INDEX_TOTAL) - min(ACTIVITY_INDEX_TOTAL),
         activity_sd = sd(ACTIVITY_INDEX_TOTAL))

# # Filter rows where there are differences in activity levels
differences_data <- grouped_data %>%
  filter(activity_range > 0 | activity_sd > 0)

# # Print the rows with differences
print(differences_data)




## Mean the values in the duplicated time and places ---------------------------
df_mean <- df_2 |>
  group_by(AGG_DAY_PERIOD, LONLAT_ID) |>
  summarize(mean_column = mean(ACTIVITY_INDEX_TOTAL, na.rm = TRUE))



## To make sure there is datapoint for all the dates and all locations---------
########### Filling the not available dates and geography by na

# Create a dataframe with all combinations of dates and geographies
all_combinations <- expand.grid(
  AGG_DAY_PERIOD = unique(df_mean$AGG_DAY_PERIOD),
  LONLAT_ID = unique(df_mean$LONLAT_ID)
)

# Left join with the original dataframe
merged_data <- left_join(all_combinations, df_mean,
                         by = c("AGG_DAY_PERIOD", "LONLAT_ID"))


##write to not repeat process again
write_parquet(df_mean, "England/data/processed_data/pre_processed_movement.parquet")
