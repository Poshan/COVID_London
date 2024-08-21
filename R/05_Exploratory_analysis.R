library(ggplot2)
library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(flexplot)
# 
london_df <- read_parquet("England/data/processed_data/london_weekly_data_with_caserate.parquet")


## Temporal plots -------------------------------------------------------------
### Calculate overall weekly mean caserate ----------------------------------
weekly_mean_caserate <- london_df |>
  group_by(date) |>
  summarise(mean_caserate = mean(caserate, na.rm = TRUE))

london_df <- left_join(london_df, weekly_mean_caserate, by = "date")
### Calculate overall weekly mean activity ----------------------------------
weekly_mean_activitys_act <- london_df |>
  group_by(date) |>
  summarise(mean_activity = mean(weekly_mean_activity, na.rm = TRUE))

london_df <- left_join(london_df, weekly_mean_activitys_act, by = "date")


### Temporal plot of caserate --------------------------------------------------
# Plot using ggplot
ggplot(london_df, aes(x = date, y = caserate, group = areaCode, color = areaCode)) +
  geom_line() +
  geom_line(aes(y = mean_caserate), color = "black", linetype = "dashed") +
  labs(x = "date", y = "Case Rate") +
  # ggtitle("COVID-19 cases per 100000 population") +
  theme_minimal() +
  theme(legend.position = "none",
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_date(date_breaks = "3 month")


### Temporal plot of mean activity----------------------------------------------
ggplot(london_df, aes(x = date, y = weekly_mean_activity, group = areaCode, color = areaCode)) +
  geom_line() +
  geom_line(aes(y = mean_activity), color = "black", linetype = "dashed") +
  labs(x = "Date", y = "Activity index") +
  # ggtitle("Weekly Mean Activity") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  scale_x_date(date_breaks = "3 month")

## Spatial Plot ----------------------------------------------------------------
###spatial plots
library(sf)
read_sf('England/data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform('OGC:CRS84') |> 
  filter(MSOA11CD %in% unique(london_df$areaCode)) -> 
  msoa 


##join geometry in the london_df df
london_df |> inner_join(msoa, by = c("areaCode"="MSOA11CD")) -> london_df
st_as_sf(london_df) -> london_df

#
date_from <- "2020-10-01"
date_upto <- "2021-10-01"

plot_caserate_activity_spatial <- function (date_from, date_upto){  
  p1<-ggplot(subset(london_df, date > date_from & date < date_upto)) +
    geom_sf(aes(fill = weekly_mean_activity), linewidth=0.0001, alpha=0.9) + 
    scale_fill_viridis_c(
      # trans = "sqrt",
      # breaks=c(0, 100, 200, 500, 1700),
      # name = "caserate"
    )+
    facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
    labs(title = "mean activity") +
    theme_minimal() +
    theme(legend.position.inside = c(0.5, 0),
          axis.text = element_blank(),  # Remove axis text for better visualization
          axis.title = element_blank())   # Remove axis titles
  
  
  p2 <-ggplot(subset(london_df, date > date_from & date < date_upto)) +
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
  return (list(activity_plot = p1, caserate_plot = p2))
}  
plots <- plot_caserate_activity_spatial(date_from, date_upto)

## flexplot --------------------------------------------------------------------
london_df |> 
  flexplot(caserate ~ weekly_mean_activity | wave, data = _, method = "poisson")

## Activity_aggregated at different levels ------------------------------------

##plot a days activity
##plot a days activity
activity_grid_1000 <- read_parquet("England/data/processed_data/activity_in_1000m_grids.parquet")
grid_1000 <- st_read("England/data/processed_data/1000m_grids.shp")
activity_grid_1000 |> filter(AGG_DAY_PERIOD == "2020-03-08") |> left_join(grid_1000, by = c("grid_ID" = "grid_ID")) ->day_df

library(ggplot2)
ggplot(st_as_sf(day_df)) +
  geom_sf(aes(fill = grid_mean), linewidth=0.0001, alpha=0.9) + 
  scale_fill_viridis_c(
    # trans = "sqrt",
    # breaks=c(0, 100, 200, 500, 1700),
    # name = "caserate"
  )+
  # facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
  labs(title = "Activity") +
  theme_minimal() +
  theme(legend.position.inside = c(0.5, 0),
        axis.text = element_blank(),  # Remove axis text for better visualization
        axis.title = element_blank())   # Remove axis titles



