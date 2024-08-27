library(INLA)
library(Metrics)
library(ggplot2)
##run 06_Modeling_create_graph.R before running this R file 
## to get the graphs structure
source("England/R/06_Modeling_create_graph.R")
##load the helper function
source("England/R/helper_functions.R")

london_df <- read_parquet("England/data/processed_data/london_df_mobilitylaggedactivity.parquet")
### Model function -----
### runs the INLA function

## parameters: formula and family - distribution (default is poisson)
## returns:  
##    formula, model, summary of model, 
##    result in form (observed cases, date, regionID(MSOAcode), weekly_mean_activity),
##    r-squared, and Root Mean squared error

inlaModels <- function(formula, family="poisson"){
  df <- london_df 
  m <- inla(formula = formula,
            family = family,
            offset = log(Population),
            data = df,
            control.compute = list(waic = T, cpo = T),
            verbose = TRUE)
  sum <- summary(m)
  
  ##resulting cases
  result <- cbind(m$summary.fitted.values,
                  obs=df$caserate,
                  week=df$date,
                  msoa=df$areaCode,
                  weekly_mean_activity = df$weekly_mean_activity)
  ##residual plot
  result$residue <- result$mean - result$obs
  # plot(cc1, ylim = c(-200, 200))
  # abline(h=0, col="red")
  
  r2 <- R_squared(result$obs, result$mean)
  
  ###rmse of the models
  rmse <- rmse(result$obs, result$mean)
  
  return(list(formula = formula, model = m, summary = sum, result = result, r2 = r2, rmse = rmse))
  
}

## Non-Spatial Model------
 
## Spatial Models ----

### No covariates ----
f0 <- as.integer(caserate) ~ 1 + 
                                f(week, model = "rw2", constr = TRUE) + 
                                f(new_id, model="bym2", graph = msoa.graph) 

### Activity Within the regions -----
f1 <- as.integer(caserate) ~ 1 + weekly_mean_activity +
                                f(week, model = "rw2", constr = TRUE) + 
                                f(new_id, model="bym2", graph = msoa.graph) 

### Activity within + Activity in mobility neighbors
f2 <- as.integer(caserate) ~ 1+ weekly_mean_activity + 
                                MobilityLag + 
                                f(week, model = "rw2", constr = TRUE) + 
                                f(new_id, model="bym2", graph = msoa.graph)

f3 <- as.integer(caserate) ~ 1+ weekly_mean_activity + 
                                weekly_mean_activity_lag1 + 
                                f(week, model = "rw2", constr = TRUE) + 
                                f(new_id, model="bym2", graph = msoa.graph)
# 
# f4 <- as.integer(caserate) ~ 1+ weekly_mean_activity + 
#                                 weekly_mean_activity_lag2 + 
#                                 f(week, model = "rw2", constr = TRUE) + 
#                                 f(new_id, model="bym2", graph = msoa.graph)
# 
# f5 <- as.integer(caserate) ~ 1+ weekly_mean_activity + 
#                                 MobilityLag_weeklag_1 + 
#                                 f(week, model = "rw2", constr = TRUE) + 
#                                 f(new_id, model="bym2", graph = msoa.graph)         
# f6 <- as.integer(caserate) ~ 1+ weekly_mean_activity + 
#                                 MobilityLag_weeklag_2 + 
#                                 f(week, model = "rw2", constr = TRUE) + 
#                                 f(new_id, model="bym2", graph = msoa.graph)
# 
f7 <- as.integer(caserate) ~ 1+ weekly_mean_activity +
                                  MobilityLag +
                                  weekly_mean_activity_lag1 +
                                  weekly_mean_activity_lag2 +
                                  MobilityLag_weeklag_1 +
                                  MobilityLag_weeklag_2 +
                                  f(week, model = "rw2", constr = TRUE) +
                                  f(new_id, model="bym2", graph = msoa.graph)
## Model Runs ----
f <- c(f0, f1, f2, f7)

inla_runs <- lapply(f, inlaModels)



## Model evaluations -------

#### Residuals and Residual plots ----------------------
summary(inla_runs[[1]]$result$residue)

plot(inla_runs[[1]]$result$residue)


summary(inla_runs[[2]]$result$residue)
plot(inla_runs[[2]]$result$residue)

summary(inla_runs[[3]]$result$residue)
plot(inla_runs[[3]]$result$residue)

summary(inla_runs[[4]]$result$residue)
plot(inla_runs[[4]]$result$residue)
#### Summaries ---------
summary(inla_runs[[1]]$model)
summary(inla_runs[[2]]$model)
summary(inla_runs[[3]]$model)
summary(inla_runs[[4]]$model)

#### CPO ------------------
cpo <- log(inla_runs[[1]]$model$cpo$cpo)
cpo <- cpo[is.finite(cpo)]
cpo1 <- sum(cpo, na.rm = TRUE)
cpo <- log(inla_runs[[2]]$model$cpo$cpo)
cpo <- cpo[is.finite(cpo)]
cpo2 <- sum(cpo, na.rm = TRUE)
cpo <- log(inla_runs[[3]]$model$cpo$cpo)
cpo <- cpo[is.finite(cpo)]
cpo3 <- sum(cpo, na.rm = TRUE)
cpo <- log(inla_runs[[4]]$model$cpo$cpo)
cpo <- cpo[is.finite(cpo)]
cpo4 <- sum(cpo, na.rm = TRUE)

#### Spatial random effect plots ----------------

if(!exists("msoa")){
  msoa <- read_sf('England/data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
    st_transform('OGC:CRS84') |> 
    filter(MSOA11CD %in% unique(london_df$areaCode))
}

plot_spatial_random_effect(inla_runs[[1]]$model, msoa, "new_id")
plot_spatial_random_effect(inla_runs[[2]]$model, msoa, "new_id")
plot_spatial_random_effect(inla_runs[[3]]$model, msoa, "new_id")
plot_spatial_random_effect(inla_runs[[4]]$model, msoa, "new_id")

#### Temporal random effect plots ----------------
plot_temporal_random_effect(inla_runs[[1]]$model, "week")
plot_temporal_random_effect(inla_runs[[2]]$model, "week")
plot_temporal_random_effect(inla_runs[[3]]$model, "week")
plot_temporal_random_effect(inla_runs[[4]]$model, "week")

#### Observed Vs Fitted caserates Temporal ----------
areas <- selected_area_code(london_df, 5)
lapply(areas, comparision_plot_temporal)


#### Observed Vs Fitted caserates Spatial -----------
selected_dates = c("2020-11-09", "2020-03-30", "2020-12-21", "2020-08-24")
lapply(selected_dates, comparision_plot_spatial)





