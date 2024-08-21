
library(Metrics)
library(ggplot2)

london_df <- read_parquet("England/data/processed_data/london_df_mobilitylaggedactivity.parquet")
### Model function -----
### runs the INLA function

## parameters: formula and family - distribution (default is poisson)
## returns:  
##    formula, model, summary of model, 
##    result in form (observed cases, date, regionID(MSOAcode), weekly_mean_activity),
##    r-squared, and Root Mean squared error

inlaModels <- function(formula, family="poisson"){
  m <- inla(formula = formula,
            family = family,
            offset = log(Population),
            data = london_df,
            control.compute = list(waic = T, cpo = T),
            verbose = TRUE)
  sum <- summary(m)
  
  ##resulting cases
  result <- cbind(m$summary.fitted.values,
                  obs=london_df$caserate,
                  week=london_df$date,
                  msoa=london_df$areaCode,
                  weekly_mean_activity = london_df$weekly_mean_activity)
  ##residual plot
  result$residue <- result$mean - result$obs
  # plot(cc1, ylim = c(-200, 200))
  # abline(h=0, col="red")
  
  r2 <- R_squared(result$obs, result$mean)
  
  ###rmse of the models
  rmse <- rmse(result$obs, result$mean)
  
  return(list(formula = formula, model = m, summary = sum, result = result, r2 = r2, rmse = rmse))
  
}

### Helper functions -----

#### R squared computation ---------------
R_squared <- function(y, fv){
  mean_y <- mean(y)
  TSS <- sum((y-mean_y)^2)
  res <- y - fv
  RSS <- sum(res^2)
  Rsq <- 1- (RSS/TSS)
  return(Rsq)
}

#### Function to plot spatial random effect --------
plot_spatial_random_effect <- function(result, spatial_data, id_col) {
  spatial_effects <- result$summary.random[[id_col]]
  # Extract the formula
  # model_formula <- result$.args$formula |> 
  #   deparse() |> 
  #   paste(collapse = " ") |>  # Collapse the character vector into a single string
  #   str_replace_all(regex("\\s+"), " ") |>  # Replace multiple whitespaces with a single space
  #   str_trim() |>
  #   str_wrap(width=80)
  
  # Merge spatial effects with spatial data
  spatial_data <- spatial_data %>%
    mutate(areaCode_id = row_number()) %>%
    left_join(spatial_effects, by = c("areaCode_id" = "ID"))
  
  plot <- ggplot(spatial_data) +
    geom_sf(aes(fill = mean), linewidth = 0.2, alpha = 0.9) + 
    scale_fill_viridis_c() + 
    labs(title = "Spatial random effect") +
    theme_minimal() +
    theme(legend.position = "right",  
          axis.text = element_blank(),  
          axis.title = element_blank())
  return (plot)
}

#### Function to plot temporal random effect ------------------------
plot_temporal_random_effect <- function(model, id_col){
  tmp_eff <- model$summary.random[[id_col]]
  temporal_df <- data.frame(
    time = tmp_eff$ID,
    mean = tmp_eff$mean,
    lower = tmp_eff$`0.025quant`,
    upper = tmp_eff$`0.975quant`
  )
  

  # Plot the temporal effect
  plot <- ggplot(temporal_df, aes(x = time, y = mean, group=1)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    labs(title = "Temporal Effect",
         x = "Week",
         y = "Effect Size") +
    theme_minimal()
  
  return (plot)
}

#### Observed Vs Predicted Temporal plots ----------------------
comparision_plot_temporal <- function(areaCode){
  ###plot the results
  model <- inla_runs[[3]]
  model$result$week <- as.Date(model$result$week)
  plot <- ggplot(filter(model$result, msoa == areaCode), aes(x=week, y=mean, group=1))+
    geom_line(col = "blue")+
    geom_line(aes(x=week, y= obs), col="red")+
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`), alpha=0.3) +
    labs(x = "Date", y = "Caserate") +
    ggtitle(areaCode) +
    # theme_minimal()+
    theme(legend.position.inside = c(0.5, 0),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    scale_x_date(date_breaks = "2 month")
  return(plot)
}


#### Observed Vs Predicted Spatial Plots ------------------------
comparision_plot_spatial <- function(date, breaks){
  library(classInt)  # For natural breaks
  model <- inla_runs[[3]]
  model$result |> 
    left_join(msoa, by=c("msoa"="MSOA11CD")) |> 
    st_as_sf() -> 
    df_for_plots
  
  # Reshape the data to include a 'type' column 
  long_df <- df_for_plots|>
    filter(week == date)|>
    select(msoa, mean, obs, geometry)|>
    pivot_longer(cols = c(mean, obs), names_to = "type", values_to = "caserate")
  
  # Compute natural breaks for caserate
  n_breaks <- breaks  # Define how many breaks you want to compute; adjust as needed
  natural_breaks <- classIntervals(long_df$caserate, n = n_breaks, style = "sd")
  breaks <- natural_breaks$brks
  
  # Create the plot with facets and dynamic scaling
  combined_plot <- ggplot(long_df) +
    geom_sf(aes(fill = caserate, geometry = geometry), linewidth = 0.0001, alpha = 0.9) + 
    scale_fill_viridis_c(breaks = breaks) +
    facet_wrap(~ type, ncol = 2, labeller = as_labeller(c(mean = "Fitted Case Rate per 100,000", obs = "Observed Case Rate per 100,000"))) +
    labs(title = paste("Case Rate Analysis for Week:", date)) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  return(combined_plot)
}

#### Identify regions with highest and lowest caserates -----------------
selected_area_code <- function(london_df, n=10){
  ####identify top 10 areaCode with highest average number of cases
  
  # Calculate average caserate per areaCode
  average_caserates <- london_df %>%
    group_by(areaCode) %>%
    summarize(average_caserate = mean(caserate, na.rm = TRUE)) %>%
    ungroup()
  
  # Find the top 10 areaCodes with highest average caserates
  top_10_highest <- average_caserates %>%
    arrange(desc(average_caserate)) %>%
    slice_head(n = n)
  top_10_highest <- top_10_highest$areaCode
  
  # Find the top 10 areaCodes with lowest average caserates
  top_10_lowest <- average_caserates %>%
    arrange(average_caserate) %>%
    slice_head(n = n)
  top_10_lowest <- top_10_lowest$areaCode
  
  return(c(top_10_highest, top_10_lowest))
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

## Model Runs ----
f <- c(f0, f1, f2)
inla_runs <- lapply(f, inlaModels)


## Model evaluations -------

#### Residuals and Residual plots ----------------------
summary(inla_runs[[1]]$result$residue)
plot(inla_runs[[1]]$result$residue)

summary(inla_runs[[2]]$result$residue)
plot(inla_runs[[2]]$result$residue)

summary(inla_runs[[3]]$result$residue)
plot(inla_runs[[3]]$result$residue)

#### Summaries ---------
summary(inla_runs[[1]]$model)
summary(inla_runs[[2]]$model)
summary(inla_runs[[3]]$model)


#### CPO ------------------
cpo1 <- sum(log(inla_runs[[1]]$model$cpo$cpo), na.rm = TRUE)
cpo2 <- sum(log(inla_runs[[2]]$model$cpo$cpo), na.rm = TRUE)
cpo3 <- sum(log(inla_runs[[3]]$model$cpo$cpo), na.rm = TRUE)


#### Spatial random effect plots ----------------

if(!exists("msoa")){
  msoa <- read_sf('England/data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
    st_transform('OGC:CRS84') |> 
    filter(MSOA11CD %in% unique(london_df$areaCode))
}

plot_spatial_random_effect(inla_runs[[1]]$model, msoa, "new_id")
plot_spatial_random_effect(inla_runs[[2]]$model, msoa, "new_id")
plot_spatial_random_effect(inla_runs[[3]]$model, msoa, "new_id")

#### Temporal random effect plots ----------------
plot_temporal_random_effect(inla_runs[[1]]$model, "week")
plot_temporal_random_effect(inla_runs[[2]]$model, "week")
plot_temporal_random_effect(inla_runs[[3]]$model, "week")


#### Observed Vs Fitted caserates Temporal ----------
areas <- selected_area_code(london_df, 2)
lapply(areas, comparision_plot_temporal)


#### Observed Vs Fitted caserates Spatial -----------

comparision_plot_spatial("2020-10-19", 10)




