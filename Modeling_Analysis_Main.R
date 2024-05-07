# Purpose: Conduct main crux of modeling and all relevant analysis
# Our key goal is to examining the difference-in-differences between Black-White conditions during and following reconstruction
# Then we want to apply forecasting to predict counterfactual trends in the hypothetical scenario of prolonged reconstruction 

# Load needed packages
library(tidyverse) # For general use
library(haven) # For loading in .dta files
library(naniar) # For seeing missingness
library(tidycensus) # For getting built in state-county FIPS data 
library(tseries) # For working with time series data
library(forecast) # For creating forecasting models
library(bsts) # For state-space model
library(imputeTS)
library(bsts)

# Loads in the R environment containing the final versions of our clean data, as well as functions made
load("AnalysisData_Clean.RData")
# Can also read in CSV files from directory if needed 

############# Time Period ############# 
# Determine time periods of interest era
preCWreconstruction_start <- 1810 # Gives model idea of trends building into the civil war and then reconstruction
reconstruction_start <- 1863 # Strict start for reconstruction
reconstruction_end <- 1877 # Strict end for reconstruction
reconstruction_era_end <- 1890 # Looser definition of reconstruction "era"
# Filter data based on these periods

# Strict reconstruction 
Recon_B_Elect_State <- B_Elect_State_Year %>% 
  filter(Year >= preCWreconstruction_start & Year <= reconstruction_end) %>% 
  mutate(Time_Period = case_when(
    Year >= preCWreconstruction_start & Year < reconstruction_start ~ "Pre-Reconstruction",
    Year >= reconstruction_start & Year <= reconstruction_end ~ "Reconstruction",
    Year > reconstruction_end ~ "Post-Reconstruction",
    TRUE ~ NA_character_  # Handles any year outside the specified ranges
    )
  )

Recon_B_Elect_Year <- B_Elect_Year %>% 
  filter(Year >= preCWreconstruction_start & Year <= reconstruction_end) %>% 
  mutate(Time_Period = case_when(
    Year >= preCWreconstruction_start & Year < reconstruction_start ~ "Pre-Reconstruction",
    Year >= reconstruction_start & Year <= reconstruction_end ~ "Reconstruction",
    Year > reconstruction_end ~ "Post-Reconstruction",
    TRUE ~ NA_character_  # Handles any year outside the specified ranges
    )
  )
Recon_HistStat <- Hist_US_Stat_Main %>% 
  filter(Year >= preCWreconstruction_start & Year <= reconstruction_end) %>% 
  mutate(Time_Period = case_when(
    Year >= preCWreconstruction_start & Year < reconstruction_start ~ "Pre-Reconstruction",
    Year >= reconstruction_start & Year <= reconstruction_end ~ "Reconstruction",
    Year > reconstruction_end ~ "Post-Reconstruction",
    TRUE ~ NA_character_  # Handles any year outside the specified ranges
    )
  )
Recon_HistStat_OccDispr <- Hist_US_Stat_OccupDispar %>% 
  filter(Year >= preCWreconstruction_start & Year <= reconstruction_end) %>% 
  mutate(Time_Period = case_when(
    Year >= preCWreconstruction_start & Year < reconstruction_start ~ "Pre-Reconstruction",
    Year >= reconstruction_start & Year <= reconstruction_end ~ "Reconstruction",
    Year > reconstruction_end ~ "Post-Reconstruction",
    TRUE ~ NA_character_  # Handles any year outside the specified ranges
    )
  )
# Reconstruction era
Era_B_Elect_State <- B_Elect_State_Year %>% 
  filter(Year >= preCWreconstruction_start & Year <= reconstruction_era_end) %>% 
  mutate(Time_Period = case_when(
    Year >= preCWreconstruction_start & Year < reconstruction_start ~ "Pre-Reconstruction",
    Year >= reconstruction_start & Year <= reconstruction_end ~ "Reconstruction",
    Year > reconstruction_end ~ "Post-Reconstruction",
    TRUE ~ NA_character_  # Handles any year outside the specified ranges
    )
  )

Era_B_Elect_Year <- B_Elect_Year %>% 
  filter(Year >= preCWreconstruction_start & Year <= reconstruction_era_end) %>% 
  mutate(Time_Period = case_when(
    Year >= preCWreconstruction_start & Year < reconstruction_start ~ "Pre-Reconstruction",
    Year >= reconstruction_start & Year <= reconstruction_end ~ "Reconstruction",
    Year > reconstruction_end ~ "Post-Reconstruction",
    TRUE ~ NA_character_  # Handles any year outside the specified ranges
    )
  )

Era_HistStat <- Hist_US_Stat_Main %>% 
  filter(Year >= preCWreconstruction_start & Year <= reconstruction_era_end) %>% 
  mutate(Time_Period = case_when(
    Year >= preCWreconstruction_start & Year < reconstruction_start ~ "Pre-Reconstruction",
    Year >= reconstruction_start & Year <= reconstruction_end ~ "Reconstruction",
    Year > reconstruction_end ~ "Post-Reconstruction",
    TRUE ~ NA_character_  # Handles any year outside the specified ranges
    )
  )

Era_HistStat_OccDispr <- Hist_US_Stat_OccupDispar %>% 
  filter(Year >= preCWreconstruction_start & Year <= reconstruction_era_end) %>% 
  mutate(Time_Period = case_when(
    Year >= preCWreconstruction_start & Year < reconstruction_start ~ "Pre-Reconstruction",
    Year >= reconstruction_start & Year <= reconstruction_end ~ "Reconstruction",
    Year > reconstruction_end ~ "Post-Reconstruction",
    TRUE ~ NA_character_  # Handles any year outside the specified ranges
    )
  )


############# Examining Form of Data #############

reveal_dataset(Era_B_Elect_State)
reveal_dataset(Era_B_Elect_Year)
reveal_dataset(Era_HistStat)
reveal_dataset(Era_HistStat_OccDispr)


############# Create race specific datasets #############

Era_HistStat_black <- Era_HistStat %>% 
  filter(Race == "Black")
Era_HistStat_white <- Era_HistStat %>% 
  filter(Race == "White")

############# Imputing Missing Values #############

impute_missing_values <- function(time_series_data, time_var, outcome_var) {
  # Replace missing values with NA
  time_series_data[is.na(time_series_data[[outcome_var]]), outcome_var] <- NA
  
  # Impute missing values using spline method
  imputed_data <- na_interpolation(time_series_data[[outcome_var]], option = "spline")
  
  # Create a dataframe with time and imputed values
  imputed_df <- data.frame(Time = time_series_data[[time_var]], Outcome = imputed_data)
  
  return(imputed_df)
}

impute_missing_values_multiple <- function(time_series_data, time_vars, outcome_vars) {
  imputed_dfs <- map2(time_vars, outcome_vars, ~ {
    time_var <- .x
    outcome_var <- .y
    
    # Replace missing values with NA
    time_series_data %>%
      mutate(!!outcome_var := ifelse(is.na(!!sym(outcome_var)), NA, !!sym(outcome_var))) %>%
      transmute(Time = !!sym(time_var), Outcome = na_interpolation(!!sym(outcome_var), option = "spline"))
  })
  
  return(imputed_dfs)
}

# Example usage
time_vars <- rep("Year", 3)
outcome_vars <- c("SchoolEnroll_Percent", "Illiteracy_Percent", "Institutionalized_per10000")

Era_HistStat_black_imp <- impute_missing_values_multiple(Era_HistStat_black, time_vars, outcome_vars)
Era_HistStat_white_imp <- impute_missing_values_multiple(Era_HistStat_white, time_vars, outcome_vars)

# BLACK DATA TO FEED MODEL
SchoolEnroll_black_imp <- Era_HistStat_black_imp[[1]]
names(SchoolEnroll_black_imp) <- c("Year", "Enroll_Percent")
Illiteracy_black_imp <- Era_HistStat_black_imp[[2]]
names(Illiteracy_black_imp) <- c("Year", "Illiteracy_Percent")
Institutional_black_imp <- Era_HistStat_black_imp[[3]]
names(Institutional_black_imp) <- c("Year", "Institutionalized_per10000")
Era_HistStat_black_imp <- full_join(SchoolEnroll_black_imp, Illiteracy_black_imp, "Year")
Era_HistStat_black_imp <- full_join(Era_HistStat_black_imp, Institutional_black_imp, "Year")
# WHITE DATA TO FEED MODEL
SchoolEnroll_white_imp <- Era_HistStat_white_imp[[1]]
names(SchoolEnroll_white_imp) <- c("Year", "Enroll_Percent")
Illiteracy_white_imp <- Era_HistStat_white_imp[[2]]
names(Illiteracy_white_imp) <- c("Year", "Illiteracy_Percent")
Institutional_white_imp <- Era_HistStat_white_imp[[3]]
names(Institutional_white_imp) <- c("Year", "Institutionalized_per10000")
Era_HistStat_white_imp <- full_join(SchoolEnroll_white_imp, Illiteracy_white_imp, "Year")
Era_HistStat_white_imp <- full_join(Era_HistStat_white_imp, Institutional_white_imp, "Year")
# ACTUAL DATA TO COMPARE
Actual_HistStat <- Hist_US_Stat_Main %>% 
  filter(Year > reconstruction_era_end) %>% 
  select(Year, Race, SchoolEnroll_Percent, Illiteracy_Percent, Institutionalized_per10000)
Actual_HistStat_black <- Actual_HistStat %>% 
  filter(Race == "Black") %>% select(-Race)
Actual_HistStat_white <- Actual_HistStat %>% 
  filter(Race == "White") %>% select(-Race)



############# Function for Forecasting Model #############

ts_data <- ts(Era_HistStat_black_imp$Illiteracy_Percent, start = min(Era_HistStat_black_imp$Year), frequency = 10)
# Create a time series object from the outcome data
ts_data <- ts(Era_HistStat_black_imp$outcome, start = min(Era_HistStat_black_imp$year), frequency = 1/(max(Era_HistStat_black_imp$year) - min(Era_HistStat_black_imp$year)) * 10)

# Bayesian structural time series
fit_bsts <- function(data) {
  ss_model <- AddLocalLinearTrend(list(), data)
  ss_model <- bsts(data, state.specification = ss_model, niter = 1000)
  predict(ss_model, horizon = 30, burn = 100)
}

# Example usage
forecast_bsts <- fit_bsts(ts_data)
plot(forecast_bsts)
