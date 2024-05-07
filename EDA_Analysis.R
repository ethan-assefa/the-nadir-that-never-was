# Purpose: Conduct some exploratory data analysis of our key variables, consider how to best apply our data
# Gain further insight through informal visuals and summary tables

# Load needed packages
library(tidyverse) # For general use
library(haven) # For loading in .dta files
library(naniar) # For seeing missingness
library(tidycensus) # For getting built in state-county FIPS data 


# Loads in the R environment containing the final versions of our clean data, as well as functions made
load("AnalysisData_Clean.RData")
# Can also read in CSV files from directory if needed 

reveal_dataset(Reconstruction_ElectVoter)
reveal_dataset(B_Elect_State_Year)
reveal_dataset(B_Elect_Year)
reveal_dataset(Hist_US_Stat_Main)
reveal_dataset(Hist_US_Stat_OccupDispar)


Actual_HistStat <- Hist_US_Stat_Main %>%
  select(Year, Race, SchoolEnroll_Percent, Illiteracy_Percent, Institutionalized_per10000)
Actual_HistStat_black <- Actual_HistStat %>% 
  filter(Race == "Black") %>% select(-Race)
Actual_HistStat_black <- full_join(Actual_HistStat_black, B_Elect_Year, "Year")
Actual_HistStat_white <- Actual_HistStat %>% 
  filter(Race == "White") %>% select(-Race)




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

Actual_HistStat_black_imp <- impute_missing_values_multiple(Actual_HistStat_black, time_vars, outcome_vars)
Actual_HistStat_white_imp <- impute_missing_values_multiple(Actual_HistStat_white, time_vars, outcome_vars)

# BLACK DATA TO FEED MODEL
SchoolEnroll_black_imp <- Actual_HistStat_black_imp[[1]]
names(SchoolEnroll_black_imp) <- c("Year", "Enroll_Percent")
Illiteracy_black_imp <- Actual_HistStat_black_imp[[2]]
names(Illiteracy_black_imp) <- c("Year", "Illiteracy_Percent")
Institutional_black_imp <- Actual_HistStat_black_imp[[3]]
names(Institutional_black_imp) <- c("Year", "Institutionalized_per10000")
Actual_HistStat_black_imp <- full_join(SchoolEnroll_black_imp, Illiteracy_black_imp, "Year")
Actual_HistStat_black_imp <- full_join(Actual_HistStat_black_imp, Institutional_black_imp, "Year")
Actual_HistStat_black_imp <- full_join(Actual_HistStat_black_imp, B_Elect_Year, "Year")

# WHITE DATA TO FEED MODEL
SchoolEnroll_white_imp <- Actual_HistStat_white_imp[[1]]
names(SchoolEnroll_white_imp) <- c("Year", "Enroll_Percent")
Illiteracy_white_imp <- Actual_HistStat_white_imp[[2]]
names(Illiteracy_white_imp) <- c("Year", "Illiteracy_Percent")
Institutional_white_imp <- Actual_HistStat_white_imp[[3]]
names(Institutional_white_imp) <- c("Year", "Institutionalized_per10000")
Actual_HistStat_white_imp <- full_join(SchoolEnroll_white_imp, Illiteracy_white_imp, "Year")
Actual_HistStat_white_imp <- full_join(Actual_HistStat_white_imp, Institutional_white_imp, "Year")


# Create csv files with clean, wrangled data
write.csv(Actual_HistStat_black_imp, "Actual_HistStat_black_imp.csv")
write.csv(Actual_HistStat_white_imp, "Actual_HistStat_white_imp.csv")
