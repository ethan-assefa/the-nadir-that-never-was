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

# Loads in the R environment containing the final versions of our clean data, as well as functions made
load("AnalysisData_Clean.RData")
# Can also read in CSV files from directory if needed 

############# Time Period ############# 
# Determine time periods of interest era
preCWreconstruction_start <- 1850 # Gives model idea of trends building into the civil war and then reconstruction
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


############# Setting Up Time Series #############

# Create time series object for 
literacy_ts <- ts(literacy_ts$mean_literacy_rate, start = start_year, frequency = 1)
# Decompose
decomposed <- decompose(literacy_ts)
plot(decomposed)