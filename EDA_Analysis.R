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


