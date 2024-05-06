# Purpose: Bring in qualitative data for analysis, format to proper form, output wrangled data as json

# Load needed packages
library(tidyverse) # For general use
library(haven) # For loading in .dta files
library(naniar) # For seeing missingness

# Create function for displaying data form to easily show in ChatGPT
reveal_dataset <- function(dataset, length=5){
  
  # Gives shape of dataset
  cat(paste0("Shape of dataset: ", dim(dataset)[1], " Rows; ", dim(dataset)[2], " Columns"), sep="\n")
  
  # Seperator
  cat("-----------------------------------------------------------------------------------------------------------", sep="\n")
  
  # Gives column names and descriptions if available
  str(dataset)
  
  # Seperator
  cat("-----------------------------------------------------------------------------------------------------------", sep="\n")
  
  # Gives first 5 rows including column names in csv format
  # Create a text connection object
  output <- textConnection("csv_content", "w")
  # Write the first five rows to the text connection in CSV format
  write.csv(head(dataset, length), output, row.names = FALSE)
  # Close the text connection
  close(output)
  # Print the content
  cat(csv_content, sep = "\n")
}

############################################################# 
###################### QUALITATIVE DATA #####################
############################################################# 

# Import Data for:
# DocSouth Data
# Link: https://docsouth.unc.edu/docsouthdata/




############################################################# 
####################### EXPORTING DATA ######################
############################################################# 

# Create json files with clean, wrangled data

# Showcase clean data for brainstorming analysis to follow
