# Purpose: Bring in quantitative data for analysis, format to proper form, output wrangled data as csv

# Load needed packages
library(tidyverse) # For general use
library(haven) # For loading in .dta files
library(naniar) # For seeing missingness
library(tidycensus) # For getting built in state-county FIPS data 

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

  # Provides missingness table of variables
  print(miss_var_summary(dataset), n = ncol(dataset))
  
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
##################### QUANTITATIVE DATA ##################### 
############################################################# 

########
# Import Replication Data for: 
# Stewart, Megan; Kitchens, Karin, 2021, "Replication Data for: Social Transformation and Violence: Evidence from U.S. Reconstruction"
# DOI: https://doi.org/10.7910/DVN/CCHS9C
########

# Set file path to data folder
file_path <- "C:/Users/affes/OneDrive/Documents/UVA SDS Spring 2024/SARC 5400/Final Project/US_Reconstruction_Evidence/"
# Import relevant datasets
reconstruc_diff_data <- read_dta(paste0(file_path, "diff_diff_data_set.dta"))
reconstruc_main_data <- read_dta(paste0(file_path, "main_analysis.dta"))

# NOT NEEDED unless interested in original analysis (STATA file)
#reconstruc_analysis <- readLines("C:/Users/affes/OneDrive/Documents/UVA SDS Spring 2024/SARC 5400/Final Project/US_Reconstruction_Evidence/main_analysis_models.do")

########
# Import Data for:
# Historical Statistics of the United States
# Link: https://hsus.cambridge.org/HSUSWeb/HSUSEntryServlet
########

# Set file path to data folder
file_path2 <- "C:/Users/affes/OneDrive/Documents/UVA SDS Spring 2024/SARC 5400/Final Project/Historic_Statistics_US/"
# Import relevant datasets
Race_ChildArrangement <- read_csv(paste0(file_path2, "Race_ChildArrangement_Ae128-190.csv"))

Race_FemaleWorkforce <- read_csv(paste0(file_path2, "Race_FemaleWorkforce_Ba40-49.csv"))
Race_MaleWorkforce <- read_csv(paste0(file_path2, "Race_MaleWorkforce_Ba1-10.csv"))

Race_FemaleWorkPartic <- read_csv(paste0(file_path2, "Race_FemaleWorkPartic_Ba50-63.csv"))
Race_MaleWorkPartic <- read_csv(paste0(file_path2, "Race_MaleWorkPartic_Ba11-24.csv"))

Race_FertilMortil <- read_csv(paste0(file_path2, "Race_FertilMortil_Ab1-10.csv"))

Race_Illiteracy <- read_csv(paste0(file_path2, "Race_Illiteracy_Bc793-797.csv"))

Race_Institutionalized <- read_csv(paste0(file_path2, "Race_Institutionalized_Ae97-127.csv"))

Race_LaborPartic1850_srn <- read_csv(paste0(file_path2, "Race_LaborPartic_Ad752-759.csv")) # Labor rate; sex,race,nativity 1850–1997
# Missing much more data, more granular than needed so will go with srn version
#Race_LaborPatic1801_srca <- read_csv(paste0(file_path2, "Race_LaborPatic_Af15-42.csv")) # Labor rate; sex,race,cohort,age 1801–1980

Race_OccupSegregation <- read_csv(paste0(file_path2, "Race_OccupSegregation_Ba4207-4213.csv"))

Race_Pop <- read_csv(paste0(file_path2, "Race_Pop_Aa145-184.csv"))

Race_SchoolEnroll <- read_csv(paste0(file_path2, "Race_SchoolEnroll_Bc438-446.csv"))

########
# Replication Data for:
# Logan, Trevon; 2019, "Do Black Politicians Matter? Evidence from Reconstruction."
# DOI: https://doi.org/10.3886/E115861V1
########

# Set file path to data folder
file_path3 <- "C:/Users/affes/OneDrive/Documents/UVA SDS Spring 2024/SARC 5400/Final Project/do_black_politicans_matter_Evidence/"
# Import relevant datasets
black_politicians <- read_csv(paste0(file_path3, "Officials_Raw_JEH_12-30-19.csv"))
voter1867_registration <- read_csv(paste0(file_path3, "voter_registration1867_69.csv"), col_select = -1)

############################################################# 
####################### CLEANING DATA #######################
############################################################# 

# Determining state id values
# Bring in state code mapping
state_map <- read_delim(paste0(file_path, "State_Key.txt"), delim = ";", comment = "#")
# Use built in FIPS dataset from tidycensus package and link to get more info
data(fips_codes)
# Sets a dataset for county level fips codes
fips_county <- fips_codes %>% 
  rename(State_Abbr = state, State_Fips = state_code, State_Name = state_name, 
         County_Fips = county_code, County_Name = county) %>% 
  # Drop all county names to lowercase for easier matching later
  mutate(County_Name = str_to_lower(County_Name)) %>% 
  # Remove all parts of string containing "county" for easier matching later
  mutate(County_Name = str_remove(County_Name, " county")) %>% 
  # Remove all parts of string containing "parish" for easier matching later
  mutate(County_Name = str_remove(County_Name, " parish")) %>% 
  # Sets "st." to "st" to match county names with voter1867_registration  
  mutate(County_Name = str_replace(County_Name, "st. ", "st "))

# Remove old fips codes dataset
rm(fips_codes)
# Join the state_map data to the county level fips
fips_county <- left_join(x = fips_county,
                         y = state_map %>% select(State_Code, State_Abbr),
                         by = "State_Abbr")
# Sets a dataset for state level fips codes
fips_state <- fips_county %>% 
  select(starts_with("state")) %>% 
  distinct() 
# Reset index
row.names(fips_state) <- NULL
# Drop US territories except DC
fips_state <- fips_state[-c(52:57),]

########
# Clean Replication Data for: 
# Stewart, Megan; Kitchens, Karin, 2021, "Replication Data for: Social Transformation and Violence: Evidence from U.S. Reconstruction"
# DOI: https://doi.org/10.7910/DVN/CCHS9C
########

# Overview of missingness
#vis_miss(reconstruc_diff_data %>% filter(lsc1870==1))

# Join state mapping to diff data
reconstruc_diff_data_clean <- right_join(x = fips_state,
                                         y = reconstruc_diff_data %>% filter(lsc1870==1) %>% select(-lsc1870),
                                         by = join_by(State_Code == stateid))

# Transform variables as needed
reconstruc_diff_data_clean <- reconstruc_diff_data_clean %>% 
  mutate(totpop=round(exp(log_totpop_)))

########
# Clean Data for:
# Historical Statistics of the United States
# Link: https://hsus.cambridge.org/HSUSWeb/HSUSEntryServlet
########

#### Race_ChildArrangement ####
# Clean column names; add race to all and remove unneeded end code 
names(Race_ChildArrangement)[15:22] <- paste0("AllRaces_", names(Race_ChildArrangement)[15:22])
names(Race_ChildArrangement)[36:43] <- paste0("White_", names(Race_ChildArrangement)[36:43])
names(Race_ChildArrangement)[57:64] <- paste0("Black_", names(Race_ChildArrangement)[57:64])
# Remove suffix "_AeXXX_Number"
names(Race_ChildArrangement) <- sub("_Ae[0-9]+_Number$", "", names(Race_ChildArrangement))
# Turn from wide to long format for race groups
Race_ChildArrangement_clean <- pivot_longer(Race_ChildArrangement, 
                                            cols = starts_with("AllRaces_") | starts_with("White_") | starts_with("Black_"), 
                                            names_to = c("Race", "LivingSituation"),
                                            names_pattern = "^(AllRaces|White|Black)_(.*)$",
                                            values_to = "Number") %>%
  pivot_wider(names_from = LivingSituation, values_from = Number) %>%
  relocate(Race, .after = Year)
# Add back "count" to relevant column names for clarity
names(Race_ChildArrangement_clean)[3:23] <- paste0(names(Race_ChildArrangement_clean)[3:23], "_Count")

#### Race_Workforce ####
# Remove suffix "_BaXX_Number"
names(Race_MaleWorkforce) <- sub("_Ba[0-9]+_Number$", "", names(Race_MaleWorkforce))
names(Race_FemaleWorkforce) <- sub("_Ba[0-9]+_Number$", "", names(Race_FemaleWorkforce))
# Add sex column
Race_MaleWorkforce$Sex <- "M" 
Race_FemaleWorkforce$Sex <- "F"
# Combine both to single dataframe
Race_Workforce <- rbind(Race_MaleWorkforce, Race_FemaleWorkforce)
# Turn from wide to long format for race groups
Race_Workforce_clean <- pivot_longer(Race_Workforce, 
                                            cols = starts_with("AllRaces_") | starts_with("White_") | starts_with("Nonwhite_Total_") | starts_with("Nonwhite_Free_") | starts_with("Nonwhite_Slave_"), 
                                            names_to = c("Race", "Workforce_Count"),
                                            names_pattern = "^(AllRaces|White|Nonwhite_Total|Nonwhite_Free|Nonwhite_Slave)_(.*)$",
                                            values_to = "Number") %>%
  pivot_wider(names_from = Workforce_Count, values_from = Number) %>%
  relocate(Race, Sex, .after = Year)
# Add back "count" to relevant column names for clarity
names(Race_Workforce_clean)[4:5] <- paste0(names(Race_Workforce_clean)[4:5], "_Count")

#### Race_WorkPartic ####
# Remove suffix "_BaXX_Number"
names(Race_MaleWorkPartic) <- sub("_Ba[0-9]+_Percent$", "", names(Race_MaleWorkPartic))
names(Race_FemaleWorkPartic) <- sub("_Ba[0-9]+_Percent$", "", names(Race_FemaleWorkPartic))
# Add sex column
Race_MaleWorkPartic$Sex <- "M" 
Race_FemaleWorkPartic$Sex <- "F"
# Combine both to single dataframe
Race_WorkPartic <- rbind(Race_MaleWorkPartic, Race_FemaleWorkPartic)
# Turn from wide to long format for race groups
Race_WorkPartic_clean <- pivot_longer(Race_WorkPartic, 
                                      cols = starts_with("AllRaces_") | starts_with("White_") | starts_with("Nonwhite_Total_") | starts_with("Nonwhite_Free_") | starts_with("Nonwhite_Slave_"), 
                                      names_to = c("Race", "WorkPartic_Percent"),
                                      names_pattern = "^(AllRaces|White|Nonwhite_Total|Nonwhite_Free|Nonwhite_Slave)_(.*)$",
                                      values_to = "Percent") %>%
  pivot_wider(names_from = WorkPartic_Percent, values_from = Percent) %>%
  relocate(Race, Sex, .after = Year)
# Add back "percent" to relevant column names for clarity
names(Race_WorkPartic_clean)[4:6] <- paste0(names(Race_WorkPartic_clean)[4:6], "_Percent")

#### Race_FertilMortil ####
# Remove suffix "_AbXX"
names(Race_FertilMortil) <- sub("_Ab[0-9]+", "", names(Race_FertilMortil))
# Reshape the data to long format
Race_FertilMortil_clean <- Race_FertilMortil %>%
  pivot_longer(cols = -Year,  # Exclude 'Year' from the reshaping process
               names_to = c("Variable", "Race", ".value"),
               names_pattern = "^([^_]+)_(.+)_([^_]+)$",
               values_to = "Prop_Per1,000")

#### Race_Illiteracy ####
# Remove suffix "_BcXX"
names(Race_Illiteracy) <- sub("_Bc[0-9]+", "", names(Race_Illiteracy))
# Reshape the data to long format
Race_Illiteracy_clean <- Race_Illiteracy %>%
  pivot_longer(
    cols = -Year, # Exclude the Year column from the transformation
    names_to = "Race_Nativity", # Name of the new column for race or nativity descriptor
    values_to = "Percent" # Name of the new column for the percentage values
  )

#### Race_Institutionalized ####
# Remove suffix "_AeXX"
names(Race_Institutionalized) <- sub("_Ae[0-9]+", "", names(Race_Institutionalized))
# Select columns of interest and reshape the data to long format
Race_Institutionalized_clean <- Race_Institutionalized %>% 
  select(Year, PopulationInInstitutions_Total_Number, PopulationInInstitutions_CorrectionalInstitutions_Number, 
         PopulationInInstitutions_MentalInstitutions_Number, PopulationInInstitutions_Male_Total_Number,
         PopulationInInstitutions_Male_White_Number, PopulationInInstitutions_Male_Black_Number,
         PopulationInInstitutions_Female_Total_Number, PopulationInInstitutions_Female_White_Number, 
         PopulationInInstitutions_Female_Black_Number) %>%
  pivot_longer(cols = -Year, # Exclude the Year column from the transformation
               names_to = "original_name", # Temporarily store original column names
               values_to = "Count") %>% # Store counts in 'Count'
  mutate(
    # Extract the "Sex" information based on "Male" or "Female" in the column names
    Sex = case_when(str_detect(original_name, "_Male") ~ "Male", str_detect(original_name, "_Female") ~ "Female", TRUE ~ NA_character_),
    # Extract "Race" based on "White" or "Black" in the column names
    Race = case_when(str_detect(original_name, "White") ~ "White", str_detect(original_name, "Black") ~ "Black", TRUE ~ NA_character_),
    # Define Variable, removing parts not needed and ensuring correct labels
    Variable = str_replace(original_name, "_Number", "") %>% str_replace_all("PopulationInInstitutions_", "") %>% str_replace_all("_Male|_Female|_White|_Black", ""),
    # Ensure that total descriptions are uniform
    Variable = if_else(str_detect(original_name, "Total"), "Total", Variable)) %>%
  # Further refining the Variable column to ensure totals are correctly labeled
  mutate(Variable = if_else(str_detect(Variable, "Male") | str_detect(Variable, "Female"), "Total", Variable)) %>%
  select(Year, Variable, Sex, Race, Count)  # Reorder and select necessary columns

#### Race_LaborPartic ####
# Remove suffix "_AdXX"
names(Race_LaborPartic1850_srn) <- sub("_Ad[0-9]+", "", names(Race_LaborPartic1850_srn))
# Reshape the data to long format
Race_LaborParticSRN_clean <- Race_LaborPartic1850_srn %>% 
  pivot_longer(cols = -Year,  # Keep the Year column fixed
               names_to = "original_name",  # Store original column names
               values_to = "LaborPartic_Percent") %>%  # Store the values in 'LaborPartic_Percent'
  mutate(
    # Extract 'Nativity' based on the presence of 'Native-born' or 'Foreign-born' keywords
    Nativity = case_when(str_detect(original_name, "Native-born") ~ "Native-born", str_detect(original_name, "Foreign-born") ~ "Foreign-born", TRUE ~ NA_character_),
    # Extract 'Sex' based on the presence of 'Male' or 'Female' in the column names
    Sex = case_when(str_detect(original_name, "Male") ~ "Male", str_detect(original_name, "Female") ~ "Female", TRUE ~ NA_character_),
    # Extract 'Race' from the remaining part of the column names
    Race = str_extract(original_name, "(White|Black|OtherRace)"),
    # Simplify the 'Race' to be more readable
    Race = if_else(Race == "OtherRace", "Other", Race),
    # Remove all extracted parts from the original name for cleanliness, not necessary in this specific transformation but good for checking
    original_name = str_remove(original_name, "_Percent"),
    original_name = str_remove(original_name, "Male_|Female_"),
    original_name = str_remove(original_name, "Native-born_|Foreign-born_"),
    original_name = str_remove(original_name, "_White|_Black|_OtherRace")) %>%
  select(Year, Nativity, Sex, Race, LaborPartic_Percent)

#### Race_OccupSegregation ####
# Remove suffix "_BaXXX"
names(Race_OccupSegregation) <- sub("_Ba[0-9]+", "", names(Race_OccupSegregation))

#### Race_Pop ####
# Remove suffix "_AaXXX"
names(Race_Pop) <- sub("_Aa[0-9]+", "", names(Race_Pop))
# Reshape the data to long format
Race_Pop_clean <- Race_Pop %>%
  # Remove all columns not looking at black/white pop
  select(!contains("OtherRaces")) %>% 
  pivot_longer(cols = -Year,  # Keep the Year column fixed
               names_to = "original_name",  # Store original column names
               values_to = "Pop_Count") %>%  # Store the values in 'RacePop_Count'
  mutate(
    # Extract 'Sex' based on the presence of 'Male' or 'Female' in the column names
    Sex = case_when(str_detect(original_name, "BothSexes") ~ "Both", str_detect(original_name, "Male") ~ "Male", str_detect(original_name, "Female") ~ "Female", TRUE ~ NA_character_),
    # Extract 'Race' from the remaining part of the column names
    Race = str_extract(original_name, "(AllRaces|White|Black_Total|Black_Slave|Black_FreeColored)"),
    # Simplify 'Race' to be more readable
    Race = if_else(Race == "AllRaces", "All", Race),
    Race = if_else(Race == "Black_Total", "Black Total", Race),
    Race = if_else(Race == "Black_Slave", "Black Slave", Race),
    Race = if_else(Race == "Black_FreeColored", "Black Free", Race),
    # Remove all extracted parts from the original name for cleanliness, not necessary in this specific transformation but good for checking
    original_name = str_remove(original_name, "_Number"),
    original_name = str_remove(original_name, "BothSexes_|Male_|Female_"),
    original_name = str_remove(original_name, "_AllRaces|_White|_Black_Total|_Black_Slave|_Black_FreeColored")) %>%
  select(Year, Sex, Race, Pop_Count)

#### Race_SchoolEnroll ####
# Remove suffix "_BcXXX"
names(Race_SchoolEnroll) <- sub("_Bc[0-9]+", "", names(Race_SchoolEnroll))
# Reshape the data to long format
Race_SchoolEnroll_clean <- Race_SchoolEnroll %>%
  # Only interested in reconstruction/jim crow era
  filter(Year < 1940) %>% 
  pivot_longer(cols = -Year, 
               names_to = c("Sex", "Race"), 
               values_to = "Enroll_Percent",
               names_pattern = "^(BothSexes|Males|Females)_(Total|White|Nonwhite)_") %>%
  # Simplify variable groups to be more readable
  mutate(Sex = ifelse(Sex == "BothSexes", "Both", Sex),
         Race = ifelse(Race == "Nonwhite", "Non-White", Race)) %>% 
  relocate(Sex, Race, .after = Year)

#### Combining all national-level historical statistics data ####
# Add dataset name to start of the value variables for clarity when joining
names(Race_Workforce_clean)[4:5] <- paste0("Workforce_", names(Race_Workforce_clean)[4:5])
names(Race_WorkPartic_clean)[4:6] <- paste0("WorkParticRate_", names(Race_WorkPartic_clean)[4:6])
# NOTE: redefining non-white to be mainly black [based on documentation], even though there is some other
Race_Workforce_clean <- Race_Workforce_clean %>% 
  filter(Race != "Nonwhite_Free" & Race != "Nonwhite_Slave") %>% 
  mutate(Race = ifelse(Race == "Nonwhite_Total", "Black", Race))
Race_WorkPartic_clean <- Race_WorkPartic_clean %>% 
  filter(Race != "Nonwhite_Free" & Race != "Nonwhite_Slave") %>% 
  mutate(Race = ifelse(Race == "Nonwhite_Total", "Black", Race))

# Join the work partic and workforce dataframes
Race_Workforce_WorkPartic <- full_join(x = Race_Workforce_clean,
                                       y = Race_WorkPartic_clean,
                                       by = c("Year", "Race", "Sex"))

# Convert joined dataframe to better match Labor partic dataset
Race_Workforce_WorkPartic <- Race_Workforce_WorkPartic %>% 
  mutate(Sex = ifelse(Sex == "F", "Female", "Male")) # M/F binary w/ no missing data, so code shouldn't cause problems
  
# Add dataset name to start of enroll variable for clarity when joining
names(Race_SchoolEnroll_clean)[4] <- paste0("School", names(Race_SchoolEnroll_clean)[4])
# Transform Race Pop data to better match SchoolEnroll data
Race_Pop_clean <- Race_Pop_clean %>% 
  mutate(Race = ifelse(Race == "All", "Total", Race)) %>% 
  filter(Race != "Black Slave" & Race != "Black Free")
# Transform SchoolEnroll data to better match Race Pop data 
# NOTE: redefining non-white to be mainly black [based on documentation], even though there is some other
Race_SchoolEnroll_clean <- Race_SchoolEnroll_clean %>% 
  mutate(Sex = ifelse(Sex == "Males", "Male", Sex)) %>% 
  mutate(Sex = ifelse(Sex == "Females", "Female", Sex)) %>% 
  mutate(Race = ifelse(Race == "Non-White", "Black Total", Race))

# Join the school enroll and race pop dataframes
Race_PopSchoolEnroll <- full_join(x = Race_Pop_clean,
                                  y = Race_SchoolEnroll_clean,
                                  by = c("Year", "Race", "Sex"))

# Transform illiteracy data to better join to enroll/pop data
Race_Illiteracy_clean <- Race_Illiteracy_clean %>% 
  filter(Race_Nativity != "White_Native-born_Percent" & Race_Nativity != "White_Foreign-born_Percent") %>% 
  mutate(Race_Nativity = str_remove(Race_Nativity, "_Percent")) %>% 
  separate(Race_Nativity, into = c("Race", "Nativity"), sep = "_", extra = "merge", fill = "right") %>% 
  mutate(Sex = "Both", .before = Race) %>% 
  mutate(Race = ifelse(Race == "AllPersons", "Total", Race)) %>% 
  mutate(Race = ifelse(Race == "BlackAndOtherRaces", "Black Total", Race)) %>% 
  select(-Nativity)
# Add dataset name to start of illiterate variable for clarity when joining
names(Race_Illiteracy_clean)[4] <- paste0("Illiteracy_", names(Race_Illiteracy_clean)[4])

# Join the illiteracy and the enroll/pop dataframes
Race_PopSchoolEnrollIlliteracy <- full_join(x = Race_PopSchoolEnroll,
                                            y = Race_Illiteracy_clean,
                                            by = c("Year", "Race", "Sex"))

# Transform institutionalized data to better join to enroll/pop/illiteracy data
Race_Institutionalized_clean <- Race_Institutionalized_clean %>% 
  filter(Variable == "Total") %>% 
  select(-Variable) %>% 
  # Remove Sex variable and just consider totals across gender
  group_by(Year, Race) %>% 
  summarise(Institutionalized_Counts = sum(Count)) %>% 
  drop_na(Race) %>% 
  mutate(Sex = "Both", Race = ifelse(Race == "Black", "Black Total", Race), .after = Year)
  
# Join the instiutionalized and the enroll/pop/illiteracy dataframes
Race_PopSchoolEnrollIlliteracyInstit <- full_join(x = Race_PopSchoolEnrollIlliteracy,
                                            y = Race_Institutionalized_clean,
                                            by = c("Year", "Race", "Sex"))

# Start to congregate data; remove granularity by aggregating across gender (remove some of the missingness)
Race_PopSchoolEnrollIlliteracyInstit_Final <- Race_PopSchoolEnrollIlliteracyInstit %>% 
  filter(Sex == "Both") %>% 
  mutate(Race = ifelse(Race == "Black Total", "Black", Race),
         Institutionalized_per10000 = (Institutionalized_Counts / Pop_Count)*10000)

# Consider only counts from the Race_Workforce_WorkPartic dataset, aggregate across all genders 
Race_Workforce_WorkPartic <- Race_Workforce_WorkPartic %>% 
  select(!contains("Percent")) %>% 
  mutate(Race = ifelse(Race == "AllRaces", "Total", Race)) %>% 
  group_by(Year, Race) %>% 
  summarise(`Workforce_Age10-15_Count` = sum(`Workforce_Age10-15_Count`),
            `Workforce_Age16AndOlder_Count` = sum(`Workforce_Age16AndOlder_Count`)) %>% 
  rowwise() %>% 
  mutate(`Workforce_10+_Count` = sum(`Workforce_Age10-15_Count`, `Workforce_Age16AndOlder_Count`)) %>% 
  ungroup() %>%
  select(-c(`Workforce_Age10-15_Count`, `Workforce_Age16AndOlder_Count`))

# Join the Workforce and the enroll/pop/illiteracy/instiutionalized dataframes
Race_PopSchoolEnrollIlliteracyInstitWorkforce <- full_join(x = Race_PopSchoolEnrollIlliteracyInstit_Final %>% select(-Sex),
                                                  y = Race_Workforce_WorkPartic,
                                                  by = c("Year", "Race"))

# Clean Race_ChildArrangement_clean to create big groupings
Race_ChildArrangement_Final <- Race_ChildArrangement_clean %>% 
  rowwise() %>% 
  mutate(Race = ifelse(Race == "AllRaces", "Total", Race),
         `BothParentsPresent0-17_Sum` = sum(across(starts_with("BothParentsPresent")), na.rm = T),
         `MotherSolely0-17_Sum` = sum(across(starts_with("MotherOnlyPresent")), na.rm = T),
         `FatherSolely0-17_Sum` = sum(across(starts_with("FatherOnlyPresent")), na.rm = T),
         `BothParentsAbsent0-17_Sum` = sum(across(starts_with("BothParentsAbsent")), na.rm = T),
         `BothParentsPresent0-17_Prop` = `BothParentsPresent0-17_Sum` / Total_Count,
         `MotherSolely0-17_Prop` = `MotherSolely0-17_Sum` / Total_Count,
         `FatherSolely0-17_Prop` = `FatherSolely0-17_Sum` / Total_Count,
         `BothParentsAbsent0-17_Prop` = `BothParentsAbsent0-17_Sum` / Total_Count) %>% 
  ungroup() %>% 
  select(!ends_with("Count")) %>% 
  select(!ends_with("Sum"))

# Join the Child arrangement and the enroll/pop/illiteracy/instiutionalized/workforce dataframes
Race_PopSchEnrlIlitInstWrkChld <- full_join(x = Race_PopSchoolEnrollIlliteracyInstitWorkforce,
                                                           y = Race_ChildArrangement_Final,
                                                           by = c("Year", "Race"))

#### FINAL DATASETS USED #### 

# Race_PopSchEnrlIlitInstWrkChld
# Contains data from: Race Population, School Enrollment, Illiteracy Rates, Institutionalization, Workforce, Child Arrangement
# Shrunk to 1830 - 1940 time range to better match with other data
# Dropped Labor participation [redundant], Work participation [redundant], fertility and mortality [too much missingness]

#Race_OccupSegregation
# Contains data from: occupation/segregation
# Shrunk to 1830 - 1940 time range to better match with other data
# Kept separate from long form data due to different comparison being made

########
# Clean Data for:
# Logan, Trevon; 2019, "Do Black Politicians Matter? Evidence from Reconstruction."
# DOI: https://doi.org/10.3886/E115861V1
########

# General cleaning
voter1867_clean <- voter1867_registration %>%
  # Gives appropriate names to columns
  rename(County=name, State=state_terr, BlackRegVoter1867_Count = black_reg, WhiteRegVoter1867_Count = white_reg) %>% 
  # Makes county names lowercase for easier matching
  mutate(County = str_to_lower(County))

#### fips county - voter reg data inconsistencies; changed voter reg data to match #### 
# AL: dekalb - de kalb, NA - sanford
voter1867_clean$County[voter1867_clean$County == "de kalb"] <- "dekalb"
# MI: NA - de soto
# TX: NA - davis, dewitt - de witt
voter1867_clean$County[voter1867_clean$County == "de witt"] <- "dewitt"
# VA: alexandria city - alexandria, NA - elizabeth city, NA - nansemond, norfolk city - norfolk, NA - princess anne, NA - warwick
voter1867_clean$County[voter1867_clean$County == "alexandria"] <- "alexandria city"
voter1867_clean$County[voter1867_clean$County == "norfolk"] <- "norfolk city"
# GA: NA - campbell, NA - milton
#### All other inconsistencies fixed using following: #### 
# Remove parish from fips county data (to make LA counties match)
# Removed "." after "st" to fips county data (to make all with st in county match)
 
# First join to just state for state code/fips etc. to get all with no missing
voter1867_clean <- right_join(x = fips_state, 
                                   y = voter1867_clean, 
                                   by = join_by(State_Name == State))
# Then join to state-county mapping to link more easily to other data
voter1867_county <- right_join(x = fips_county %>% select(County_Name, County_Fips, State_Name), 
                              y = voter1867_clean, 
                              by = join_by(County_Name == County, State_Name == State_Name))
# Create black-white_reg ratio variable at county level
voter1867_county <- voter1867_county %>% 
  mutate(B_W_RegVoter1867_Ratio = BlackRegVoter1867_Count / WhiteRegVoter1867_Count)

# Aggregate to state level from voter reg. data at county-level 
voter1867_state <- voter1867_county %>% 
  group_by(State_Name, State_Abbr, State_Fips, State_Code) %>% 
  summarise(BlackRegVoter1867_Count = sum(BlackRegVoter1867_Count, na.rm = T), 
            WhiteRegVoter1867_Count = sum(WhiteRegVoter1867_Count, na.rm = T)) %>% 
  # Create black-white_reg ratio variable at state level
  mutate(B_W_RegVoter1867_Ratio = BlackRegVoter1867_Count / WhiteRegVoter1867_Count)

# Consider missingness of variables
vis_miss(black_politicians) 
# Note: generally office 2/3 and related data are 90%+ missing or not applicable, will remove and consider only 1st office

# General cleaning
black_elect_county <- black_politicians %>% 
  # Remove all unrelated or heavily missing variables
  select(-c("Birth Year", "Death Year", "State of Office 2", "County of Office 2", "Occupation 2", 
            "Occupation 3", "Office 2", "Office 3")) %>% 
  # Give readable column titles
  rename(Politician_ID = ID, Office_Start = `Entered Office`, Office_End = `Left Office`, Last = `Last Name`, 
         First = `First Name`, Office_State = `State of Office`, Office_County = `County of Office`, 
         Victim = `Victim of Violence`, Own_Property = `Property Ownership ($100)`, Born_Slave = `Born Slave`, 
         Occupation = `Occupation 1`, Office_Title = `Office 1`) %>% 
  # Turn all binary into proper 0/1 coding
  mutate(Literate = ifelse(is.na(Literate), NA, ifelse(Literate == "Y", 1, 0)),
         Victim = ifelse(is.na(Victim), NA, ifelse(Victim == "Y", 1, 0)),
         Own_Property = ifelse(is.na(Own_Property), NA, ifelse(Own_Property == "Y", 1, 0)),
         Born_Slave = ifelse(is.na(Born_Slave), NA, ifelse(Born_Slave == "Y", 1, 0))) %>% 
  # Makes county names lowercase for easier matching
  mutate(Office_County = str_to_lower(Office_County)) %>% 
  # Fixes differences in county formatting to allow matching to county_fips and voter reg data
  mutate(Office_County = str_replace(Office_County, "e. ", "east ")) %>% 
  mutate(Office_County = str_replace(Office_County, "w. ", "west ")) %>%
  mutate(Office_County = str_replace(Office_County, "st. ", "st "))
# First join to just state for state code/fips etc. to get all with no missing
black_elect_county <- right_join(x = fips_state, 
                              y = black_elect_county, 
                              by = join_by(State_Abbr == Office_State))
# Then join to state-county mapping to link more easily to other data
black_elect_county <- right_join(x = fips_county %>% select(County_Name, County_Fips, State_Name), 
                              y = black_elect_county, 
                              by = join_by(County_Name == Office_County, State_Name == State_Name))

# Gets earliest and latest range of elected years
first_yr <- min(black_elect_county$Office_Start, na.rm = T)
last_yr <- max(black_elect_county$Office_End, na.rm = T)

# Create time freq series of black elected officials
black_elect_years <- black_elect_county %>%
  # Generate a list column of all years from start_year to end_year for each official
  mutate(Year = map2(first_yr, last_yr, seq, by = 1)) %>%
  # Transform the list into separate rows
  unnest(Year) %>%
  # Ensure that the end year is not included
  filter(Year >= Office_Start & Year <= Office_End)

# Count the number of officials in office each year divided by state
black_elect_state_years <- black_elect_years %>%
  count(State_Name, State_Abbr, State_Fips, State_Code, Year)

# Count the number of officials in office each year overall
black_elect_years <- black_elect_years %>%
  count(Year)

# Create state level black politician counts
black_elect_state <- black_elect_county %>% 
  group_by(State_Name, State_Abbr, State_Fips, State_Code) %>% 
  summarise(Black_Elect_Count = n(),
            Black_Elect_Yr_Range_Start = min(Office_Start, na.rm = T),
            Black_Elect_Yr_Range_End = max(Office_End, na.rm = T),
            Black_Elect_Prop_Literate = round(sum(Literate, na.rm = T)/n(), 3),
            Black_Elect_Prop_Victim = round(sum(Victim, na.rm = T)/n(), 3),
            Black_Elect_Prop_Born_Slave = round(sum(Born_Slave, na.rm = T)/n(), 3),
            Black_Elect_Prop_Own_Property = round(sum(Own_Property, na.rm = T)/n(), 3))

# Join black election state level data to voter registration state level data
black_elect_voter1867_state <- left_join(x = black_elect_state %>% filter(State_Abbr != "MA"),
                                         y = voter1867_state,
                                         by = c("State_Name", "State_Abbr", "State_Fips", "State_Code"))

# Add "State_" to start of every column in black_elect_voter1867_state for clarity that it is state-level data
names(black_elect_voter1867_state)[5:14] <- paste0("State_", names(black_elect_voter1867_state)[5:14])

# Join black politician/voter state level data to reconstruction county level data (repeated values for all counties in given state)
BlkElect_Votr67_Recon <- left_join(x = reconstruc_diff_data_clean,
                                   y = black_elect_voter1867_state, # Have to remove DC to not cause issue with join
                                   by = c("State_Code" = "State_Code", "State_Abbr" = "State_Abbr", 
                                          "State_Name" = "State_Name", "State_Fips" = "State_Fips"))

# Add a county group variable (no county names, but gives groupings for the before/after intervention)
BlkElect_Votr67_Recon$County_Code <- (seq_len(nrow(BlkElect_Votr67_Recon)) - 1) %/% 2 + 1
# Order column appropriately
BlkElect_Votr67_Recon <- BlkElect_Votr67_Recon %>% relocate(County_Code, .after = State_Code)
# Create county level black politician descriptions, can use for map data; link to state-county geo data


############################################################# 
####################### EXPORTING DATA ######################
############################################################# 

# Get the values for the total rows (last minute cleaning)
Wrkfrce_ttls <- Race_PopSchEnrlIlitInstWrkChld$`Workforce_10+_Count`[Race_PopSchEnrlIlitInstWrkChld$`Race`=="Total"]
Wrkfrce_ttls <- rep(Wrkfrce_ttls, each = 3)

# Hist_US_Stat_Main (formerly Race_PopSchEnrlIlitInstWrkChld)
# Contains data from: Race Population, School Enrollment, Illiteracy Rates, Institutionalization, Workforce, Child Arrangement
# Shrunk to 1850 - 1940 time range to better match with other data
# Dropped Labor participation [redundant], Work participation [redundant], fertility and mortality [too much missingness]
Hist_US_Stat_Main <- Race_PopSchEnrlIlitInstWrkChld %>%
  mutate(`Workforce_10+_Totals_Count` = Wrkfrce_ttls, .after = `Workforce_10+_Count`) %>% 
  #filter(Year >= 1830 & Year < 1950) %>% 
  select(-c(Institutionalized_Counts)) %>% 
  rowwise() %>% 
  mutate(`Workforce_10+_Prop` = `Workforce_10+_Count`/`Workforce_10+_Totals_Count`, .after = `Workforce_10+_Totals_Count`) %>% 
  ungroup() %>% 
  select(-c(`Workforce_10+_Count`, `Workforce_10+_Totals_Count`))

# Hist_US_Stat_OccupDispar (formerly Race_OccupSegregation)
# Contains data from: occupation/segregation
# Shrunk to 1850 - 1940 time range to better match with other data
# Kept separate from long form data due to different comparison being made
Hist_US_Stat_OccupDispar <- Race_OccupSegregation %>% 
  select(Year, WhiteMenAndNonwhiteMenCompared_Index, WhiteWomenAndNonwhiteWomenCompared_Index) %>% 
  rename(WhiteM_NonwhiteM_Index = WhiteMenAndNonwhiteMenCompared_Index, 
         WhiteF_NonwhiteF_Index = WhiteWomenAndNonwhiteWomenCompared_Index) #%>% 
  #filter(Year >= 1830 & Year < 1950)

# B_Elect_State_Year (formerly black_elect_state_years)
# Contains data from Black politicians
# Stratified by states and years
# Start at 1845 time range to better match with other data
B_Elect_State_Year <- black_elect_state_years %>% 
  rename(Black_Elect_Count = n) #%>% 
  #filter(Year >= 1830 & Year < 1950)

# B_Elect_Year (formerly black_elect_years)
# Contains data from Black politicians
# Stratified only by years
# Start at 1845 time range to better match with other data
B_Elect_Year <- black_elect_years %>% 
  rename(Black_Elect_Count = n) #%>% 
  #filter(Year >= 1830 & Year < 1950)

# Reconstruction_ElectVoter (formerly BlkElect_Votr67_Recon)
# Contains data from Black politicians, registered voters, reconstruction info
# County-level data, though only identifiable at the state level;  Black politicians/registered voters data is at state-level
Reconstruction_ElectVoter <- BlkElect_Votr67_Recon

# Create csv files with clean, wrangled data
write.csv(Hist_US_Stat_Main, "Hist_US_Stat_Main.csv")
write.csv(Hist_US_Stat_OccupDispar, "Hist_US_Stat_OccupDispar.csv")
write.csv(B_Elect_State_Year, "B_Elect_State_Year.csv")
write.csv(B_Elect_Year, "B_Elect_Year.csv")
write.csv(Reconstruction_ElectVoter, "Reconstruction_ElectVoter.csv")

# Create R environment file (.Rdata) of key datasets to easily load work done in script
save(reveal_dataset, Hist_US_Stat_Main, Hist_US_Stat_OccupDispar, B_Elect_State_Year, B_Elect_Year, Reconstruction_ElectVoter, file = "AnalysisData_Clean.RData")

# Showcase clean data for brainstorming analysis to follow
