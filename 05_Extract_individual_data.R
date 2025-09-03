# Step 17 Create data frame INDIVIDUAL_DATA_RP:

# This data frame is created to store for only the RPs the data from the INDIVIDUAL table: 
# SEX, BIRTH_DATE, DEATH_DATE, BIRTH_LOCATION, OBSERVATION, START_OBSERVATION and END_OBSERVATION

# Load packages

library(dplyr)
library(tidyr)
library(data.table)

# First the relevant data is subsetted out of the INDIVIDUAL data frame.
# Then the data frame is inner joined with the RP data frame so that there is only data about the RPs stored.   
# The above mentioned data frame are created using the INDIVIDUAL_DATA_RP data frame.

INDIVIDUAL_RP <- INDIVIDUAL %>% 
  subset (Type == "SEX" | Type == "BIRTH_DATE" | Type == "DEATH_DATE" | Type == "BIRTH_LOCATION" | 
            Type == "OBSERVATION" | Type == "START_OBSERVATION" | Type == "END_OBSERVATION") %>%
  select(Id_I, Type, Value,	Value_Id_C,	Day, Month, Year,	Start_day, Start_month, Start_year, End_day, End_month, End_year) %>%
  right_join(RP, by = c("Id_I"))