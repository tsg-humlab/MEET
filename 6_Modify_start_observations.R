# Step 18 Modify observations  

# Load packages

library(dplyr)
library(tidyr)
library(data.table)


# In each data set there can be cases that in which observations are consecutive and that the end of an observation
# has the same date as the start of the next observation. 

# Without changing the start date of the next observation the records in the Chronicle file will not be properly sorted 
# This is because the start of the next observation will be placed before the end of the previous observation. 

# This means that the final Migrations Episode File (MEF) will not be properly build. 

# This implies that the start dates of the next observation will be add with 0.01 to give them a slightly later date than the 
# end date of the previous observation.

# In some cases data set may have observations that have the same start and end date. These records will also have to be 
# modified, in this case the end date will be added with 0.01. 

# This step is divided in five substeps:

# 18.1 Create OBSERVATIONS_RP data frame
# 18.2 Prepare OBSERVATIONS_RP data frame
# 18.3 The end dates that are the same as the start dates in an observation are added with 0.01 
# 18.4 The start dates that match the previous end dates are added with 0.01   
# 18.5 The end dates that are the same as the start dates in an observation are added with 0.01
# 18.6 The data frames START_OBSERVATIONS_RP and END_OBSERVATIONS_RP are created


# Step 18.1 Create OBSERVATIONS_RP data frame

# Out of the INDIVIDUAL_DATA_RP data frame, only the OBSERVATION records are subsetted.
# Only the relevant columns are selected and the day, month, year columns are combined into 
# new Start_date and End_date columns
# In the last part only the Id_I, Type, Start_date and End_date are kept.

OBSERVATIONS_RP <- INDIVIDUAL_RP %>% 
  subset (Type == "OBSERVATION") %>%
  unite (Start_date, Start_year, Start_month, Start_day, sep = "-", remove = FALSE) %>% 
  mutate (Start_date = as.Date(Start_date)) %>%
  unite (End_date, End_year, End_month, End_day, sep = "-", remove = FALSE) %>% 
  mutate (End_date = as.Date(End_date)) %>%
  select(Id_I, Start_date, End_date, Type)


# 18.1 Prepare OBSERVATIONS_RP data frame

# First the data frame is sorted, then with using group_by and
# the previous End_date is place in a new column in the next record.  
# In the new column Contiguous is marked where the observations are contiguous.
# In the new column Start_is_end is marked which observation has the same 
# start and end date. The data frame is ungrouped.

OBSERVATIONS_RP_PREP <- OBSERVATIONS_RP %>% 
  arrange(Id_I, Start_date, End_date) %>%  
  group_by (Id_I) %>% 
  mutate (End_date_test = lag(End_date)) %>%
  mutate (Contiguous = if_else(Start_date == End_date_test, "Yes", "No")) %>% 
  mutate (Start_is_end = if_else(Start_date == End_date, "Yes", "No")) %>% 
  ungroup()


# 18.2 When Start_date == End_date: add 0.01 to End_date 

# In this step the End_date that match the Start_date in an observation 
# is added with 0.01. 

# First the OBSERVATIONS_RP_PREP is split up in a data frame 
# with records for which Start_is_end == "Yes" and one with "No". 
# In the first the End_dates are added with 0.01.

OBSERVATIONS_RP_START_END_YES_1 <- OBSERVATIONS_RP_PREP %>% 
  filter (Start_is_end == "Yes") %>%
  mutate(End_date = (End_date + 0.01))

OBSERVATIONS_RP_START_END_NO_1 <- OBSERVATIONS_RP_PREP %>% 
  filter (Start_is_end == "No") 

# With rbind the records of the two data frames are binded together and sorted.

OBSERVATIONS_RP_BIND_1 <- rbind(OBSERVATIONS_RP_START_END_YES_1, OBSERVATIONS_RP_START_END_NO_1) %>%
  arrange(Id_I, Start_date, End_date)


# 18.3 In case Start_date matches previous End_date: Start_date + 0.01

# In this step each Start_date that matches the previous End_date is added with 0.01. 

# First the OBSERVATIONS_RP_BIND_1 data frame is split up in a data frame 
# with records for which Contiguous == "Yes" and one with "No" or "NA". 
# In the first the Start_dates are added with 0.01.

OBSERVATIONS_RP_CONT_YES <- OBSERVATIONS_RP_BIND_1 %>% 
  filter (Contiguous == "Yes") %>%
  mutate(Start_date = (Start_date + 0.01))

OBSERVATIONS_RP_CONT_NOT <- OBSERVATIONS_RP_BIND_1 %>% 
  subset (Contiguous != "Yes" | is.na(Contiguous))

# With rbind the records of the two data frames are binded together and sorted.
# In the new column Start_is_end is marked which observation has the same 
# start and end date.

OBSERVATIONS_RP_BIND_2 <- rbind(OBSERVATIONS_RP_CONT_YES, OBSERVATIONS_RP_CONT_NOT) %>%
  arrange(Id_I, Start_date, End_date) %>% 
  select (Id_I, Start_date, End_date) %>%
  group_by (Id_I) %>% 
  mutate (Start_is_end = if_else(Start_date == End_date, "Yes", "No")) %>%
  ungroup()


# 18.4 When Start_date == End_date: add 0.01 to End_date 

# In this step the End_date that matches the Start_date in an observation 
# is added with 0.01. 

# First the OBSERVATIONS_RP_BIND_2 is split up in a data frame 
# with records for which Start_is_end == "Yes" and one with "No". 
# In the first the End_dates are added with 0.01.

OBSERVATIONS_RP_START_END_YES_2 <- OBSERVATIONS_RP_BIND_2 %>% 
  filter (Start_is_end == "Yes") %>%
  mutate(End_date = (End_date + 0.01))

OBSERVATIONS_RP_START_END_NO_2 <- OBSERVATIONS_RP_BIND_2 %>% 
  filter (Start_is_end == "No") 

# With rbind the records of the two data frames are binded together and sorted.
# The column Type with "OBSERVATION" is added to the data frame.
# Only the necessary columns are saved.

OBSERVATIONS_RP_BIND_3 <- rbind(OBSERVATIONS_RP_START_END_YES_2, OBSERVATIONS_RP_START_END_NO_2) %>%
  arrange(Id_I, Start_date, End_date) %>%
  mutate(Type = "OBSERVATION") %>%
  select (Id_I, Start_date, End_date, Type)


# 18.5 Create START_OBSERVATIONS_RP and END_OBSERVATIONS_RP data frames

# The OBSERVATIONS_RP_BIND_3 data frame is split up into the START_OBSERVATIONS_RP and END_OBSERVATIONS_RP
# data frames, two new columns are added (Variable and Value, both with NA) 
# The columns Type are changed into "START_OBSERVATION" and "END_OBSERVATION"

START_OBSERVATIONS_RP <- OBSERVATIONS_RP_BIND_3 %>% 
  select(Id_I, Start_date, Type) %>%
  rename(Date = Start_date) %>%
  mutate (Variable = NA) %>%
  mutate (Value = NA) %>%
  mutate(Type = "START_OBSERVATION") %>%
  select(Id_I, Variable, Value, Date, Type)

END_OBSERVATIONS_RP <- OBSERVATIONS_RP_BIND_3 %>% 
  select(Id_I, End_date, Type) %>%
  rename(Date = End_date) %>%
  mutate (Variable = NA) %>%
  mutate (Value = NA) %>%
  mutate(Type = "END_OBSERVATION") %>%
  select(Id_I, Variable, Value, Date, Type)


# 18.6 Combine and sort the START and END OBSERVATION data frames

# The START_OBSERVATIONS_RP and END_OBSERVATIONS_RP data frames are combined into 
# one and sorted on the Date and Id_I columns. 
# In the 8_Build_chronicle.R script the CHRON_OBS will be further used. 

CHRON_OBS <- rbind(START_OBSERVATIONS_RP, END_OBSERVATIONS_RP) %>%
  arrange(Id_I, Date)