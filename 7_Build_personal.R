# Step 19. In this script a range of data frames are made to be used to later on create the PERSONAL data frame: 

# - SEX_RP
# - BIRTH_DATE_RP
# - DEATH_DATE_RP
# - BIRTH_LOCATION_RP
# - MIGRATION_TOTAL_RP

#  After this these data frames are made they will be combined to create the PERSONAL data frame in step 19.	

# Load packages

library(dplyr)
library(tidyr)
library(data.table)


# 19.1 Create data frame SEX_RP

# In this step the data frame will be created that stores the sex of all RPs.
# The data is subsetted out of the INDIVIDUAL_DATA_RP data frame.
# The data frame is reordered in the last part of the code.

SEX_RP <- INDIVIDUAL_RP %>% subset(Type == "SEX") %>% 
  select (Id_I, Type, Value) %>% 
  mutate (Type = "Sex")


# 19.2 Create data frame BIRTH_DATE_RP 
  
# In this step the data frame will be created that stores the birth dates all RPs. 
# The data is subsetted out of the INDIVIDUAL_DATA_RP data frame.
# The information in the date columns Year, Month and Day are placed in one column called Date and are afterwards deleted.
# The data frame is reordered in the last part of the code.
  
BIRTH_DATE_RP <- INDIVIDUAL_RP %>% 
  subset(Type == "BIRTH_DATE") %>% 
  select(Id_I, Type, Value, Day, Month, Year) %>% 
  unite (Value, Year, Month, Day, sep = "-", remove = TRUE) %>% 
  select(Id_I, Type, Value) %>%
  mutate (Type = "Birth_date")


# 19.3 Create data frame DEATH_DATE_RP

# In this step the data frame will be created that stores the death dates all RPs. 
# The data is subsetted out of the INDIVIDUAL_DATA_RP data frame.
# The information in the date columns Year, Month and Day are placed in one column called Date and are afterwards deleted.
# The data frame is reordered in the last part of the code.

DEATH_DATE_RP <- INDIVIDUAL_RP %>% 
  subset(Type == "DEATH_DATE") %>% 
  select(Id_I, Type, Value, Day, Month, Year) %>% 
  unite (Value, Year, Month, Day, sep = "-", remove = TRUE) %>% 
  select(Id_I, Type, Value) %>%  
  mutate (Type = "Death_date")


# 19.4 Create data frame BIRTH_LOCATION_RP

# In this step the data frame will be created that stores the birth location of all RPs. 
# The data are first subsetted out of the INDIVIDUAL_DATA_RP data frame.
# The Value_Id_C column is renamed Id_C_B to match the corresponding column in the CONTEXT_B data frame
# The names of the locations of birth are retrieved via the column Value_Id_C and the Id_C column in the CONTEXT_B data frame.
# Then the data frame is reordered and non-relevant columns are removed
# The value_B column is renamed into Value.

BIRTH_LOCATION_RP <-INDIVIDUAL_RP %>% 
  subset(Type == "BIRTH_LOCATION") %>% 
  rename (Id_C_B = Value_Id_C) %>% 
  inner_join(CONTEXT_B, by = c("Id_C_B")) %>% 
  select (Id_I, Type, Value_B) %>% 
  rename (Value = Value_B) %>%
  mutate (Type = "Birth_location") 	


# 19.5 Create data frame MIGRATION_TOTAL_RP for future PERSONAL data frame:

# Select Id_I and Migration_total from MIGRATION_COMPLETE
# Filter out duplicate records. 
# Rename the column Migration_total: Value
# Add the column Type with "Migration_total" as value 
# Change the type of the column Value into character
# Reorder the data frame

MIGRATION_TOTAL_RP <- MIGRATION_COMPLETE %>% 
  select (Id_I, Migration_total) %>% 
  distinct(Id_I, Migration_total) %>% 
  rename (Value = Migration_total) %>% 
  mutate (Type = "Migration_total") %>% 
  mutate(Value=as.character(Value)) %>% 
  select(Id_I, Type, Value)


# Step 20 Combine the 5 newly created data frames into PERSONAL data frame

# In this step all data frames that were created in step 18 are combined into one using bind_rows and 
# sorted using the Id_I column.
# The outcome, the PERSONAL data frame, is to be used in the Episode File Creator.

PERSONAL <- bind_rows (SEX_RP, BIRTH_DATE_RP, DEATH_DATE_RP, BIRTH_LOCATION_RP, MIGRATION_TOTAL_RP) %>% 
  arrange (Id_I)


# Step 21 Transform the PERSONAL data frame from a long format into a wide format 

# The Id_I column remains, but the types in the Type column and the values in the Value column 
# are now combined in the columns, Birth_date, Birth_location, Death_date, Migration_total and Sex. 
# Logically the Type and Value columns have been deleted from the data frame. 
# The Birth_date and Death_date are stored as date.

PER <- PERSONAL %>% 
  spread (key = "Type", value= "Value", convert = TRUE) %>% 
  mutate (Birth_date = as.Date(Birth_date)) %>% 
  mutate (Death_date = as.Date(Death_date))