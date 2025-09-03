# Step 22 Create CHRONICLE data frame

# This script creates the CHRONICLE data frame in a specific format that later on will be transformed to the chron
# data frame, with the help of the techniques in the isdr-package. 

# Load packages

library(dplyr)
library(tidyr)
library(data.table)

# 22.1 Change Type, Variable and Value in the new data frame

# This is done to transform the file for the final transformation 
# using the techniques of the isdr-package

# For the START_OBSERVATION records these values are changed:
# In the Variable column: "present"
# In the Value column: "yes"
# In the Type column: "Start_observation"

take <- CHRON_OBS$Type == "START_OBSERVATION"
CHRON_OBS$Variable[take] <- "Present"
CHRON_OBS$Value[take] <- "yes"
CHRON_OBS$Type[take] <- "Start_observation"

# For the END_OBSERVATION records these values are changed:
# In the Variable column: "present"
# In the Value column: "no"
# In the Type column: "End_observation"

take <- CHRON_OBS$Type == "END_OBSERVATION"
CHRON_OBS$Variable[take] <- "Present"
CHRON_OBS$Value[take] <- "no"
CHRON_OBS$Type[take] <- "End_observation"


# 22.2 Create CHRON_BIRTH_START_EVENT data frame out of the already existing 
# BIRTH_DATE_RP that stores the start of the events, in this case, 

# the date of birth:
# The values in the Type column are changed into "Migration_event_start"
# The column Value is changed into Date and stored as a date value
# From the dates in the column Date is 0.01 subtracted to make sure that the start of the event is
# placed before the start of the observation TH: maybe not necessary??
# The column Value is added, with "yes" as Value
# The column Variable is added, with "birth" as Value
# The data frame is reordered

CHRON_BIRTH_START_EVENT <- BIRTH_DATE_RP %>%
  mutate (Type = "Migration_event_start") %>%
  mutate (Date = Value) %>%
  mutate (Date = as.Date(Date)) %>%	
  mutate (Date = (Date-0.01)) %>%
  mutate (Value = "yes") %>% 
  mutate (Variable = "Birth") %>%
  select (Id_I, Variable, Value, Date, Type)


# 22.3 Create CHRON_MIGR_END_EVENT data frame

# This data frame stores the migration events. The data comes from the MIGRATION_COMPLETE
# data frame.
#
# The columns Id_I, End_year, [Start_year_2], End_month [Start_month_2], End_day [Start_day_2], and Migration_sequence are selected
# The column Migration_sequence is renamed into Value and
# the values changed into "no" and stored as character
# The column Variable with "migrated" as value is added
# The column Type with "Migration_event_end" as value is added
# The information in the date columns End_year, End__month and End_day 
# are placed in one column called Date and are afterwards deleted. 
# The Date column is changed stored as "Date"
# From the dates in the column Date 0.01 is subtracted so that in case of an end of an observation  
# on the same day, the event is placed before the end of the observation in the CHRONICLE / CHRON data frame
# The data frame is reordered.

CHRON_MIGR_END_EVENT <- MIGRATION_COMPLETE %>% 
  select (Id_I, End_year, End_month, End_day, Migration_sequence) %>% 
  rename (Value = Migration_sequence) %>% 
  mutate (Value = "no") %>%
  mutate (Value = as.character(Value)) %>% 
  mutate (Variable = "Migrated") %>% 
  mutate (Type = "Migration_event_end") %>% 
  unite  (Date, End_year, End_month, End_day, sep = "-", remove = TRUE) %>% 
  mutate (Date = "is.na<-"(Date, Date == "NA-NA-NA")) %>%
  mutate (Date = as.Date(Date)) %>%
  select (Id_I, Variable, Value, Date, Type) 


# 22.4 Create CHRON_RESIDENCE data frame

# In order to know in which municipality each RP in each spell lived,  
# the CHRON_RESIDENCE data frame is created. This data frame, based on the 
# CONTEXT_RP_CONTEXTS_COUNTS data frame that was created earlier, stores all the
# municipalities, with the start date where the RP has lived. 

# This data frame is build basically the same way as the others:

# Out of the CONTEXT_RP_CONTEXTS_COUNTS the columns Id_I, Municipality, 
# Start_day, Start_month, Start_year are selected. 
# The information in the date columns Start_year, Start_month, Start_day, 
# are placed in one column called Date and are afterwards deleted. 
# The Date column is changed and stored as a "Date" column.
# The name of the column Municipality is changed into "Value".
# The column Type with "Residence" as value is added.
# The column Variable with "Residence" as value is added.
# The columns in the data frame are reordered. 

CHRON_RESIDENCE <- CONTEXT_RP_CONTEXTS_COUNTS %>%
  select (Id_I, Municipality, Start_day, Start_month, Start_year) %>%
  unite (Date, Start_year, Start_month, Start_day, sep = "-", remove = TRUE) %>% 
  mutate (Date = as.Date(Date)) %>%
  mutate (Value = Municipality) %>%
  mutate (Type = "Residence") %>%
  mutate (Variable = "Residence" ) %>%
  select (Id_I, Variable, Value, Date, Type)


# 22.5 Combine the four new data frames

# Now that the four data frames are ready, and all have the 
# same structure, they can be combined into the CHRONICLE data frame.
# When this is done, the data frame is sorted using the Id_I and Date columns.

CHRONICLE <- bind_rows(CHRON_OBS, CHRON_BIRTH_START_EVENT, CHRON_MIGR_END_EVENT, CHRON_RESIDENCE) %>% 
  arrange(Id_I, Date)
