# Step 24. Add data on the migration events to the CHRON data frame

# In this step the information on the origin and destination, the sequence and the type of each migration event
# are added to the CHRON data frame. The Id_I of each RP and the date of each migration / event is used to link each event 
# in the CHRON date frame to the correct migration record extracted from the MIGRATION_COMPLETE data frame.

# Load packages

library(dplyr)
library(tidyr)
library(data.table)


# 24.1 Create CHRONICLE_MIGRATION data frame

# First the data frame CHRONICLE_MIGRATION is created that stores all the migration records that need
# to be added to the CHRON data frame. The data is taken out of the MIGRATION_BIND data frame that
# was created earlier. First the relevant columns are selected, two are renamed and 
# then in the column Link a unique id is created using Id_I and the Event_date.  
# This id will be used to add the records to the correct events. 

CHRONICLE_MIGRATION <- MIGRATION_COMPLETE %>% 
  select (Id_I, Municipality_D, Municipality_O, Migration_sequence, Migration_type, End_year, End_month, End_day) %>%
  unite (Event_date, End_year, End_month, End_day, sep = "-", na.rm = TRUE, remove = TRUE) %>% 
  mutate (Event_date = as.Date(Event_date)) %>%
  rename (Migration_origin = Municipality_O) %>%
  rename (Migration_destination = Municipality_D) %>%
  mutate (Link = paste(Id_I, Event_date, sep = "_"))


# 24.2 Add CHRONICLE_MIGRATION to the CHRON data frame, by "Link"

# Next, in the CHRON data frame a column with the name Link, the same unique id ("Link") is also added
# to the data frame (using Id_I and the column Date). Using Link, the contents of the 
# CHRONICLE_MIGRATION data frame are added to the CHRON data frame, the Id_I column of the CHRON data frame 
# that was renamed during the join process into Id_I.x is renamed back into Id_I and the last steps 
# are selecting the columns that we want to keep and ungrouping the data frame. 

CHRON <- CHRON %>% 
  group_by (Id_I) %>%          
  mutate (Link = paste(Id_I, lead(Date), sep = "_")) %>%
  mutate (Link = replace(Link, Event == FALSE, NA)) %>% 
  left_join (CHRONICLE_MIGRATION, by=c("Link")) %>%  
  rename (Id_I = Id_I.x) %>% 
  select (Id_I, Date, Migration_origin, Migration_destination, Migration_sequence, Migration_type, Residence, Start_date, Enter, Exit, Event) %>%
  ungroup()