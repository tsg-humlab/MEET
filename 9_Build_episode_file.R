# Step 23 Create the CHRON data frame

# In this step the CHRON data frame will be constructed with the help of the methods in the isdr-package. 
# The end result will be joined with the data in the per data frame.

# Load packages

library(dplyr)
library(tidyr)
library(data.table)

# 23.1 Spread out the Chronicle data frame

# Here, basically the same happens with the PERSONAL data frame but first the data frame is sorted 
# using the Id_I and the Date columns and duplicate records are removed. 

CHRON <- CHRONICLE %>% 
  select (Id_I, Variable, Value, Date, Type) %>%
  arrange (Id_I, Date) %>%
  distinct (Id_I, Variable, Value, Date, Type, .keep_all = TRUE) %>%	
  spread (key = "Variable", value = "Value", convert = TRUE)


# 23.2 Fill in the blank cells

# The second step is filling in the empty cells of the columns birth, migrated and present 
# using the contents of the cell above each empty cell

CHRON <- CHRON %>%
  group_by(Id_I) %>% 
  fill (names(CHRON)[-(1:2)]) %>% 
  ungroup()


# Step 23.3 Add the start date of the migration event(s) to the CHRON data frame. 

# First from the CHRON data frame all records with Type = "Migration_event_start" are 
# subsetted in a new data frame and only the Id_I and the Date columns are selected.
# The name of the Date column is changed into Start_date. 

STARTING <- CHRON %>%
  subset(Type == "Migration_event_start") %>%
  select (Id_I, Date) %>%
  rename (Start_date = Date)

# Then the STARTING data frame is joined with the CHRON data frame

CHRON <- CHRON %>%
  inner_join(STARTING, by = c("Id_I"))


# Step 23.4 Add enter column to the data frame

# The enter column is the duration since start_date and shows the age in years.

CHRON <- CHRON %>%
  mutate (Enter = as.numeric(Date - Start_date)) %>%
  mutate (Enter = round(Enter/365.2425, 3))


# Step 23.5 Add exit column to the data frame

# For each Id_I (group by) the new column is filled with the data from the enter column
# from one record lower and if not available or the last record, with NA. 

CHRON <- CHRON %>% 
  group_by(Id_I) %>% 
  mutate(Exit = c(Enter[-1], NA)) %>%
  ungroup()


# Step 23.6 Add column event

# Now the column event is added to the data frame. It indicates an spell ending with "migration_end_event". 
# Please note: it corresponds to the row that precedes the row with Type == migration_end_event.

CHRON$Event <- NA
erows <- which(CHRON$Type == "Migration_event_end") - 1
CHRON$Event <- FALSE
CHRON$Event[erows] <- TRUE


# Step 23.7 Cleaning up the CHRON data frame

# Keep records with present is "yes" and present is not "NA" 

CHRON <- CHRON %>% filter(!is.na(Present)) %>% 
  subset(Present == "yes") 

# Create an index that stores the records that need to be deleted: 
# records that are not "TRUE" and for which exit is not "NA" and for which 
# enter and exit are the same

remove <- !CHRON$Event & !is.na(CHRON$Exit) & (CHRON$Enter == CHRON$Exit)
CHRON <- CHRON[!remove, ]

# Keep only necessary columns:

CHRON <- CHRON %>% select (Id_I, Date, Start_date, Residence, Enter, Exit, Event)