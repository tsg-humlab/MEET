# Step 6. Import IDS tables in R as data frames

# In this step four IDS tables will be imported in R as data frames: 
# INDIVIDUAL, INDIV_CONTEXT, CONTEXT_CONTEXT and CONTEXT. 

# Some columns (Id_I and Id_C) need to be modified because R adds ",00" after each value 
# and they are not automatically interpreted as numerical as they should be.

# Since the HSN database is encoded as "Latin-1" we use that encoding when importing the tables.

# Load packages

library(dplyr)
library(tidyr)
library(data.table)

# Step 6.1 Import INDIVIDUAL table

# The table is imported in R as a data frame and the column Id_I is changed:

INDIVIDUAL <- fread("INDIVIDUAL_CLEAN.txt", encoding="Latin-1") %>% 
  mutate (Id_I = as.numeric(gsub(",00", "", Id_I))) %>%  
  mutate (Value_Id_C = as.numeric(gsub(",00", "", Value_Id_C)))


# Step 6.2 Import INDIV_CONTEXT table

# The table is imported in R as a data frame and the columns Id_I and Id_C are changed:

INDIV_CONTEXT <- fread("INDIV_CONTEXT.txt", encoding="Latin-1") %>%
  mutate (Id_I = as.numeric(gsub(",00", "", Id_I))) %>%
  mutate (Id_C = as.numeric(gsub(",00", "", Id_C)))


# Step 6.3 Import CONTEXT_CONTEXT table

# The table is imported in R as a data frame and the columns Id_C_1 and Id_C_2 are changed:

CONTEXT_CONTEXT <- fread("CONTEXT_CONTEXT.txt", encoding="Latin-1") %>%
  mutate (Id_C_1 = as.numeric(gsub(",00", "", Id_C_1))) %>%
  mutate (Id_C_2 = as.numeric(gsub(",00", "", Id_C_2)))


# Step 6.4 Import CONTEXT table

# The table is imported in R as a data frame and the column Id_C is changed:

CONTEXT <- fread("CONTEXT.txt", encoding="Latin-1") %>%
  mutate (Id_C = as.numeric(gsub(",00", "", Id_C)))