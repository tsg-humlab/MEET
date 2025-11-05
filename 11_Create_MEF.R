# Step 25. Create Migration Episode File (MEF)

# This script joins the CHRON and PER data frames in a new data frame, the Migration Episode File. 
# Id_I is used as the variable to join by.  

# Load packages

library(dplyr)
library(tidyr)
library(data.table)

MEF <- CHRON %>% 
  inner_join (PER, by = c("Id_I"))