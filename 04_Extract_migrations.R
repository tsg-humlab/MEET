# Steps 7.2 - 16.8

# This script creates a data frame in which the migration histories of all RPs are stored.  
# This MIGRATION_COMPLETE data frame is the basis of the Migration Episode File (MEF), that is constructed
# later on. 

# Load packages

library(dplyr)
library(tidyr)
library(data.table)


# Step 7.2 Change row names 

# Changing the row names of the RP data frame

rownames(RP) <- 1:nrow(RP)


# Step 8. Select the contexts (Id_C) of the RPs in INDIV_CONTEXT

# Since we now have the RPs in the data frame RP, we can now select all the contexts 
# belonging to the RPs by merging the RP data frame with the INDIV_CONTEXT data frame.
# Only the relevant columns will be stored in the new data frame. 

INDIV_CONTEXT_RP <- RP %>% 
  inner_join(INDIV_CONTEXT, by = c("Id_I")) %>% 
  select(Id_I, Id_C, Start_day, Start_month, Start_year) 


# Step 9. Create clean data frame CONTEXT_CONTEXT_CLEAN

# In this step a modified version of CONTEXT_CONTEXT is created with only the Id_C_1 and Id_C_2 codes 
# with the Relation "Address and Municipality" in the new data frame. 
# This way the codes of the localities ("Address and Locality") will not be retrieved in step 9 
# when the INDIV_CONTEXT_RP data frame will be joined with the information in CONTEXT_CONTEXT.

CONTEXT_CONTEXT_CLEAN <- CONTEXT_CONTEXT %>% 
  subset(Relation == "Address and Municipality") %>% 
  select(Id_C_1, Id_C_2_nw) 

# Step 10. Add Id_C_2 codes to the data frame

# The next step is to use the Id_C codes from INDIV_CONTEXT_RP and the Id_C_1 codes of CONTEXT_CONTEXT_CLEAN 
# to retrieve the Id_C_2 codes of the municipalities. This is done by joining the two data frames 
# and keeping the records that have a match in the second table. 
# Only the relevant columns will be stored in the new data frame. 

CONTEXT_CONTEXT_RP <- INDIV_CONTEXT_RP %>% 
  inner_join(CONTEXT_CONTEXT_CLEAN, by = c("Id_C" = "Id_C_1")) %>% 
  select(Id_I, Id_C, Id_C_2_nw, Start_day, Start_month, Start_year)	


# Step 11. Create clean data frame CONTEXT_CLEAN 

# In this step a modified version of CONTEXT is created so that only the names of the  
# geographical contexts will be added to the data frame in step 11. 
# The Value column is changed into Municipality and to better distinguish between 
# the different Id_C columns in step 11, the name of the Id_C column in CONTEXT_CLEAN 
# is changed into Id_C_clean.

CONTEXT_CLEAN <- CONTEXT %>% 
  subset(Type == "NAME") %>% 
  select(Id_C, Value) %>% 
  rename(Id_C_clean = Id_C) %>%			
  rename(Municipality = Value) 							


# Step 12. Add the names of municipalities to the data frame

# Now that the CONTEXT table has a clean version, we can add the names of the municipalities to the new data frame.

CONTEXT_RP <- CONTEXT_CONTEXT_RP %>% 
  inner_join(CONTEXT_CLEAN, by =  c("Id_C_2_nw" = "Id_C_clean")) %>%
  rename(Id_C_2 = Id_C_2_nw) %>%	# Rename de extra column
  select(Id_I, Id_C, Id_C_2, Municipality, Start_day, Start_month, Start_year) %>%
  arrange(Id_I, Start_year, Start_month, Start_day)


# Step 13. Filtering out intra-municipal migrations in the CONTEXT_RP data frame

# In this step the data frame will be cleared of the intra-municipal migrations because we want 
# the migration data frame to show only the migrations between municipalities. 
# This will be done by counting the sequences of the municipalities (per ID) in the column Municipality 
# and add the sequences to Count_1. 
# This new column will help to find the first address of each sequence of addresses (where Count_1 = 1). 

# 13.1 Sort data frame

# First the data frame CONTEXT_RP needs to be sorted, using Id_I, Start_year, Start_month and Start_day.
# This is done to make sure that all contexts for each RP are chronologically ordered.

CONTEXT_RP_ORDERED <- CONTEXT_RP %>% 
  arrange (Id_I, Start_year, Start_month, Start_day) 


# 13.2 Add column Count_1

# Then a column called Count_1 that shows per Id_I and per Municipality
# the sequence of records, is added to the data frame

# Add new empty column to the data frame
CONTEXT_RP_ORDERED$Count_1 <- NA

# This for loop counts the occurrences per Id_I of each new sequence of municipalities.

for(i in seq_len(nrow(CONTEXT_RP_ORDERED))) {
  if (i == 1 || (CONTEXT_RP_ORDERED$Municipality[i] != CONTEXT_RP_ORDERED$Municipality[i-1] || (CONTEXT_RP_ORDERED$Id_I[i] != CONTEXT_RP_ORDERED$Id_I[i-1])))
    CONTEXT_RP_ORDERED$Count_1[i] <- 1
  else
    CONTEXT_RP_ORDERED$Count_1[i] <- CONTEXT_RP_ORDERED$Count_1[i-1] + 1
}


# 13.3 Keep only every first record of each sequence.

# Only the first records of each sequence are stored in a new data frame 
# This is done by selecting the records where Count_1 has the value 1, so that we have a data frame 
# with all of the first addresses for each municipality the RP has lived in.  

CONTEXT_RP_COUNT_1 <- CONTEXT_RP_ORDERED %>% 
  subset(Count_1 == 1) %>% 
  select (Id_I, Id_C, Id_C_2, Municipality, Start_day, Start_month, Start_year, Count_1)


# Step 14. Create subsets of the CONTEXT and CONTEXT_CONTEXT data frames

# The reason for creating a range of customized versions of the CONTEXT and CONTEXT_CONTEXT data frames with  
# adapted the names of the columns is that it will be easier to keep track of the extra columns 
# when they are added to the main migration data frame in step 15 when the Id_C_2 codes and the names of the regions
# and the countries in which the municipalities are located are retrieved 

# 14.1 Create subset CONTEXT_CONTEXT_A for retrieval of the Id_C_2 code of the Region

# Subset only the records with the relation "Municipality and Region"
# Select only the Id_C_1 and Id_C_2 columns
# Rename the names of these columns to make them unique
  
CONTEXT_CONTEXT_A <- CONTEXT_CONTEXT %>% 
  subset(Relation == "Municipality and Region") %>% 
  select(Id_C_1, Id_C_2) %>% 
  rename(Id_C_1_A = Id_C_1, Id_C_2_A = Id_C_2)


# 14.2 Create subset CONTEXT_B for retrieval of the name of the Region

# Subset only the records with for which Type = "NAME"
# Select only the Id_C and Value columns
# Rename the names of these columns to make them unique

CONTEXT_B <- CONTEXT %>% 
  subset(Type == "NAME") %>% 
  select(Id_C, Value) %>% 
  rename(Id_C_B = Id_C, Value_B = Value)


# 14.3 Create subset CONTEXT_CONTEXT_C for retrieval of the Id_C_2 code of the Country

# Subset only the records with the relation "Region and Country"
# Select only the Id_C_1 and Id_C_2 columns
# Rename the names of these columns to make them unique

CONTEXT_CONTEXT_C <- CONTEXT_CONTEXT %>% 
  subset(Relation == "Region and Country") %>% 
  select(Id_C_1, Id_C_2) %>% 
  rename(Id_C_1_C = Id_C_1, Id_C_2_C = Id_C_2)


# 14.4 Create subset CONTEXT_D retrieval of the name of the Country

# Subset only the records with for which Type = "NAME"
# Select only the Id_C and Value columns
# Rename the names of these columns to make them unique

CONTEXT_D <- CONTEXT %>% 
  subset(Type == "NAME") %>% 
  select(Id_C, Value) %>% 
  rename(Id_C_D = Id_C, Value_D = Value)

# Step 15. Adding the contexts Region and Country to the data frame

# In this step the names of the regions and the countries in which the municipalities are located are 
# added in new columns, using the four data frames created in Step 13.

# First the codes of the region in which the municipalities are located are added to the data frame (Id_C_2_A)
# and only the necessary columns are stored

# Second, the name of the region is added to the data frame (Value_B) 
# and only the necessary columns are stored

# Third, the code of the region is used to retrieve the code of the country (Id_C_2_C)
# and only the necessary columns are stored

# Fourth, the name of the country is added to the data frame (Value_D) 
# and only the necessary columns are stored

# The columns Value_B and Value_D are renamed
# The data frame is sorted

CONTEXT_RP_CONTEXTS <- CONTEXT_RP_COUNT_1 %>% 
  left_join(CONTEXT_CONTEXT_A, by = c("Id_C_2" = "Id_C_1_A")) %>%
  select(Id_I, Id_C, Id_C_2, Municipality, Id_C_2_A, Start_day, Start_month, Start_year) %>%
  left_join(CONTEXT_B, by = c("Id_C_2_A" = "Id_C_B")) %>%
  select(Id_I, Id_C, Id_C_2, Municipality, Id_C_2_A, Value_B, Start_day, Start_month, Start_year) %>%
  left_join(CONTEXT_CONTEXT_C, by = c("Id_C_2_A" = "Id_C_1_C")) %>%
  select(Id_I, Id_C, Id_C_2, Municipality, Id_C_2_A, Value_B, Id_C_2_C, Start_day, Start_month, Start_year) %>%
  left_join(CONTEXT_D, by = c("Id_C_2_C" = "Id_C_D")) %>%
  select(Id_I, Id_C, Id_C_2, Municipality, Value_B, Value_D, Start_day, Start_month, Start_year) %>% 
  rename(Region = Value_B, Country = Value_D) %>%
  arrange(Id_I, Start_year, Start_month, Start_day)
  

# Step 16. Create data frame with all migrations for all RPs

# The CONTEXT_RP_CONTEXTS data frame is first split up into two parts: one with all the records of RPs who do not migrate beyond 
# the municipality where they were born (MIGRATION_RP_NO) and one data frame that contains the migrations of the RPs that 
# migrate once or more. 

# The data frame with migrations is again split into two data frames, one with the records of the municipalities where 
# the RP has migrated from (MIGRATION_MUNICIPALITIES_1) and one with the municipalities 
# where the RP has migrated to (MIGRATION_MUNICIPALITIES_2). 

# 16.1 Add two new columns to the data frame that show per Id_I the sequences of the records and  
# for each record the highest number of each sequence. 

CONTEXT_RP_CONTEXTS_COUNTS <- CONTEXT_RP_CONTEXTS %>% 
  group_by(Id_I) %>% 
  mutate(Count_3 = seq(n())) %>% 
  group_by(Id_I) %>% 
  mutate(Count_4 = tail(n())) %>%
  ungroup()
 

# 16.2 Subset the records for RPs without migration in a new data frame. Since for a RP without migration 
# there is only one record in the data frame, they can be selected by using Count_4 == 1. (STAYERS)

MIGRATION_RP_NO <- CONTEXT_RP_CONTEXTS_COUNTS %>% 
  subset(Count_4 == 1) %>% 
  select (Id_I, Id_C, Id_C_2, Municipality, Region, Country, Start_day, Start_month, Start_year)
    

# Step 16.3 Add new columns to MIGRATION_RP_NO data frame (STAYERS)

# Before it is possible to combine the data frames with RP who did and did not migrate out of their municipality where they were born, 
# the last one needs to be changed so they match up with each other. 

# This means that a range of columns needs to be added to the MIGRATION_RP_NO data frame so that both data frames have the same columns. 
# Most columns are filled in with "NA" but the Migration_sequence and Migration_total are filled in with "0". 

MIGRATION_RP_NO <- MIGRATION_RP_NO %>% 
  mutate (Id_I_D = NA) %>% 
  mutate (Id_C_2_D = NA) %>% 
  mutate (End_day = NA) %>% 
  mutate (End_month = NA) %>% 
  mutate (End_year = NA) %>% 
  mutate (Municipality_D = NA) %>% 
  mutate (Region_D = NA) %>% 
  mutate (Country_D = NA) %>%
  
  mutate (Migration_sequence = 0) %>% 
  mutate (Migration_total = 0) %>% 
  mutate (Migration_type = NA)


# 16.4 Subset the remaining records (for RPs with migration) in new data frame, by selecting 
# the records for which Count_4 is larger than 1.(MOVERS)

MIGRATION_RP_YES <- CONTEXT_RP_CONTEXTS_COUNTS %>% 
  subset(Count_4 > 1) %>% 
  select (Id_I, Id_C, Id_C_2, Municipality, Region, Country, Start_day, Start_month, Start_year, Count_3, Count_4)


# 16.5 Subset MIGRATION_YES to create a new data frame that stores the data for the destination of each migration

# All records except the first are subsetted (Count_3 is not 1)
# and then only the necessary columns are selected and 
# renamed and placed into a new data frame.

MIGRATION_MUNICIPALITY_DEST <- MIGRATION_RP_YES %>% 
  subset(Count_3 != 1) %>% 
  select(Id_I, Id_C_2, Start_day, Start_month, Start_year, Municipality, Region, Country) %>% 
  rename (Id_I_D = Id_I, Id_C_2_D = Id_C_2, Municipality_D = Municipality, Region_D = Region, Country_D = Country, End_day = Start_day, End_month = Start_month, End_year = Start_year) 


# 16.6 Subset MIGRATION_RP_YES to create a new data frame that stores the data for the origin of each migration

# All records except the first are subsetted (Count_3 is not equal to Count_4)
# and then only the necessary columns are selected and 
# renamed and placed into a new data frame.

MIGRATION_MUNICIPALITY_ORIG <- MIGRATION_RP_YES %>% 
  subset(Count_3 != Count_4) %>% 
  select (Id_I, Id_C, Id_C_2, Start_day, Start_month, Start_year, Municipality, Region, Country) 


# Step 16.7 Combine the MIGRATION_MUNICIPALITY_ORIG and MIGRATION_MUNICIPALITY_DEST data frames 

# In this step, the two data frames are combined into the MIGRATION_BI data frame, that shows for each RP
# all migrations between municipalities, the origin on the left (Municipality) and the destination (Municipality_2) on the right. 
# Three new columns are added to the data frame after this part.

# The column Migration_sequence that shows for each RP the sequence of each migration
# The column Migration_total that shows for each RP the total number of migrations
# The column Migration_type that shows for each RP the kind of migration 

# As for both Municipality and Municipality_2 the names of the countries where they are located in, 
# have been added to the data frame, it is possible to add a column with the type of the migration to the data frame, 
# for all migrations. If a migration takes place in one country, the migration is labeled: “Internal”. 
# If the migrant comes from outside the country or leaves the country, the migration will be labeled as “External”. 

MIGRATION_ORIG_DEST <- bind_cols(MIGRATION_MUNICIPALITY_ORIG, MIGRATION_MUNICIPALITY_DEST) %>% 
  group_by(Id_I) %>% 
  mutate(Migration_sequence = seq(n())) %>%
  group_by(Id_I) %>% 
  mutate(Migration_total = tail(n())) %>%
  mutate(Migration_type = if_else(Country == Country_D, "Internal", "International")) 

# Step 16.8. Combine the data frames MIGRATION_RP_NO and MIGRATION_ORIG_DEST

# Now that both data frames have the same structure, it is possible to combine the two. 
# Having both data frames combined, means that it is easier to extract data to build the PERSONAL and CHRONICLE data frames later. 

MIGRATION_COMPLETE <- bind_rows(MIGRATION_RP_NO, MIGRATION_ORIG_DEST) %>% 
  arrange(Id_I)

# Afterwards some of the names of the fields can be altered in order to harmonize the names of the fields in the 
# MIGRATION_COMPLETE data frame

MIGRATION_COMPLETE <- MIGRATION_COMPLETE %>%
  rename(Municipality_O = Municipality, Region_O = Region, Country_O = Country)