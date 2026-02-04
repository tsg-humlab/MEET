# Step 7.1 Create RP data frame

# To be able to extract all the migrations for the Research Persons (RPs), we need to first find the RPs.
# This script asks the user how the Research Persons are marked in the IDS set that is used.
# The user is first asked the Type and then the Value.

# Below the type and value that are used in the HSN: 

# Type: HSN_RESEARCH_PERSON
# Value: HSN RP

# Load packages

library(dplyr)
library(tidyr)
library(data.table)


# Step 7.1 Get the RPs

get_id_rp <- function(input_type=NULL, input_value=NULL) {
  
  
  # Ask the user for input twice
  if (is.null(input_type)) {
    IDS_RP_Type <- user_input_type()
  } else {
    IDS_RP_Type <- input_type
  }
  
  if (is.null(input_value)) {
    IDS_RP_Value <- user_input_value()
  } else {
    IDS_RP_Value <- input_value
  }

  # With the help of the user input, the RPs can be retrieved from the INDIVIDUAL data frame, 
  RP_TEMP <-INDIVIDUAL %>% subset(Type = IDS_RP_Type, Value == IDS_RP_Value) %>% select(Id_I)  
  
}

# These functions are called above
user_input_type<- function(){  
  x <- readline(prompt = "Enter Type that marks the RP in the IDS that you are using, please do not use quotes: ")
  return(x)
}

user_input_value<- function(){  
  y <- readline(prompt = "Enter Value that marks the RP in the IDS that you are using, please do not use quotes: ")
  return(y)
}

# Calling the main function and store the data frame as "RP"

RP <- get_id_rp()