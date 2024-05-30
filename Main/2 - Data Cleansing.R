# Load required libraries
library(janitor)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

# Function to cleanse a single dataset
cleanse_data <- function(data) {
  data <- data %>%
    # Step 1: Remove symbols like £, commas, and quotes
    mutate(across(everything(), ~ {
      .x <- str_replace_all(.x, "[£,]", "")
      .x <- str_replace_all(.x, "['\"]", "")
    })) %>%
    # Step 2: Convert character columns to double if they contain numeric values
    mutate(across(where(is.character), ~ {
      numeric_val <- suppressWarnings(as.numeric(.x))
      if (!all(is.na(numeric_val))) {
        round(numeric_val, 0)
      } else {
        if(all(is.na(.x))) {
          0
        } else {
          .x
        }
      }
    })) %>%
    # Step 3: Rename variables if they exist in the dataset
    rename_with(~ str_replace_all(.x, c(
      'Office maximum budget available' = 'Office budget',
      'Staffing maximum budget available' = 'Staffing budget',
      'Accommodation maximum budget available' = 'Accommodation budget',
      'Travel and subsistence spend' = 'Travel and subsistence (uncapped)',
      'Other costs spend' = 'Other costs (uncapped)',
      'Winding-up spend' = 'Wind-up spend',
      'Wind-up maximum budget available' = 'Wind-up budget',
      'Winding-up budget' = 'Wind-up budget'
    )), everything()) %>%
    # Step 3.1: Include "Start-up spend" variable if it doesn't exist and fill missing values with 0
    {
      if (!"Start-up spend" %in% colnames(.)) {
        mutate(., `Start-up spend` = 0)
      } else {
        .
      }
    } %>%
    # Step 3.2: Replace NA's in specific columns with 0
    mutate(across(c('Start-up spend', 'Wind-up spend', 'Accommodation budget', 'Accommodation spend'), ~ replace_na(.x, 0))) %>%
    # Step 4: Derive variables using provided formulas
    mutate(
      `Subtotal of office running costs` = `Office spend` + `Staffing spend` + `Wind-up spend` + `Start-up spend`,
      `Subtotal of other parliamentary costs` = `Accommodation spend` + `Travel and subsistence (uncapped)` + `Other costs (uncapped)`,
      `Overall total spend for this financial year` = `Office spend` + `Staffing spend` + `Accommodation spend` + `Travel and subsistence (uncapped)` + `Other costs (uncapped)` + `Wind-up spend` + `Start-up spend`
    ) %>%
    # Step 5: Include "Office budget" variable if it doesn't exist and fill missing values with 0
    {
      if (!"Office budget" %in% colnames(.)) {
        mutate(., `Office budget` = 0)
      } else {
        .
      }
    } %>%
    # Step 5.1: Replace negative values with positive values for variables with 'total' or 'spend' in their names
    mutate(across(matches("Subtotal of other parliamentary costs"), ~ ifelse(.x < 0, abs(.x), .x))) %>%
    # Step 6: Drop variables not needed for analysis
    select(-contains("reason"), 
           -contains("Travel and subsistence maximum budget available"),
           -contains("Other costs maximum budget available")
    ) %>%
    # Step 7: Clean column names using janitor package
    clean_names()
  
  return(data)
}

# Apply the cleansing function to each dataset in the list
cleaned_data <- map(all_data, cleanse_data)

# Bind the datasets for each year together to form one dataset 'processed_data'
processed_data <- bind_rows(cleaned_data)

# Replace NA's
processed_data <- replace(processed_data, is.na(processed_data), 0)
