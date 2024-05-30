# Set options to display numbers without scientific notation
options(scipen = 999)

# Load required library
library(janitor)
library(dplyr)
library(purrr)
library(testthat)
library(tibble)
library(testthat)

# Define the variables to select
vars_to_select <- c("Constituency", "Office budget", "Office spend",
                    "Remaining office budget", "Staffing budget", "Staffing spend",
                    "Remaining staffing budget", "Subtotal of office running costs",
                    "Accommodation budget", "Accommodation spend", "Remaining accommodation budget",
                    "Travel and subsistence (uncapped)", "Other costs (uncapped)",
                    "Subtotal of other parliamentary costs", "Overall total spend for this financial year",
                    "Office maximum budget available", "Staffing maximum budget available",
                    "Accommodation maximum budget available", "Travel and subsistence spend",
                    "Other costs spend", "pID", "Year")

# Function to select specified variables, clean column names, and clean values
select_vars <- function(data, vars_to_select) {
  selected <- data[, intersect(names(data), vars_to_select)]
  processed_data <- selected %>% clean_names()  
  
  # Clean numerical values by removing symbols
  processed_data[] <- lapply(processed_data, function(col) {
    as.numeric(gsub("[^0-9.]", "", col))
  })
  
  return(processed_data)
}

# Output original data sets imported from IPSA website by year
# Filter processed data by year

# 2010/11
df_201011_unprocessed <- select_vars(all_data[[1]], vars_to_select)
df_201011_processed <- filter(processed_data, year == "2010/11")

# 2011/12
df_201112_unprocessed <- select_vars(all_data[[2]], vars_to_select)
df_201112_processed <- filter(processed_data, year == "2011/12")

# 2012/13
df_201213_unprocessed <- select_vars(all_data[[3]], vars_to_select)
df_201213_processed <- filter(processed_data, year == "2012/13")

# 2013/14
df_201314_unprocessed <- select_vars(all_data[[4]], vars_to_select)
df_201314_processed <- filter(processed_data, year == "2013/14")

# 2014/15
df_201415_unprocessed <- select_vars(all_data[[5]], vars_to_select)
df_201415_processed <- filter(processed_data, year == "2014/15")

# 2015/16
df_201516_unprocessed <- select_vars(all_data[[6]], vars_to_select)
df_201516_processed <- filter(processed_data, year == "2015/16")

# 2016/17
df_201617_unprocessed <- select_vars(all_data[[7]], vars_to_select)
df_201617_processed <- filter(processed_data, year == "2016/17")

# 2017/18
df_201718_unprocessed <- select_vars(all_data[[8]], vars_to_select)
df_201718_processed <- filter(processed_data, year == "2017/18")

# 2018/19
df_201819_unprocessed <- select_vars(all_data[[9]], vars_to_select)
df_201819_processed <- filter(processed_data, year == "2018/19")

# 2019/20
df_201920_unprocessed <- select_vars(all_data[[10]], vars_to_select)
df_201920_processed <- filter(processed_data, year == "2019/20")

# 2020/21
df_202021_unprocessed <- select_vars(all_data[[11]], vars_to_select)
df_202021_processed <- filter(processed_data, year == "2020/21")

# 2021/22
df_202122_unprocessed <- select_vars(all_data[[12]], vars_to_select)
df_202122_processed <- filter(processed_data, year == "2021/22")

# 2022/23
df_202223_unprocessed <- select_vars(all_data[[13]], vars_to_select)
df_202223_processed <- filter(processed_data, year == "2022/23")

# Join processed and unprocessed data sets 

# Define the years range
years <- c("201011", "201112", "201213", "201314", "201415", 
           "201516", "201617", "201718", "201819", "201920", 
           "202021", "202122", "202223")

# Initialise an empty list to store results
results <- list()

# Define the columns to select
columns_to_select <- c("p_id", "staffing_spend", "subtotal_of_office_running_costs", 
                       "subtotal_of_other_parliamentary_costs", "overall_total_spend_for_this_financial_year")

# Function to check and select existing columns
safe_select <- function(df, columns) {
  existing_columns <- columns[columns %in% names(df)]
  select(df, all_of(existing_columns))
}

# Loop through each year
for (year in years) {
  
  # Construct the names of the dataframes
  processed_df_name <- paste0("df_", year, "_processed")
  unprocessed_df_name <- paste0("df_", year, "_unprocessed")
  
  # Check if the dataframes exist
  if (exists(processed_df_name) && exists(unprocessed_df_name)) {
    
    # Select the required columns for processed and unprocessed data
    check_1_processed <- safe_select(get(processed_df_name), columns_to_select)
    check_1_unprocessed <- safe_select(get(unprocessed_df_name), columns_to_select)
    
    # Merge to form one dataset
    if ("p_id" %in% names(check_1_processed) && "p_id" %in% names(check_1_unprocessed)) {
      check_1 <- merge(check_1_processed, check_1_unprocessed, by = "p_id")
      
      # Reorder names of columns
      check_1 <- check_1[, order(names(check_1))]
      
      # Round numeric variables in the dataset
      check_1 <- check_1 %>%
        mutate(across(where(is.numeric), ~ round(., 0))) %>%
        mutate(across(where(is.character), ~ {
          numeric_val <- suppressWarnings(as.numeric(.))
          if (!all(is.na(numeric_val))) {
            round(numeric_val, 0)
          } else {
            .
          }
        }))
      
      # Calculate the absolute difference
      check_1 <- check_1 %>%
        mutate(across(ends_with(".x"), 
                      ~ abs(. - get(sub("\\.x$", ".y", cur_column()))), 
                      .names = "{.col}_absolute_difference"))
      
      # Reorder names of columns
      check_1 <- check_1[, order(names(check_1))]
      
      # Store the result in the list
      results[[year]] <- list(data = check_1)
    } else {
      warning(paste("Skipping year", year, ": 'p_id' column is missing in one or both dataframes"))
    }
  } else {
    warning(paste("Skipping year", year, ": One or both dataframes do not exist"))
  }
}


# Same-Same-Same (SSS) Testing - one duplicate, MP served two different constituencies in the same year
# Reason for absolute difference for 2019/20
dupes_set1 <- processed_data %>%
  group_by(p_id, year) %>%
  summarise(times_occured = n()) %>%
  filter(times_occured > 1) %>%
  ungroup() %>%
  mutate(t_dupes1 = row_number())

# Same-Same-Same (SSS) Testing - one duplicate, MP served two different constituencies in the same year
dupes_set2 <- processed_data %>%
  group_by(p_id, `m_ps_name`, year) %>%
  summarise(times_occured = n()) %>%
  filter(times_occured > 1) %>%
  ungroup() %>%
  mutate(t_dupes1 = row_number())

# Same-Same-Different (SSD) Testing - 0 - practically same test as before but omitting a variable
dupes_set3 <- processed_data %>%
  group_by(year, p_id) %>%
  summarise(unique_ids = n_distinct(p_id)) %>%
  filter(unique_ids > 1) %>%
  ungroup() %>%
  mutate(t_dupes2 = row_number())