# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

# Select the relevant columns
data_for_outlier_visualisation_2 <- data_for_visualisation %>%
  select(pID, Year, `Constituency`, `Office spend`, `Staffing spend`, `Accommodation spend`, 
         `Travel and subsistence spend (uncapped)`, `Other costs spend (uncapped)`, 
         `Subtotal of office running costs`, `Subtotal of other parliamentary costs`, 
         `Overall total spend for this financial year`)

# Calculate years of service for each MP
years_of_service <- data_for_outlier_visualisation_2 %>%
  group_by(pID) %>%
  summarise(YearsOfService = n_distinct(Year))

# Merge the years of service information with the main dataset
data_for_outlier_visualisation_2 <- data_for_outlier_visualisation_2 %>%
  left_join(years_of_service, by = "pID")

# Summarise the count of outliers by Constituency and Category
outlier_counts_2 <- outliers %>%
  group_by(Constituency, Category) %>%
  summarise(OutlierCount = n(), .groups = 'drop')

# Summarise the count of outliers by pID
outlier_counts_3 <- outliers %>%
  group_by(pID) %>%
  summarise(OutlierCount = n(), .groups = 'drop')

# Merge years of service onto outlier_counts_4
outlier_counts_4 <- outlier_counts_3 %>%
  left_join(years_of_service, by = "pID") %>%
  left_join(data_for_outlier_visualisation_2 %>% 
              distinct(pID, Constituency), by = "pID")

# Unique outlier counts
unique_outlier_counts_4 <- outlier_counts_4 %>% distinct(pID, .keep_all = TRUE)

# Merge the outlier counts with the main dataset for visualisation
data_for_visualisation <- data_for_outlier_visualisation_2 %>%
  left_join(outlier_counts_3, by = "pID") %>%
  mutate(OutlierCount = ifelse(is.na(OutlierCount), 0, OutlierCount))
