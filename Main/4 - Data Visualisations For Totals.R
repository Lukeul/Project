# Load required libraries
library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

# Renaming and reorder columns
data_for_visualisation <- processed_data %>% rename("pID" = p_id, 
                                  "MP's name" = m_ps_name, 
                                  "Constituency" = constituency, "Year" = year,
                                  "Office budget" = office_budget, 
                                  "Office spend" = office_spend, 
                                  "Remaining office budget" = remaining_office_budget,
                                  "Staffing budget" = staffing_budget, 
                                  "Staffing spend" = staffing_spend,
                                  "Remaining staffing budget" = remaining_staffing_budget, 
                                  "Start-up budget" = start_up_maximum_budget_available, 
                                  "Start-up spend" = start_up_spend, 
                                  "Remaining start-up budget" = remaining_start_up_budget,
                                  "Wind-up budget" = wind_up_budget, 
                                  "Wind-up spend" = wind_up_spend,
                                  "Remaining wind-up budget" = remaining_wind_up_budget, 
                                  "Subtotal of office running costs" = subtotal_of_office_running_costs,
                                  "Accommodation budget" = accommodation_budget,
                                  "Accommodation spend" = accommodation_spend, 
                                  "Remaining accommodation budget" = remaining_accommodation_budget,
                                  "Travel and subsistence spend (uncapped)" = travel_and_subsistence_uncapped, 
                                  "Other costs spend (uncapped)" = other_costs_uncapped,
                                  "Subtotal of other parliamentary costs" = subtotal_of_other_parliamentary_costs, 
                                  "Overall total spend for this financial year" = overall_total_spend_for_this_financial_year) %>%
                            select("pID", 
                                   "MP's name", 
                                   "Constituency", 
                                   "Year",
                                   "Office budget", 
                                   "Office spend", 
                                   "Remaining office budget",
                                   "Staffing budget", 
                                   "Staffing spend",
                                   "Remaining staffing budget", 
                                   "Start-up budget", 
                                   "Start-up spend", 
                                   "Remaining start-up budget",
                                   "Wind-up budget", 
                                   "Wind-up spend",
                                   "Remaining wind-up budget", 
                                   "Accommodation budget",
                                   "Accommodation spend", 
                                   "Remaining accommodation budget",
                                   "Travel and subsistence spend (uncapped)", 
                                   "Other costs spend (uncapped)",
                                   "Subtotal of office running costs",
                                   "Subtotal of other parliamentary costs", 
                                   "Overall total spend for this financial year")

# VISUALISAITON 1 - OVERALL TOTAL WITH 3-YEAR FORECAST

# Renames column
data_for_visualisation <- data_for_visualisation %>%
  mutate(
    `Year` = sprintf("%s/%s", substr(Year, 1, 4), substr(Year, 6, 7))
  )

# Summarise the data to calculate totals by year
summary_total_spend <- data_for_visualisation %>%
  group_by(Year) %>%
  summarise(total_spend = sum(`Overall total spend for this financial year`)) %>%
  arrange(Year)

# Prepare the data for forecasting
ts_data <- ts(na.omit(summary_total_spend$total_spend), start = c(2010, 1), frequency = 1)

# Fit a forecasting model
fit <- auto.arima(ts_data)

# Forecast the next 3 years
forecast_data <- forecast(fit, h = 3)

# Prepare forecasted data for plotting
forecast_years <- seq(2010 + length(summary_total_spend$Year), length.out = 3)
forecast_df <- data.frame(Year = sprintf("%d/%d", forecast_years, forecast_years + 1), total_spend = forecast_data$mean)

# Combine historical and forecasted data
combined_data <- bind_rows(summary_total_spend, forecast_df)

# Calculate percentage change for the combined data
combined_data <- combined_data %>%
  arrange(Year) %>%
  mutate(perc_change = (total_spend - lag(total_spend)) / lag(total_spend) * 100)

# Create the bar plot with a line superimposed on top, including forecasted data
ggplot(combined_data, aes(x = Year, y = total_spend)) +
  geom_bar(stat = "identity", fill = ifelse(combined_data$Year <= max(summary_total_spend$Year), "skyblue", "orange")) +
  geom_line(aes(group = 1), color = "red", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  geom_text(aes(label = ifelse(is.na(perc_change), "", sprintf("%.1f%%", perc_change))), 
            vjust = -0.5, size = 3.5) +
  labs(title = "Overall Total Spend by Year
  3-Year Forecast",
       x = "Year",
       y = "Total Spend") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# VISUALISAITON 2 & 3 - OTHER PALIAMENTARY COSTS & OFFICE RUNNING COSTS (NO FORECAST)

# Summary statistics
summary_other_parliamentary <- data_for_visualisation %>%
  group_by(Year) %>%
  summarise(subtotal_other_parliamentary = sum(`Subtotal of other parliamentary costs`)) %>%
  arrange(Year)

summary_office_running_costs <- data_for_visualisation %>%
  group_by(Year) %>%
  summarise(subtotal_of_office_running_costs = sum(`Subtotal of office running costs`)) %>%
  arrange(Year)

# Calculate percentage change
summary_other_parliamentary_combined <- summary_other_parliamentary %>%
  arrange(Year) %>%
  mutate(perc_change = (subtotal_other_parliamentary - lag(subtotal_other_parliamentary)) / lag(subtotal_other_parliamentary) * 100)

summary_office_running_costs_combined <- summary_office_running_costs %>%
  arrange(Year) %>%
  mutate(perc_change = (subtotal_of_office_running_costs - lag(subtotal_of_office_running_costs)) / lag(subtotal_of_office_running_costs) * 100)

# Data visualisation for "Subtotal of Other Parliamentary Costs" with percentage change
ggplot(summary_other_parliamentary_combined, aes(x = Year, y = subtotal_other_parliamentary)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(group = 1), color = "red", size = 1) +  
  geom_point(aes(group = 1), color = "red", size = 2) +  
  geom_text(aes(label = ifelse(is.na(perc_change), "", sprintf("%.1f%%", perc_change))), 
            vjust = -0.5, size = 3.5, color = "black", hjust = 0.5, angle = 0) +  
  labs(title = "Subtotal of Other Parliamentary 
Costs by Year",
       x = "Year",
       y = "Subtotal of Other Parliamentary Costs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Data visualisation for "Subtotal of Office Running Costs" with percentage change
ggplot(summary_office_running_costs_combined, aes(x = Year, y = subtotal_of_office_running_costs)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(group = 1), color = "red", size = 1) +  
  geom_point(aes(group = 1), color = "red", size = 2) +  
  geom_text(aes(label = ifelse(is.na(perc_change), "", sprintf("%.1f%%", perc_change))), 
            vjust = -0.5, size = 3.5, color = "black", hjust = 0.5, angle = 0) +  
  labs(title = "Subtotal of Office Running 
Costs by Year",
       x = "Year",
       y = "Subtotal of Office Running Costs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# DATA VISUALISATIONS 4,5 & 6 - BUDGET vS SPEND (ACCOMMODATION, OFFICE, AND STAFFING - FILTER FOR YEARS AFTER 2013/14)

# Select variables 
data_for_visualisation_budget <- data_for_visualisation %>%
  select(
    "Year",
    "Office budget",
    "Office spend",
    "Staffing budget",
    "Staffing spend",
    "Accommodation budget",
    "Accommodation spend"
  )

# Filter data for years 2013/14 onwards
data_for_visualisation_budget <- data_for_visualisation_budget %>%
  filter(Year >= "2013/14")

# Summarise the data to calculate totals by year
summary_office_budget_spend <- data_for_visualisation_budget %>%
  group_by(Year) %>%
  summarise(office_budget = sum(`Office budget`),
            office_spend = sum(`Office spend`)) %>%
  arrange(Year)

summary_staffing_budget_spend <- data_for_visualisation_budget %>%
  group_by(Year) %>%
  summarise(staffing_budget = sum(`Staffing budget`),
            staffing_spend = sum(`Staffing spend`)) %>%
  arrange(Year)

summary_accommodation_budget_spend <- data_for_visualisation_budget %>%
  group_by(Year) %>%
  summarise(accommodation_budget = sum(`Accommodation budget`),
            accommodation_spend = sum(`Accommodation spend`)) %>%
  arrange(Year)

# Plot for Staffing
ggplot(summary_staffing_budget_spend, aes(x = Year)) +
  geom_bar(aes(y = staffing_budget, fill = "Budget"), stat = "identity", alpha = 0.5) +
  geom_bar(aes(y = staffing_spend, fill = "Spend"), stat = "identity", alpha = 0.5) +
  geom_text(aes(y = staffing_budget, label = staffing_budget), vjust = -0.5, color = "black", size = 3) +
  geom_text(aes(y = staffing_spend, label = staffing_spend), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Staffing Budget vs. Spend", y = "Amount", x = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "Legend", values = c("Budget" = "blue", "Spend" = "red"))

# Plot for Office
ggplot(summary_office_budget_spend, aes(x = Year)) +
  geom_bar(aes(y = office_budget, fill = "Budget"), stat = "identity", alpha = 0.5) +
  geom_bar(aes(y = office_spend, fill = "Spend"), stat = "identity", alpha = 0.5) +
  geom_text(aes(y = office_budget, label = office_budget), vjust = -0.5, color = "black", size = 3) +
  geom_text(aes(y = office_spend, label = office_spend), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Office Budget vs. Spend", y = "Amount", x = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "Legend", values = c("Budget" = "blue", "Spend" = "red"))

# Plot for Accommodation
ggplot(summary_accommodation_budget_spend, aes(x = Year)) +
  geom_bar(aes(y = accommodation_budget, fill = "Budget"), stat = "identity", alpha = 0.5) +
  geom_bar(aes(y = accommodation_spend, fill = "Spend"), stat = "identity", alpha = 0.5) +
  geom_text(aes(y = accommodation_budget, label = accommodation_budget), vjust = -0.5, color = "black", size = 3) +
  geom_text(aes(y = accommodation_spend, label = accommodation_spend), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Accommodation Budget vs. Spend", y = "Amount", x = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "Legend", values = c("Budget" = "blue", "Spend" = "red"))