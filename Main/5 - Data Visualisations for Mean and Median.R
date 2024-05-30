# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)

# Select the relevant columns, using backticks for column names with spaces
data_for_mean_median_visualisation <- data_for_visualisation %>%
  select(Year, `Office spend`, `Staffing spend`, `Accommodation spend`, 
         `Travel and subsistence spend (uncapped)`, `Other costs spend (uncapped)`, 
         `Subtotal of office running costs`, `Subtotal of other parliamentary costs`, 
         `Overall total spend for this financial year`)

# Summary statistics
summary_stats <- data_for_mean_median_visualisation %>%
  group_by(Year) %>%
  summarise(across(where(is.numeric), 
                   list(Mean = ~mean(.[. != 0], na.rm = TRUE), 
                        Median = ~median(.[. != 0], na.rm = TRUE)), 
                   .names = "{col}_{fn}"))

# Convert the summary_stats to long format for easier plotting (bar charts)
summary_stats_long <- summary_stats %>%
  pivot_longer(cols = -Year, names_to = c("Category", "Average"), names_sep = "_") %>%
  mutate(Statistic = factor(Average, levels = c("Mean", "Median")))

# Convert the data to long format for easier plotting (box plots)
data_long <- data_for_mean_median_visualisation %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Value")

# Create bar plots for each category
plot_list_bar <- list()
categories <- unique(summary_stats_long$Category)

for (category in categories) {
  p <- ggplot(summary_stats_long %>% filter(Category == category), aes(x = Year, y = value, fill = Average)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = ifelse(Average == "Mean", round(value, 0), round(value, 2))), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              size = 3) +  # Remove diagonal rotation
    labs(title = category, y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) # Center the title
  
  plot_list_bar[[category]] <- p
}

# Display the bar plots
for (category in categories) {
  print(plot_list_bar[[category]])
}

# Create box plots for each category with upper bound outliers presented as red dots
plot_list_box_with_outliers <- list()

for (category in categories) {
  # Compute quartiles and IQR by Year and Category
  data_filtered <- data_long %>% filter(Category == category)
  data_stats <- data_filtered %>%
    group_by(Year) %>%
    summarise(Q3 = quantile(Value, 0.75),
              IQR = IQR(Value),
              UpperBound = Q3 + 1.5 * IQR)
  
  # Join the statistics back to the data
  data_with_bounds <- data_filtered %>%
    left_join(data_stats, by = "Year")
  
  # Identify outliers
  outliers <- data_with_bounds %>% filter(Value > UpperBound)
  
  p <- ggplot(data_filtered, aes(x = as.factor(Year), y = Value)) +
    geom_boxplot(color = "#2196F3", fill = "#BBDEFB") + # Adjust boxplot colors
    geom_point(data = outliers, aes(x = as.factor(Year), y = Value), color = "red", size = 2, alpha = 0.6) + # Present upper bound outliers as red dots
    labs(title = category, y = "Value", x = "Year", color = "Outliers") + # Rename legend title
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5), # Center the title
          panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), # Add grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          axis.title = element_text(size = 12), # Increase axis title font size
          axis.text = element_text(size = 10)) # Increase axis text font size
  
  plot_list_box_with_outliers[[category]] <- p
}

# Display the box plots with outliers
for (category in categories) {
  print(plot_list_box_with_outliers[[category]])
}
