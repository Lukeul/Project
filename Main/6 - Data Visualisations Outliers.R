# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

# Select the relevant columns, using backticks for column names with spaces
data_for_outlier_visualisation <- data_for_visualisation %>%
  select(pID, Year, `Constituency`, `Office spend`, `Staffing spend`, `Accommodation spend`, 
         `Travel and subsistence spend (uncapped)`, `Other costs spend (uncapped)`, 
         `Subtotal of office running costs`, `Subtotal of other parliamentary costs`, 
         `Overall total spend for this financial year`)

# Convert the data to long format for easier processing
data_long <- data_for_outlier_visualisation %>%
  pivot_longer(cols = -c(pID, Year, Constituency), names_to = "Category", values_to = "Value")

# Identify upper bound outliers for each category and year
outliers <- data_long %>%
  group_by(Category, Year) %>%
  mutate(Q3 = quantile(Value, 0.75),
         IQR = IQR(Value),
         UpperBound = Q3 + 1.5 * IQR) %>%
  filter(Value > UpperBound) %>%
  ungroup()

# Summarise the count of outliers by year and category
outlier_counts <- outliers %>%
  group_by(Year, Category) %>%
  summarise(OutlierCount = n(), .groups = 'drop')

# Create a plot for each category
categories_with_outliers <- unique(outlier_counts$Category)

plot_list <- list()

for (category in categories_with_outliers) {
  p <- ggplot(outlier_counts %>% filter(Category == category), aes(x = as.factor(Year), y = OutlierCount, fill = as.factor(Year))) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = OutlierCount), position = position_dodge(width = 0.9), vjust = -0.5, size = 3, color = "black") +
    labs(title = paste("Outlier Counts for Category:", category), y = "Outlier Count", x = "Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none") +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    guides(fill = "none") 
  
  plot_list[[category]] <- p
}

# Display the bar plots for each category
for (category in names(plot_list)) {
  print(plot_list[[category]])
}
