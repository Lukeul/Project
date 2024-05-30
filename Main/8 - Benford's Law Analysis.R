library(dplyr)
library(ggplot2)
library(cowplot)


# Function to calculate Benford's Law probabilities
benford_probabilities <- function(digit) {
  log10(1 + 1 / digit)
}

# Filter 0's

cleaned_data_benfords_analysis <- data_for_visualisation %>%
  filter(!is.na(`Travel and subsistence spend (uncapped)`),
         `Travel and subsistence spend (uncapped)` > 0,  
         !is.na(`Other costs spend (uncapped)`),
         `Other costs spend (uncapped)` > 0)  

# Check if data spans multiple orders of magnitude for Benford's Law assumption
mag_range <- cleaned_data_benfords_analysis %>%
  summarise(min_magnitude = log10(min(abs(`Travel and subsistence spend (uncapped)`))),
            max_magnitude = log10(max(abs(`Travel and subsistence spend (uncapped)`))))

if (diff(unlist(mag_range)) > 1) {
  print("Data spans multiple orders of magnitude - Benford's Law assumption is met.")
} else {
  print("Data does not span multiple orders of magnitude - Benford's Law assumption may not be fully met.")
}

mag_range <- cleaned_data_benfords_analysis %>%
  summarise(min_magnitude = log10(min(abs(`Other costs spend (uncapped)`))),
            max_magnitude = log10(max(abs(`Other costs spend (uncapped)`))))

if (diff(unlist(mag_range)) > 1) {
  print("Data spans multiple orders of magnitude - Benford's Law assumption is met.")
} else {
  print("Data does not span multiple orders of magnitude - Benford's Law assumption may not be fully met.")
}

# Benford's Law curve data
benford_curve <- data.frame(
  digit = 1:9,
  prob = benford_probabilities(1:9)
)

# Plot for Benford curve - Travel and subsistence spend
cleaned_data_benfords_analysis %>%
  mutate(digit = as.numeric(substr(`Travel and subsistence spend (uncapped)`, 1, 1))) %>%
  group_by(digit) %>%
  summarise(times_occurred = n()) %>%
  mutate(prop = times_occurred / sum(times_occurred)) %>%
  ggplot(aes(x = as.factor(digit), y = prop)) +
  geom_col(color = "black", fill = "lightblue") +
  geom_line(data = benford_curve, aes(x = as.factor(digit), y = prob, group = 1), color = 'red') + 
  geom_point(data = benford_curve, aes(x = as.factor(digit), y = prob), color = 'red') +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%"), y = prop), vjust = -0.5, size = 3) +  # Add percentage labels
  labs(title = "Benford's Law Analysis - Travel and Subsistence Spend",
       x = "Digit",
       y = "Probability") +
  theme_minimal()

# Plot for Benford curve - Other costs spend
cleaned_data_benfords_analysis %>%
  mutate(digit = as.numeric(substr(`Other costs spend (uncapped)`, 1, 1))) %>%
  group_by(digit) %>%
  summarise(times_occurred = n()) %>%
  mutate(prop = times_occurred / sum(times_occurred)) %>%
  ggplot(aes(x = as.factor(digit))) +
  geom_col(aes(y = prop), color = "black", fill = "lightblue") +
  geom_line(data = benford_curve, aes(x = as.factor(digit), y = prob, group = 1), color = 'red') + 
  geom_point(data = benford_curve, aes(x = as.factor(digit), y = prob), color = 'red') +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%"), y = prop), vjust = -0.5, size = 3) +  # Add percentage labels
  labs(title = "Benford's Law Analysis - Other Costs Spend",
       x = "Digit",
       y = "Probability") +
  theme_minimal()