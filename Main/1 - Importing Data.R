# Load required libraries
library(dplyr)  
library(readr)  
library(purrr)  

# Initialise variables
start_year <- 10
end_year <- 11
all_data <- list()

# Function to construct the URL
construct_url <- function(start_year, end_year) {
  paste0("https://www.theipsa.org.uk/api/download?type=totalSpend&year=", 
         sprintf("%02d_%02d", start_year, end_year))
}

# Loop to read datasets
while (TRUE) {
  url <- construct_url(start_year, end_year)
  cat("Attempting to read data from:", url, "\n")
  
  # Try to read the CSV file
  tryCatch({
    year_label <- paste0("20", sprintf("%02d/%02d", start_year, end_year))
    data <- read_csv(url)
    
    # Add a new column with the year information
    data <- data %>%
      mutate(Year = year_label)
    
    # Store the data in the list
    all_data[[length(all_data) + 1]] <- data
    cat("Successfully read data for year:", start_year, "/", end_year, "\n")
    
    # Move to the next year
    start_year <- start_year + 1
    end_year <- end_year + 1
  }, error = function(e) {
    cat("No data found for year:", start_year, "/", end_year, "- stopping.\n")
    break
  })
}
