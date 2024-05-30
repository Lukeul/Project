# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Statistics Visualisations for Averages"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # No inputs required for this basic example
    ),
    
    # Show a plot based on the selected category
    mainPanel(
      selectInput("category", "Select Category:", choices = categories),
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Render the selected plot
  output$plot <- renderPlot({
    req(input$category)  # Ensure a category is selected
    
    # Get the selected plot from the list
    selected_plot <- plot_list[[input$category]]
    
    # Print the plot
    print(selected_plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Perform a correlation matrix and plot

# Select only numerical columns
numerical_data <- data_for_visualisation[sapply(data_for_visualisation, is.numeric)]

# Compute the correlation matrix
correlation_matrix <- cor(numerical_data, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Visualise the correlation matrix
corrplot(correlation_matrix, method = "circle")