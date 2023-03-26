# Load the required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)

# Load the dataset
airbnb_seattle <- read_csv("path/to/dataset.csv")

# Data cleaning
airbnb_seattle_cleaned <- airbnb_seattle %>% 
  select(-c(host_name, last_review, reviews_per_month)) %>% 
  mutate(price = as.numeric(gsub("[$,]", "", price))) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_at(vars(bedrooms, beds), as.integer) %>% 
  drop_na()

  # EDA
ggplot(airbnb_seattle_cleaned, aes(x = price)) + 
  geom_histogram(fill = "#1E90FF", bins = 50) +
  ggtitle("Distribution of Prices") +
  labs(x = "Price (in USD)", y = "Frequency")

ggplot(airbnb_seattle_cleaned, aes(x = neighbourhood_group, y = price)) + 
  geom_boxplot(fill = "#1E90FF") +
  ggtitle("Price Distribution by Neighborhood Group") +
  labs(x = "Neighborhood Group", y = "Price (in USD)")

ggplot(airbnb_seattle_cleaned, aes(x = room_type, y = price)) + 
  geom_boxplot(fill = "#1E90FF") +
  ggtitle("Price Distribution by Room Type") +
  labs(x = "Room Type", y = "Price (in USD)")


# Regression analysis
model <- lm(price ~ accommodates + bathrooms + bedrooms + beds + guests_included + review_scores_rating, data = airbnb_seattle_cleaned)
summary(model)

# Correlation analysis
correlations <- cor(airbnb_seattle_cleaned[,c("price", "accommodates", "bathrooms", "bedrooms", "beds", "guests_included", "review_scores_rating")])
correlations

# Hypothesis testing
t.test(airbnb_seattle_cleaned$price ~ airbnb_seattle_cleaned$room_type)

# Make interactive dashboard


# Load the required libraries
library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  
  # Title
  titlePanel("Airbnb Seattle Analysis"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Input controls
      selectInput(inputId = "room_type", label = "Select Room Type:", choices = c("Entire home/apt", "Private room", "Shared room")),
      sliderInput(inputId = "price", label = "Select Price Range:", min = 0, max = 1000, value = c(0, 500))
    ),
    
    # Output plot
    mainPanel(
      plotOutput(outputId = "price_plot")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Filter the data based on user inputs
  filtered_data <- reactive({
    airbnb_seattle_cleaned %>%
      filter(room_type == input$room_type, price >= input$price[1], price <= input$price[2])
  })
  
  # Render the plot based on the filtered data
  output$price_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = accommodates, y = price)) + 
      geom_point(color = "#1E90FF") +
      ggtitle("Price vs Accommodates") +
      labs(x = "Accommodates", y = "Price (in USD)")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
