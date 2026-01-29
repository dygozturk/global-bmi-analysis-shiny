# app.R
# Shiny Dashboard: Global Obesity Prevalence
# Topic: How does the average obesity (BMIb	%30) prevalence change by country?
# This application allows users to explore obesity trends by country and globally,
# View average obesity rates via a gauge chart, compare top 5 countries, and inspect raw data.

library(shiny)    # Web application framework for R
library(tidyverse) # Data manipulation (dplyr) and visualization (ggplot2)
library(janitor)   # Clean and standardize column names
library(plotly)    # Interactive plots
library(DT)        # Interactive data tables

# 1. Load and clean data ----
# Read the raw CSV file. Ensure the data folder contains 'bmi.csv'.
bmi <- read_csv("C:\\Users\\Lenovo\\Desktop\\CEN314_FinalProject_Group4_Altinisik_Bicen_Ozturk_Tuysuz\\CEN314_FinalProject_Group4_Altinisik_Bicen_Ozturk_Tuysuz\\data\\bmi.csv", show_col_types = FALSE) %>%
  clean_names() %>%    # Convert column names to snake_case for easier handling
  rename(
    country = country_region_world,                    # Rename for clarity
    year    = year,                                    # Ensure consistency
    sex     = sex,                                     # Male/Female categories
    obesity = prevalence_of_bmi_30_kg_m2_obesity       # Focus on obesity prevalence
  ) %>%
  mutate(
    country = as.factor(country),                     # Treat country as categorical
    sex     = as.factor(sex),                         # Treat sex as categorical
    year    = as.integer(year),                       # Ensure year is integer
    obesity = as.numeric(obesity)                     # Ensure obesity is numeric percentage
  ) %>%
  filter(!is.na(obesity) & obesity >= 0 & obesity <= 100) # Remove missing or implausible values

# 2. Precompute summary tables for performance ----
# Compute average obesity by country over all years
bmi_by_country <- bmi %>%
  group_by(country) %>%
  summarise(avg_obesity = mean(obesity), .groups = 'drop') %>%
  arrange(desc(avg_obesity))                         # Sort descending for display

# Identify top 5 countries by average obesity for comparison
top5_countries <- bmi_by_country %>% slice_max(avg_obesity, n = 5) %>% pull(country)

# Prepare time series data for top 5 countries
trend_top5 <- bmi %>%
  filter(country %in% top5_countries) %>%
  group_by(country, year) %>%
  summarise(mean_obesity = mean(obesity), .groups = 'drop')

# Compute global average obesity trend by year
global_trend <- bmi %>%
  group_by(year) %>%
  summarise(global_obesity = mean(obesity), .groups = 'drop')

# 3. Define UI ----
ui <- fluidPage(
  titlePanel("Global Obesity Dashboard"),            # App title
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select a single country
      selectInput("country", "Select a country:",
                  choices = sort(unique(bmi$country)),
                  selected = top5_countries[1]),
      # Slider to filter the year range for time series plots
      sliderInput("yearRange", "Select Year Range:",
                  min = min(bmi$year), max = max(bmi$year),
                  value = c(min(bmi$year), max(bmi$year)), sep = ""),
      # Checkbox to toggle raw data table visibility
      checkboxInput("showData", "Show raw data table", FALSE)
    ),
    mainPanel(
      # Tabset panel to organize multiple views
      tabsetPanel(
        tabPanel("Country Trend", plotlyOutput("trendPlot")),  # Line plot by country
        tabPanel("Avg Obesity", plotlyOutput("barPlot")),     # Gauge chart for average
        tabPanel("Top 5 Trends", plotlyOutput("top5Plot")),   # Compare top5 countries
        tabPanel("Global Trend", plotlyOutput("globalPlot")), # Global trend line
        tabPanel("Insights", htmlOutput("insights")),         # Narrative insights
        tabPanel("Raw Data", DTOutput("dataTable"))          # Raw data table
      )
    )
  )
)

# 4. Define server logic ----
server <- function(input, output, session) {
  # Reactive: filter data for selected country and year range
  country_data <- reactive({
    bmi %>%
      filter(country == input$country,
             year >= input$yearRange[1], year <= input$yearRange[2])
  })
  
  # 4a. Country-specific obesity trend over time
  output$trendPlot <- renderPlotly({
    p <- ggplot(country_data(), aes(x = year, y = obesity)) +
      geom_line(color = 'darkblue') +
      labs(title = paste(input$country, "Obesity Trend"),
           x = "Year", y = "Obesity Rate (%)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # 4b. Gauge chart showing average obesity for selected country
  output$barPlot <- renderPlotly({
    avg_val <- country_data() %>% summarise(avg = mean(obesity)) %>% pull(avg) * 100
    plot_ly(
      type = "indicator", mode = "gauge+number", value = avg_val,
      title = list(text = paste("Average Obesity in", input$country)),
      gauge = list(
        axis = list(range = c(0,100)),            # 0-100% scale
        bar = list(color = "darkorange"),        # Indicator bar color
        steps = list(                             # Background segments (optional)
          list(range = c(0,20)),
          list(range = c(20,40)),
          list(range = c(40,60)),
          list(range = c(60,80)),
          list(range = c(80,100))
        )
      )
    )
  })
  
  # 4c. Trend comparison for top 5 countries
  output$top5Plot <- renderPlotly({
    p3 <- ggplot(trend_top5, aes(x = year, y = mean_obesity, color = country)) +
      geom_line(size = 1) +
      labs(title = "Top 5 Countries Obesity Trend",
           x = "Year", y = "Avg Obesity (%)") +
      theme_minimal()
    ggplotly(p3)
  })
  
  # 4d. Global average obesity trend
  output$globalPlot <- renderPlotly({
    p4 <- ggplot(global_trend, aes(x = year, y = global_obesity)) +
      geom_line(size = 1) +
      labs(title = "Global Average Obesity Over Time",
           x = "Year", y = "Avg Obesity (%)") +
      theme_minimal()
    ggplotly(p4)
  })
  
  # 4e. Narrative insights section
  output$insights <- renderUI({
    avg_all <- bmi_by_country %>% summarise(m = mean(avg_obesity)) %>% pull(m)
    tagList(
      tags$h4("Key Insights"),
      tags$p(sprintf("Global average obesity (1975-2016) is %.1f%%.", avg_all*100)),
      tags$p("Top 5 countries show the highest obesity prevalence, indicating geographic disparities."),
      tags$p("Use 'Country Trend' to analyze yearly changes within a specific country."),
      tags$p("Adjust the year range slider to focus on particular time periods.")
    )
  })
  
  # 4f. Raw data table display
  output$dataTable <- renderDT({
    if (input$showData) {
      datatable(country_data(), options = list(pageLength = 10))
    }
  })
}

# 5. Launch the application ----
shinyApp(ui, server)
