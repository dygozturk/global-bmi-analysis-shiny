# 1. Load necessary libraries
library(tidyverse)  # Collection of packages for data manipulation and visualization
library(janitor)   # For data cleaning, especially column names
library(plotly)     # For interactive visualizations
library(ggplot2)    # For static visualizations

# 2. Read the dataset
bmi <- read_csv("data/bmi_dataset.csv", show_col_types = FALSE)

# 3. Clean column names and rename selected columns for clarity
bmi <- bmi %>%
  clean_names() %>%  # Standardize column names to lowercase and underscores
  rename(
    year        = year,
    sex         = sex,
    country     = country_region_world,
    obesity     = prevalence_of_bmi_30_kg_m2_obesity,
    underweight = prevalence_of_bmi_18_5_kg_m2_underweight
  )

# 4. Data type conversion and filtering
bmi <- bmi %>%
  mutate(
    year    = as.integer(year),
    sex     = as.factor(sex),
    country = as.factor(country),
    obesity = as.numeric(obesity),
    underweight = as.numeric(underweight)
  ) %>%
  filter(
    year >= 1990 & year <= as.integer(format(Sys.Date(), "%Y")),  # Include years from 1990 to current year
    between(obesity, 0, 1),       # Ensure obesity rates are between 0 and 1 (0% to 100%)
    between(underweight, 0, 1)    # Ensure underweight rates are between 0 and 1
  )

# 5. Check for missing values in each column
na_summary <- colSums(is.na(bmi))
print(na_summary)

# 6. Calculate average obesity rate per country
bmi_by_country <- bmi %>%
  filter(!is.na(obesity)) %>%
  group_by(country) %>%
  summarise(avg_obesity = mean(obesity, na.rm = TRUE)) %>%
  arrange(desc(avg_obesity))  # Sort by highest average obesity

head(bmi_by_country, 10)  # Display top 10 countries with highest obesity

# 7. Visualize average obesity rates by country (top-down bar chart)
ggplot(bmi_by_country, aes(x = reorder(country, avg_obesity), y = avg_obesity)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Average Obesity Rate by Country",
    x = "Country",
    y = "Average Obesity Rate"
  ) +
  theme_minimal()

# 8. Save the cleaned dataset
write_csv(bmi, "bmi_cleaned.csv")

# --- E. Static Visualizations using ggplot2 ---

# 1) Bar chart: Top 20 countries by average obesity
bmi_by_country <- bmi %>%
  group_by(country) %>%
  summarise(avg_obesity = mean(obesity, na.rm = TRUE)) %>%
  arrange(desc(avg_obesity))

ggplot(bmi_by_country %>% slice_max(avg_obesity, n = 20),
       aes(x = reorder(country, avg_obesity), y = avg_obesity)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Countries by Average Obesity Rate (1990b