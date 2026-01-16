# COVID-19 Testing Data Analysis
# This script retrieves COVID-19 testing data via web scraping,
# cleans the dataset, and exports a processed CSV file.
#
# Tools: R, httr, rvest

# Load required libraries
library(httr)
library(rvest)

# Function to retrieve the Wikipedia COVID-19 testing page
get_wiki_covid19_page <- function() {
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  query_params <- list(title = "Template:COVID-19_testing_by_country")
  response <- GET(wiki_base_url, query = query_params)
  return(response)
}

# Retrieve web page
response <- get_wiki_covid19_page()

# Parse HTML content
root_node <- read_html(response)

# Extract tables from the page
table_nodes <- html_nodes(root_node, "table")

# Select the main COVID-19 testing table
covid_table <- html_table(table_nodes[[2]])
covid_table <- as.data.frame(covid_table)

# Function to clean and preprocess the data
preprocess_covid_data_frame <- function(df) {

  # Remove aggregated or irrelevant rows
  df <- df[!(df$`Country or region` == "World"), ]
  df <- df[1:172, ]

  # Remove unnecessary columns
  df["Ref."] <- NULL
  df["Units[b]"] <- NULL

  # Rename columns for clarity
  names(df) <- c(
    "country",
    "date",
    "tested",
    "confirmed",
    "confirmed_tested_ratio",
    "tested_population_ratio",
    "confirmed_population_ratio"
  )

  # Convert character columns to numeric
  df$tested <- as.numeric(gsub(",", "", df$tested))
  df$confirmed <- as.numeric(gsub(",", "", df$confirmed))
  df$confirmed_tested_ratio <- as.numeric(df$confirmed_tested_ratio)
  df$tested_population_ratio <- as.numeric(df$tested_population_ratio)
  df$confirmed_population_ratio <- as.numeric(df$confirmed_population_ratio)

  return(df)
}

# Apply preprocessing
clean_covid_data <- preprocess_covid_data_frame(covid_table)

# Export cleaned data to CSV
write.csv(clean_covid_data, "covid.csv", row.names = FALSE)

