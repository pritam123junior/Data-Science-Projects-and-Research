
library(rvest)
library(dplyr)
library(stringr)

url <- "http://books.toscrape.com/catalogue/category/books/travel_2/index.html"


book_data <- read_html(url) %>%
  html_nodes('.product_pod') %>%
  html_nodes('h3') %>%
  html_nodes('a') %>%
  html_attr('title') %>%
  data.frame(title = .) 

book_data$rating <- read_html(url) %>%
  html_nodes('.star-rating') %>%
  html_attr('class') %>%
  str_extract('One|Two|Three|Four|Five') 

book_data$price <- read_html(url) %>%
  html_nodes('.price_color') %>%
  html_text()

book_data$availability <- read_html(url) %>%
  html_nodes('.instock') %>%
  html_text()

book_data$book_url <- read_html(url) %>%
  html_nodes('.image_container a') %>%
  html_attr('href')

book_data$thumbnail_url <- read_html(url) %>%
  html_nodes('.image_container img') %>%
  html_attr('src') 

book_data$rating <- gsub("star-rating ", "", book_data$rating)
book_data$price <- gsub("Â£", "", book_data$price)
book_data$availability <- gsub("\r\n", "", book_data$availability)
book_data$availability <- trimws(book_data$availability)


write.csv(book_data, "book_data.csv", row.names = FALSE)

# Load necessary libraries
library(rvest)
library(dplyr)
library(httr)

# Initialize an empty data frame to store results
products <- data.frame()

# Define a user-agent string
user_agent_string <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"

# Loop through page numbers
for (page_number in 1:40) {
  # Construct the URL for each page
  link <- paste0("https://www.ozon.ru/category/sumki-zhenskie-17001/?page=", page_number)

  # Try to read the HTML content, handle potential errors
  tryCatch({
    # Use httr::GET to set the user-agent
    page <- GET(link, user_agent(user_agent_string))
    content <- content(page, as = "text")
    parsed_page <- read_html(content)

    # Extract product names
    product_names <- parsed_page %>% html_nodes(".tile-hover-target") %>% html_text(trim = TRUE)

    # Extract images URLs
    image_urls <- parsed_page %>% html_nodes(".b0v8 img") %>% html_attr("src")

    # Extract prices
    prices <- parsed_page %>% html_nodes(".ui-p5") %>% html_text(trim = TRUE)

    # Extract descriptions (if available)
    descriptions <- parsed_page %>% html_nodes(".d1w8") %>% html_text(trim = TRUE)

    # Assuming the category is "Women's Bags"
    category <- "Women's Bags"

    # Ensure all columns have the same length by padding with NA
    max_length <- max(length(product_names), length(image_urls), length(prices), length(descriptions))
    product_names <- c(product_names, rep(NA, max_length - length(product_names)))
    image_urls <- c(image_urls, rep(NA, max_length - length(image_urls)))
    prices <- c(prices, rep(NA, max_length - length(prices)))
    descriptions <- c(descriptions, rep(NA, max_length - length(descriptions)))

    # Combine extracted data into a data frame
    page_data <- data.frame(
      category = rep(category, max_length),
      product_name = product_names,
      image_url = image_urls,
      price = prices,
      description = descriptions,
      stringsAsFactors = FALSE
    )

    # Append the page data to the main data frame
    products <- rbind(products, page_data)

    # Print the current page number
    print(paste("Page:", page_number))

  }, error = function(e) {
    # Print an error message if the page fails to load
    print(paste("Error on page:", page_number, "-", conditionMessage(e)))
  })

  # Pause for a short time to avoid getting blocked
  Sys.sleep(runif(1, 2, 5))  # sleep for a random time between 2 and 5 seconds
}

# Write the collected data to a CSV file
write.csv(products, "products.csv", row.names = FALSE)

