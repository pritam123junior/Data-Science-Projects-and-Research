
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
library(rvest)
library(dplyr)

# Initialize an empty data frame to store results
mods <- data.frame()

# Loop through page numbers from 1 to 40
for (page_number in 1:40) {
  # Construct the URL for each page
  link <- paste0("https://www.nexusmods.com/residentevil22019/mods/?page=", page_number)
  # Read the HTML content of the page
  page <- read_html(link)

  # Extract the desired elements from the page
  name <- page %>% html_nodes(".tile-name a") %>% html_text(trim = TRUE)
  category <- page %>% html_nodes(".category a") %>% html_text(trim = TRUE)
  description <- page %>% html_nodes(".desc") %>% html_text(trim = TRUE)
  author <- page %>% html_nodes(".author a") %>% html_text(trim = TRUE)

  # Combine extracted data into a data frame
  page_data <- data.frame(name, category, description, author, stringsAsFactors = FALSE)

  # Append the page data to the main data frame
  mods <- rbind(mods, page_data)

  # Print the current page number
  print(paste("Page:", page_number))
}

# Write the collected data to a CSV file
write.csv(mods, "mods.csv", row.names = FALSE)
