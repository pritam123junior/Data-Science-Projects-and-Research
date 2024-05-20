
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
