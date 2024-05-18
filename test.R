
## Rosebike

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(purrr)

extract_price <- function(text) {
  price_str <- str_extract(text, "[0-9]*,*[0-9]*\\.[0-9]*")
  price_str <- gsub(",", "", as.character(price_str))
  return(price_str)
}

make_url <- function(my_string){
  base_url <- "https://www.rosebikes.com"
  glue("{base_url}{my_string}")
}

#all model names have numbers in them
filter_model_names <- function(model_name) {
  condition1 <- str_detect(model_name, "[0-9]")
  condition2 <- str_detect(model_name, "SOUL FIRE")
  if (condition1 | condition2) {
    return(model_name)
  }
  return(NULL)
}

extract_models <- function(model_url) {
  
  css_class_model_name <- ".basic-headline__title"
  # This part extracts the model names
  html_model_names <- read_html(model_url) %>%
    html_nodes(css = css_class_model_name) %>%
    map(html_text) %>%
    map(filter_model_names)
  model_names <- unique(unlist(html_model_names))
  
  
  #This parts extracts the model price
  css_class_model_price = ".catalog-category-model__price-current-value"
  model_prices <- read_html(model_url) %>%
    html_nodes(css = css_class_model_price) %>%
    map(html_text) %>%
    map(extract_price)
  
  #The last prices are the prices of the models
  while (length(model_prices) > length(model_names)) {
    model_prices[1] <- NULL
  }
  model_prices <- model_prices %>%
    unlist() %>%
    as.double()
  
  models_tbl <- tibble(model_names, model_prices)
  
}


extract_models_from_category_list_mtb <- function(bike_url){
  
  bike_price_xml_path = "catalog-category-bikes__price-title"
  category <- str_extract(bike_url, "(?<=/)[a-z]*$")
  css_class <- ".catalog-category-bikes__button"
  
  html_bike <- read_html(bike_url)
  
  html_models_urls <- html_bike %>%
    html_nodes(css = css_class) %>%
    html_attr("href") %>%
    map(make_url)
  
  models_in_category_list <- map(html_models_urls, extract_models)
  models_in_category_list
  models_in_category_tbl <- models_in_category_list[[1]]
  for (i in 2:length(models_in_category_list)) {
    models_in_category_tbl <- add_row(models_in_category_tbl, 
                                      model_names = models_in_category_list[[i]]$model_names,
                                      model_prices = models_in_category_list[[i]]$model_prices)
  }
  
  category = rep(category, times = length(models_in_category_tbl$model_names))
  models_in_category_tbl$category <- category
  return(models_in_category_tbl)
}


home_url <- "https://www.rosebikes.com/bikes"

html_home <- read_html(home_url)
html_categories_urls <- html_home %>%
  html_nodes(css = ".catalog-navigation__link") %>%
  html_attr("href") %>%
  map(make_url)


tbl <- html_categories_urls[[1]] %>%
 extract_models_from_category_list_mtb
tbl 
