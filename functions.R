# a source file with utility functions

# download packages
download_packages <- function() {
  
  install.packages("ggplot2")
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("gridExtra")
  
}

# attach packages
load_packages <- function() {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(gridExtra)
  library(stats)
  
}

# load the dataset
load_dataset <- function(path) {
  
  amzn <- read.csv(path)
  return(amzn)
  
}

# create a pie chart with types
display_column_types <- function(amzn) {
  
  classes <- lapply(amzn, class)
  class_vector <- unlist(classes)
  class_counts <- table(class_vector)

  pie(class_counts, labels=names(class_counts), main="Data Frame Classes")
  
}

# reassign selected features to the dataset
select_features <- function(amzn) {
  
  selected_features = c(
    "product_id",
    "discounted_price",
    "actual_price",
    "discount_percentage",
    "rating",
    "rating_count",
    "about_product"
  )
  
  amzn <- amzn[selected_features]
  return(amzn)
  
}

# check for NA values, please do not interpret it as NaN or Inf values
check_na_values <- function(data) {
  
  na_values <- any(is.na(data))
  cat("The data contains NA values:", na_values, "\n")
  return(na_values)
  
}

# function that cleans the whole Amazon dataset
clean_dataset <- function(amzn) {
  
  
  amzn$discounted_price <- gsub("₹", "", amzn$discounted_price)
  amzn$discounted_price <- gsub(",", "", amzn$discounted_price)
  
  amzn$actual_price <- gsub("₹", "", amzn$actual_price)
  amzn$actual_price <- gsub(",", "", amzn$actual_price)
  
  amzn$discount_percentage <- gsub("%", "", amzn$discount_percentage)
  
  amzn$rating <-gsub(",", "", amzn$rating)
  
  amzn$rating_count <- gsub(",", "", amzn$rating_count)
  
  amzn$discounted_price <- as.numeric(amzn$discounted_price)
  amzn$actual_price <- as.numeric(amzn$actual_price)
  amzn$discount_percentage <- as.numeric(amzn$discount_percentage)
  amzn$rating <- as.numeric(amzn$rating)
  amzn$rating_count <- as.integer(amzn$rating_count)
  
  amzn$discounted_price <- replace_na(amzn$discounted_price, 0.0)
  amzn$actual_price <- replace_na(amzn$actual_price, 0.0)
  amzn$discount_percentage <- replace_na(amzn$discount_percentage, 0.0)
  amzn$rating <- replace_na(amzn$rating, 0.0)
  amzn$rating_count <- replace_na(amzn$rating_count, 0)
  
  amzn$rating <- as.numeric(amzn$rating)
  
  check_na_values(amzn)
  
  return(amzn)
  
}

# adapt the features for further statistical analysis
feature_engineering <- function(amzn, rupee_rate = 93) {

  amzn$discount_percentage = amzn$discount_percentage / 100
  
  amzn$discounted_price = amzn$discounted_price / rupee_rate
  amzn$actual_price = amzn$actual_price / rupee_rate
  
  return(amzn)
  
}

# plot PDF
probability_density_function <- function(data, column, label) {
  
  mean_value <- mean(column)
  
  ggplot(data, aes(x = column)) +
    geom_density(fill = "black", alpha = 1) +
    geom_vline(aes(xintercept=mean_value), color="red", linetype="dashed") +
    labs(title = label) +
    theme_dark()
  
}
