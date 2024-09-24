download_packages <- function() {
  
  install.packages("ggplot2")
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("gridExtra")
  
}

load_packages <- function() {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(gridExtra)
  
}

load_dataset <- function(path) {
  
  amzn <- read.csv(path)
  return(amzn)
  
}

display_column_types <- function(amzn) {
  
  classes <- lapply(amzn, class)
  class_vector <- unlist(classes)
  class_counts <- table(class_vector)

  pie(class_counts, labels=names(class_counts), main="Data Frame Classes")
  
}

select_features <- function(amzn) {
  
  selected_features = c(
    "product_id",
    "discounted_price", #double
    "actual_price", #double
    "discount_percentage", #double
    "rating", #double
    "rating_count", #integer
    "about_product"
  )
  
  amzn <- amzn[selected_features]
  
  return(amzn)
  
}

clean_dataset <- function(amzn) {
  
  amzn$discounted_price <- gsub("₹", "", amzn$discounted_price)
  amzn$discounted_price <- gsub(",", "", amzn$discounted_price)
  
  amzn$actual_price <- gsub("₹", "", amzn$actual_price)
  amzn$actual_price <- gsub(",", "", amzn$actual_price)
  
  amzn$discount_percentage <- gsub("%", "", amzn$discount_percentage)
  
  amzn$rating <-gsub(",", "", amzn$rating)
  
  amzn$rating_count <- gsub(",", "", amzn$rating_count)
  
  amzn$discounted_price <- as.double(amzn$discounted_price)
  amzn$actual_price <- as.double(amzn$actual_price)
  amzn$discount_percentage <- as.integer(amzn$discount_percentage)
  amzn$rating <- as.numeric(amzn$rating)
  amzn$rating_count <- as.integer(amzn$rating_count)
  
  amzn$rating <- replace_na(amzn$rating, 0.0)
  amzn$rating_count <- replace_na(amzn$rating_count, 0)
  
  cat("The data contains NA values:", any(is.na(amzn)))
  
  return(amzn)
  
}

feature_engineering <- function(amzn, rupee_rate = 93) {

  amzn$discount_percentage = amzn$discount_percentage / 100
  
  amzn$discounted_price = amzn$discounted_price / rupee_rate
  amzn$actual_price = amzn$actual_price / rupee_rate
  
  return(amzn)
  
}

probability_density_function <- function(data, column, label) {
  
  mean_value <- mean(column)
  
  ggplot(data, aes(x = column)) +
    geom_density(fill = "black", alpha = 1) +
    geom_vline(aes(xintercept=mean_value), color="red", linetype="dashed") +
    labs(title = label) +
    theme_dark()
  
}

shapiro_wilk_test <- function(data) {
  
  print(shapiro.test(data))
  
}

pearson_correlation <- function(data) {
  
  cor.test()
  
}

