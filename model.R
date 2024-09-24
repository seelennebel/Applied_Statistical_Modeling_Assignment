# dataset URL
# https://www.kaggle.com/datasets/karkavelrajaj/amazon-sales-dataset

source("/Users/seelennebel/development/Applied_Statistical_Modeling_Assignment/functions.R")

#download_packages()

load_packages()

amzn <- load_dataset("/Users/seelennebel/development/Applied_Statistical_Modeling_Assignment/amazon.csv")

display_column_types(amzn)

amzn <- select_features(amzn)

amzn <- clean_dataset(amzn)

amzn <- feature_engineering(amzn)

check_na_values(amzn)

display_column_types(amzn)

# display PDFs of all numerical and integer features
{
  discounted_price_PDF <- probability_density_function(amzn, amzn$discounted_price, "PDF of discounted_price")
  actual_price_PDF <- probability_density_function(amzn, amzn$actual_price, "PDF of actual_price")
  discount_percentage_PDF <- probability_density_function(amzn, amzn$discount_percentage, "PDF of discount_percentage")
  rating_PDF <- probability_density_function(amzn, amzn$rating, "PDF of discounted_price")
  rating_count_PDF <- probability_density_function(amzn, amzn$rating_count, "PDF of rating_count")

  grid.arrange(discounted_price_PDF, actual_price_PDF, discount_percentage_PDF, rating_PDF, rating_count_PDF)
}

# create a classified amzn dataset
{
  classified_amzn <- mutate(amzn,
    discount_percentage_category = case_when(
      amzn$discount_percentage == 0 ~ "0%",
      amzn$discount_percentage <= 0.2 ~ "<20%",
      amzn$discount_percentage <= 0.4 ~ "<40%",
      amzn$discount_percentage <= 0.6 ~ "<60%",
      amzn$discount_percentage <= 0.8 ~ "<80%",
      amzn$discount_percentage <= 1 ~ "<100%"
      )
    )
}

check_na_values(classified_amzn)

# create a bar plot with category counts
{
  category_counts <- classified_amzn %>%
    count(discount_percentage_category)
  
  ggplot(category_counts, aes(x = discount_percentage_category, y = n)) +
    geom_bar(stat = "identity", fill = "black", color = "black") +
    theme_minimal() +
    labs(title = "Count of Each discount_percentage category", x = "category", y = "count")
}

# create contingency table for the chi-squared test
{
  discount_contingency_table <- table(classified_amzn$discount_percentage_category)
  cat("Contingency table for the chi-squared test", "\n")
  print(discount_contingency_table)
}

# box plot of each category based on rating
{
  ggplot(classified_amzn, aes(x = discount_percentage_category, y = rating)) +
    geom_boxplot(fill = "white") +
    labs(title = "Ratings by Discount Percentage Category", x = "Discount Percentage Category", y = "Rating") +
    theme_minimal()
}

# ANOVA test for discount percentage category and rating
{
  anova_rating <- aov(rating ~ discount_percentage_category, data=classified_amzn)
  summary(anova_rating)
}

# post-hoc analysis of rating ANOVA
{
  post_hoc_rating <- TukeyHSD(anova_rating)
  plot(post_hoc_rating)
}

# ANOVA test for discount percentage category and rating_count
{
  anova_rating_count <- aov(rating_count ~ discount_percentage_category, data=classified_amzn)
  summary(anova_rating_count)
}

# post-hoc analysis of rating_count ANOVA
{
  post_hoc_rating_count <- TukeyHSD(anova_rating_count)
  plot(post_hoc_rating_count)
}
