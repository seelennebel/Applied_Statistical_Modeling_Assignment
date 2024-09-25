# dataset URL
# https://www.kaggle.com/datasets/karkavelrajaj/amazon-sales-dataset

source("~/development/Applied_Statistical_Modeling_Assignment/functions.R")

#download_packages()

load_packages()

amzn <- load_dataset("~/development/Applied_Statistical_Modeling_Assignment/amazon.csv")

display_column_types(amzn, message="Initial Data Frame Classes")

amzn <- select_features(amzn)

amzn <- clean_dataset(amzn)

amzn <- feature_engineering(amzn)

display_column_types(amzn, message="Data Frame Classes with Appropriate Types")

cat("\n", "\n")
cat("<--------------------Statistical Analysis-------------------->")
cat("\n", "\n")

# display PDFs of all numerical and integer features
discounted_price_PDF <- probability_density_function(amzn, amzn$discounted_price, "PDF of discounted_price")
actual_price_PDF <- probability_density_function(amzn, amzn$actual_price, "PDF of actual_price")
discount_percentage_PDF <- probability_density_function(amzn, amzn$discount_percentage, "PDF of discount_percentage")
rating_PDF <- probability_density_function(amzn, amzn$rating, "PDF of discounted_price")
rating_count_PDF <- probability_density_function(amzn, amzn$rating_count, "PDF of rating_count")

grid.arrange(discounted_price_PDF, actual_price_PDF, discount_percentage_PDF, rating_PDF, rating_count_PDF)

# create a classified amazon dataset
classified_amzn <- mutate(amzn,
  discount_percentage_category = case_when(
    amzn$discount_percentage == 0 ~ "0%",
    amzn$discount_percentage <= 0.25 ~ "<25%",
    amzn$discount_percentage <= 0.5 ~ "<50%",
    amzn$discount_percentage <= 0.75 ~ "<75%",
    amzn$discount_percentage <= 1 ~ "<100%"
  )
)

# box plot of each category based on rating
ggplot(classified_amzn, aes(x = discount_percentage_category, y = rating)) +
  geom_boxplot(fill = "white") +
  labs(title = "Ratings by Discount Percentage Category", x = "Discount Percentage Category", y = "Rating") +
  theme_classic()

ggplot(classified_amzn, aes(x = discount_percentage_category, y = rating_count)) +
  geom_boxplot(fill = "white") +
  labs(title = "Rating Counts by Discount Percentage Category", x = "Discount Percentage Category", y = "Rating Counts") +
  theme_classic()

shapiro.test(amzn$rating)
shapiro.test(amzn$rating_count)

kruskal.test(rating ~ discount_percentage_category, data=classified_amzn)
cat("Dunn's test for rating", "\n")
dunn.test(classified_amzn$rating, classified_amzn$discount_percentage_category, method="hochberg")

kruskal.test(rating_count ~ discount_percentage_category, data=classified_amzn)
cat("Dunn's test for rating count", "\n")
dunn.test(classified_amzn$rating_count, classified_amzn$discount_percentage_category, method="hochberg")

# create a bar plot with category counts
category_counts <- classified_amzn %>%
  count(discount_percentage_category)

ggplot(category_counts, aes(x = discount_percentage_category, y = n)) +
  geom_bar(stat = "identity", fill = "black", color = "black") +
  theme_minimal() +
  labs(title = "Count of each discount_percentage category", x = "category", y = "count")


rating_medians <- tapply(classified_amzn$rating, classified_amzn$discount_percentage_category, median)
median_plot(rating_medians, "Median Rating by Discount Percentage Category", xlab="discount percentage category", ylab="median rating")

rating_means <- tapply(classified_amzn$rating, classified_amzn$discount_percentage_category, mean)
mean_plot(rating_means, "Mean Rating by Discount Percentage Category", xlab="discount percentage category", ylab="mean rating")


rating_count_medians <- tapply(classified_amzn$rating_count, classified_amzn$discount_percentage_category, median)
median_plot(rating_count_medians, "Median Rating Count by Discount Percentage Category", xlab="discount percentage category", ylab="median rating count")

rating_count_means <- tapply(classified_amzn$rating_count, classified_amzn$discount_percentage_category, mean)
mean_plot(rating_count_means, "Mean Rating Count by Discount Percentage Category", xlab="discount percentage category", ylab="mean rating count")

