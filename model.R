# dataset URL
# https://www.kaggle.com/datasets/karkavelrajaj/amazon-sales-dataset

source("C://Users/seelennebel/dev/Applied_Statistical_Modeling_Assignment/functions.R")

#download_packages()

load_packages()

amzn <- load_dataset("C:/Users/seelennebel/dev/Applied_Statistical_Modeling_Assignment/amazon.csv")

display_column_types(amzn)

amzn <- select_features(amzn)

amzn <- clean_dataset(amzn)

display_column_types(amzn)

amzn <- feature_engineering(amzn)

# display PDFs of all numerical and integer features
{
  discounted_price_PDF <- probability_density_function(amzn, amzn$discounted_price, "PDF of discounted_price")
  actual_price_PDF <- probability_density_function(amzn, amzn$actual_price, "PDF of actual_price")
  discount_percentage_PDF <- probability_density_function(amzn, amzn$discount_percentage, "PDF of discount_percentage")
  rating_PDF <- probability_density_function(amzn, amzn$rating, "PDF of discounted_price")
  rating_count_PDF <- probability_density_function(amzn, amzn$rating_count, "PDF of rating_count")

  grid.arrange(discounted_price_PDF, actual_price_PDF, discount_percentage_PDF, rating_PDF, rating_count_PDF)
}

# create a classified dataset
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

zero <- filter(classified_amzn, discount_percentage_category=="0%")
twenty <- filter(classified_amzn, discount_percentage_category=="<20%")
forty <- filter(classified_amzn, discount_percentage_category=="<40%")
sixty <- filter(classified_amzn, discount_percentage_category=="<60%")
eighty <- filter(classified_amzn, discount_percentage_category=="<80%")
one_hundred <- filter(classified_amzn, discount_percentage_category=="<100%")

category_counts <- classified_amzn %>%
  count(discount_percentage_category)

ggplot(category_counts, aes(x = discount_percentage_category, y = n)) +
  geom_bar(stat = "identity", fill = "black", color = "black") +
  theme_minimal() +
  labs(title = "Count of Each discount_percentage category", x = "category", y = "count")

discount_contingency_table <- table(classified_amzn$discount_percentage_category)
print(discount_contingency_table)

# chi-squared test for discount_percentage categories
chi_squared_test <- chisq.test(contingency_table)
print(chi_squared_test)

anova_rating <- aov(rating ~ discount_percentage_category, data=classified_amzn)
summary(anova_rating)

post_hoc_rating <- TurkeyHSD(anova_rating)