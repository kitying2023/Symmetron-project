sex_breast <- filtered_extracted_sex

# Load necessary libraries
library(ggplot2)
library(dplyr)

columns_to_remove <- c("result_group_id","ctgov_group_code","title")
sex_breast <- sex_breast[, !(names(sex_breast) %in% columns_to_remove)]

# Create a new column 'female_ratio' and 'total_participants' 
sex_ratio <- sex_breast %>%
  group_by(nct_id) %>%
  mutate(
    total_participants = sum(param_value),
    female_ratio = sum(param_value[category == "Female"]) / sum(param_value)
  ) %>%
  filter(category == "Female")

# gender plot for breast cancer dataset
abbep <- ggplot(sex_ratio , aes(x = factor(nct_id), y = female_ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_hline(yintercept = mean(sex_ratio$female_ratio), linetype = "dashed", color = "red") +
  labs(title = "L'Abbé Plot - Sex Ratio Across Studies",
       x = "Study ID",
       y = "Sex Ratio",
       caption = "Red dashed line represents the overall sex ratio") +
  theme_minimal()

# Display the plot
print(abbep)

## working on back dataset ##
sex_back <- sex_test2

columns_to_remove <- c("result_group_id","ctgov_group_code","title")
sex_back <- sex_back[, !(names(sex_back) %in% columns_to_remove)]

# Create a new column 'female_ratio' and 'total_participants' 
sex_ratio2 <- sex_back %>%
  group_by(nct_id) %>%
  mutate(
    total_participants = sum(param_value),
    female_ratio = sum(param_value[category == "Female"]) / sum(param_value)
  ) %>%
  filter(category == "Female")

# # gender plot for back pain dataset
abbep2 <- ggplot(sex_ratio2 , aes(x = factor(nct_id), y = female_ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_hline(yintercept = mean(sex_ratio2$female_ratio), linetype = "dashed", color = "red") +
  labs(title = "L'Abbé Plot - Sex Ratio Across Studies",
       x = "Study ID",
       y = "Sex Ratio",
       caption = "Red dashed line represents the overall sex ratio") +
  theme_minimal()

# Display the plot
print(abbep2)

