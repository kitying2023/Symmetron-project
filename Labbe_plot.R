sex_breast <- filtered_extracted_sex

# Load necessary libraries
library(ggplot2)
library(dplyr)

columns_to_remove <- c("result_group_id","ctgov_group_code","title")
sex_breast <- sex_breast[, !(names(sex_breast) %in% columns_to_remove)]

# Create a new column 'female_ratio' and 'total_participants' 
sex_breast <- sex_breast %>%
  group_by(nct_id) %>%
  mutate(
    female_ratio = sum(param_value[category == "Female"]) / sum(param_value),
    total_participants = sum(param_value)
  )

sex_breast <- distinct(sex_breast, nct_id, .keep_all = TRUE)


# Create a L'Abbé plot
abbep <- ggplot(sex_breast , aes(x = factor(nct_id), y = female_ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_hline(yintercept = mean(sex_breast$female_ratio), linetype = "dashed", color = "red") +
  labs(title = "L'Abbé Plot - Sex Ratio Across Studies",
       x = "Study ID",
       y = "Sex Ratio",
       caption = "Red dashed line represents the overall sex ratio") +
  theme_minimal()

# Display the plot
print(abbep)
