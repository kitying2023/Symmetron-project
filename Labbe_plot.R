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

# Create a L'Abbé plot
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

ggplot(sex_ratio, aes(x = female_ratio, y = 1 - female_ratio, label = nct_id)) +
  geom_point(size = 3, color = "blue") +
  geom_text(aes(label = nct_id), hjust = 0, vjust = 0) +
  labs(title = "L'Abbé Plot - Proportion of Female vs Male Across Studies",
       x = "Proportion of Female",
       y = "Proportion of Male") +
  theme_minimal() +  
  xlim(0, 1) +  
  ylim(0, 1) 

