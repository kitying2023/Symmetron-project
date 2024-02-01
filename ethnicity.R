library(dplyr)
library(ggplot2)
library(tidyr)


race1 <- race_breast

#remove certain columns
columns_to_remove <- c("result_group_id","ctgov_group_code")
race1 <- race1[, !(names(race1) %in% columns_to_remove)]
# remove last column
race1 <- race1[, -ncol(race1)]

# pivot the table to visualized the data
pivoted_race <- race1 %>%
  pivot_wider(names_from = category, values_from = number, values_fill = 0)

# I found that for category 
# American Indian or Alaska Native
# Native Hawaiian or Other Pacific Islander
# More than one race
# Hispanic or Latino
# the number is so few
# so i decided to combine them with others


# Combine specified categories into "Others" for each nct_id

race1a <- race1 %>%
  mutate(category = ifelse(category %in% c("American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander", "More than one race", "Hispanic or Latino"), "Others", category)) %>%
  group_by(nct_id) %>%
  summarize(Others = sum(number[category == "Others"]),
            Asian = sum(number[category == "Asian"]),
            `Black or African American` = sum(number[category == "Black or African American"]),
            White = sum(number[category == "White"]),
            `Unknown or Not Reported` = sum(number[category == "Unknown or Not Reported"])) %>%
  pivot_longer(cols = c(Others, Asian, `Black or African American`, White, `Unknown or Not Reported`), names_to = "category", values_to = "number") %>%
  group_by(nct_id) %>%
  mutate(percentage = (number / sum(number)) * 100)

print(race1a)

ggplot(race1a, aes(x = category, y = nct_id, fill = percentage)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Ethnicity heatmap for breast cancer dataset", x = "Ethnicity", y = "NCT ID") +
  theme_minimal()