library(dplyr)
library(ggplot2)

age <- filtered_extracted_age_extra

# Remove columns by name
columns_to_remove <- c("result_group_id","classification","category","title","dispersion_type")
age <- age[, !(names(age) %in% columns_to_remove)]

age <- age[complete.cases(age), ]
colnames(age)[2] <- "mean"
colnames(age)[3] <- "number"
colnames(age)[4] <- "sd"

# mean of the mean for the 7 trials
overall_mean <- mean(age$mean)
# sd of mean across 7 trials
overall_sd <- sd(age$mean)

# standard error of mean
age <- age %>%
  mutate(se = sd / sqrt(number))


weighted_mean <- sum(age$mean * age$number/ sum(age$number))
se_weighted_mean <- sqrt(sum(age$number * (age$mean - weighted_mean)^2) / sum(age$number)^2)
sd_weighted_mean <- se_weighted_mean * sqrt(nrow(age))


ggplot(age, aes(x = mean, xmin =  mean- 1.96 * se, xmax = mean + 1.96 * se, y = nct_id)) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = overall_mean, linetype = "dashed", color = "red") +  # Add a dashed line for the overall mean
  labs(title = "Forest Plot", x = "Mean age", y = "Study")

#install.packages("metafor")
library(metafor)

# Perform a meta-analysis
result <- rma(mean, sei=se, data = age)

# Create a forest plot
forest(result, showweights = TRUE, slab = nct_id)
ggsave("forest_plot.png", device = "png") 










age <- age %>%
  mutate(standardized_means = (mean - weighted_mean) / overall_sd)

# Choose the desired confidence level (e.g., 95%)
confidence_level <- 0.95

# Calculate the Z-score based on the confidence level
z_score <- qnorm((1 + confidence_level) / 2)

# Calculate the margin of error for each standardized mean
margin_of_error <- z_score * se_weighted_mean

# Create new columns for the lower and upper bounds of the confidence interval
age <- age %>%
  mutate(
    Lower_CI_Standardized_Mean = (standardized_means - margin_of_error),
    Upper_CI_Standardized_Mean = (standardized_means + margin_of_error)
  )

# Create a forest plot using ggplot2
ggplot(age, aes(x = standardized_means, xmin = Lower_CI_Standardized_Mean, xmax = Upper_CI_Standardized_Mean, y = nct_id)) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  labs(title = "Forest Plot", x = "Mean", y = "Study")





