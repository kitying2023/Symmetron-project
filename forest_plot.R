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

#install.packages("metafor")
library(metafor)

# Perform a meta-analysis
result <- rma(mean, sei=se, data = age)

# Create a forest plot
forest(result, showweights = TRUE, slab = nct_id)

# Display the results
summary(result)


# 'measure' specifies the type of effect size to compute

result <- escalc(measure = "SMD", m1i = mean1, sd1i = sd1, n1i = n1,
                 m2i = mean2, sd2i = sd2, n2i = n2, data = your_data)

# 'result' now contains the computed effect sizes and sampling variances

##### back pain dataset ###
age_back <- age_backpain
columns_to_remove <- c("result_group_id","classification","category","title","dispersion_type")
age_back <- age_back[, !(names(age_back) %in% columns_to_remove)]
age_back <- age_back[complete.cases(age_back), ]

colnames(age_back)[2] <- "mean"
colnames(age_back)[3] <- "number"
colnames(age_back)[4] <- "sd"

# standard error of mean
age_back <- age_back %>%
  mutate(se = sd / sqrt(number))

result <- rma(mean, sei=se, data = age_back)

# Create a forest plot
forest(result, showweights = TRUE, slab = nct_id)

