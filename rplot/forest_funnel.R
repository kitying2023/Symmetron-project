library(dplyr)
library(ggplot2)
library(metafor)

###breast dataset###

age1 <- age_breast

# Remove columns by name
columns_to_remove <- c("result_group_id","classification","category","title","dispersion_type")
age1 <- age1[, !(names(age1) %in% columns_to_remove)]

age1 <- age1[complete.cases(age1), ]
colnames(age1)[2] <- "mean"
colnames(age1)[3] <- "number"
colnames(age1)[4] <- "sd"

# standard error of mean
age1 <- age1 %>%
  mutate(se = sd / sqrt(number))


weighted_mean <- sum(age$mean * age$number/ sum(age$number))
se_weighted_mean <- sqrt(sum(age$number * (age$mean - weighted_mean)^2) / sum(age$number)^2)
sd_weighted_mean <- se_weighted_mean * sqrt(nrow(age))

#overall mean
overall_mean<-result1$b

# Perform a meta-analysis
result1 <- rma(mean, sei=se, data = age1)

# Create a forest plot using all data
forest( result1, slab = nct_id,
        xlab = "Mean",
        head =c("NCT ID", "Mean [95% CI]"),
        cex=1.5)

# create funnel plot
funnel1<- funnel(result1)


# ?baujat plot package
baujat(result1)

b1 <- baujat(result1,symbol=19)
b1 <- b1[b1$x >= 1 | b1$y >= 0.2,]
text(b1$x, b1$y, b1$slab, pos=1, cex=0.8)

# try to plot baujat using ggplot

# create new column for square pearson residual
age1 <- age1 %>%
  mutate(pearson = (c(overall_mean)-mean)**2/c(overall_mean))

#create a new column for variance and contribution to heterogeneity of individual trials
age1 <- age1 %>%
  mutate(var = se^2 + result1$tau2) %>%
  mutate(h = (mean - c(overall_mean))^2 / var)

# Create a new column for influences in age
age1$influence <- NA

# Loop through each row of df
for (i in 1:nrow(age1)) {
  # Remove the i-th row
  df_without_row_i <- age1[-i, ]
  
  # Fit a random-effects meta-analysis model
  estimate <- rma(mean, sei = se, data = df_without_row_i)
  
  # Calculate influence
  influence <- (estimate$b - overall_mean)**2 / estimate$se**2
  
  # Store influence value in the new column
  age1$influence[i] <- influence
}

age1

ggplot(age1, aes(x = h, y = influence)) +
  geom_point() +
  labs(x = "Sqaured Pearson Residual", y = "Influence on overall result")


##### back pain dataset ###
age2 <- age_back
columns_to_remove <- c("result_group_id","classification","category","title","dispersion_type")
age2 <- age2[, !(names(age2) %in% columns_to_remove)]
age2 <- age2[complete.cases(age2), ]

colnames(age2)[2] <- "mean"
colnames(age2)[3] <- "number"
colnames(age2)[4] <- "sd"

# standard error of mean
age2 <- age2 %>%
  mutate(se = sd / sqrt(number))

result2 <- rma(mean, sei=se, data = age2)
overall_mean2<-result2$b


# Create a forest plot
forest(result2, slab = nct_id,
       refline='NA',
       xlab = "Mean",
       head =c("NCT ID", "Mean [95% CI]"),
       cex=1)

# create funnel plot
funnel2<- funnel(result2)

#baujat plot
b2 <- baujat(result2,symbol=19)
b2 <- b2[b2$x >= 3 | b2$y >= 0.10,]
text(b2$x, b2$y, b2$slab, pos=1, cex=0.8)

# try to plot baujat using ggplot

# create new column for square pearson residual, variance and contribution to heterogeneity of individual trials
age2 <- age2 %>%
  mutate(pearson = (c(overall_mean2)-mean)**2/c(overall_mean2)) %>%
  mutate(var = se^2+result2$tau2) %>%
  mutate(h = (mean - c(overall_mean2))^2 / var)

# Create a new column for influences in age
age2$influence <- NA

# Loop through each row of df
for (i in 1:nrow(age2)) {
  # Remove the i-th row
  df_without_row_i <- age2[-i, ]
  
  # Fit a random-effects meta-analysis model
  estimate <- rma(mean, sei = se, data = df_without_row_i)
  
  # Calculate influence
  influence <- (estimate$b - overall_mean2)**2 / estimate$se**2
  
  # Store influence value in the new column
  age2$influence[i] <- influence
}

age2

ggplot(age2, aes(x = h, y = influence)) +
  geom_point() +
  labs(x = "h", y = "Influence on overall result")

### mean difference ###

# breast dataset #
estimate<-result1$b

# mean difference for breast dataset
age1 <- age1 %>%
  mutate(mean_diff = mean - c(estimate))

ggplot(age1, aes(x = nct_id, y = mean_diff)) +
  geom_point(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Mean Difference by nct_id",
       x = "nct_id",
       y = "Mean Difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# back pain dataset #
# mean difference for back pain dataset
estimate2<-result2$b

age2 <- age2 %>%
  mutate(mean_diff = mean - c(estimate2))

ggplot(age2, aes(x = nct_id, y = mean_diff)) +
  geom_point(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Mean Difference by nct_id",
       x = "nct_id",
       y = "Mean Difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

