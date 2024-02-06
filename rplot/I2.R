library(tidyverse)

df<- filtered_extracted_sex

library(tidyverse)



# Modify the dataframe in place to add the number of males and the proportion of females
df <- df %>%
  group_by(nct_id) %>%
  mutate(
    Num_Females = sum(param_value[category == "Female"]),
    Num_Males = sum(param_value[category == "Male"]),
    Proportion_Females = Num_Females / (Num_Females + Num_Males)
  ) %>%
  ungroup() %>%
  # Select distinct rows based on 'nct_id' to remove duplicates
  distinct(nct_id, .keep_all = TRUE) %>%
 
  select(nct_id, Num_Females, Num_Males, Proportion_Females)


print(df)
install.packages("meta")
library(meta)
df <- df %>%
  mutate(Total_Participants = Num_Females + Num_Males)


print(df)

meta_analysis <- metaprop(df$Num_Females,df$Total_Participants, sm = "PAS", method.tau.ci="J", data=df)

# Print the meta-analysis results
print(meta_analysis)


random_effects <- meta_analysis$random
cat("Random Effects Pooled Proportion:", random_effects$TE, "\n95% CI =", random_effects$lower, "to", random_effects$upper, "\n\n")

# Print Cochran's Q and I² statistics
cat("Cochran's Q =", meta_analysis$Q, "(df =", meta_analysis$k - 1, ") P-value =", meta_analysis$pval.Q, "\n")
cat("I² =", meta_analysis$I2, "% (95% CI =", meta_analysis$I2.lower, "% to", meta_analysis$I2.upper, "%)\n")

# Print the moment-based estimate of between-study variance
cat("Moment-based estimate of between studies variance =", meta_analysis$tau², "\n")