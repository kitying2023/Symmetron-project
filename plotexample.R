# Install and load the metafor package if you haven't already
# install.packages("metafor")
library(metafor)
#Load Libraries
library(meta)
library(metafor)


# Creating a hypothetical dataset
df <- data.frame(
  study = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5", "Study 6", "Study 7"),
  Measure = rep("CT", 7),
  nd = c(6, 6, 10, 6, 8, 6, 6),
  meand = c(129.4, 98.4, 54.3, 90.5, 72.7, 90.3, 92.3),
  sdd = c(5.14, 3.28, 2.8, 1.87, 7.4, 2.72, 2.71),
  n = c(6, 6, 10, 6, 8, 6, 6),
  mean = c(145.0, 135.0, 56.1, 130.0, 103.0, 107.0, 106.0),
  sd = c(11.2, 1.96, 2.6, 3.02, 5.5, 3.09, 3.05)
)

# Print the dataset
print(df)

meta_data <- escalc(measure="MD", m1i=meand, sd1i=sdd, n1i=nd, m2i=mean, sd2i=sd, n2i=n, data=df)

res <- rma(yi, vi, data=meta_data)

# Create a forest plot
forest(res, slab=df$study, refline='NA',showweights = FALSE, 
       header="Study ID", ilab.pos=4,cex=1.5)

