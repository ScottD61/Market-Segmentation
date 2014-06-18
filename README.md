Clustering-Customer--1
======================

Daisy clustering algorithm applied to the first customer

# Load Data
> Store1 <- read.csv("/Users/scdavis6/Documents/Work/TowerData/TowerData/Client1.csv", head=FALSE)

# Convert csv to data.frame
> df <-as.data.frame(Store1)

# View data.frame
> head(df)

# Info about data.frame
> str(df)

# Purpose of Clustering: 
# Identify feature space for finding structures of features
# Data Pre-Processing
# Fill in missing values b/c no missing values in k-means - normalization
# Feature scaling

# Daisy package automatically performs standardization
# Download daisy package
> library(cluster)

#Create dissimilarity matrix
> daisy1 <- daisy(df, metric="gower")

#Pam clustering algorithm
> pam(daisy1, 3, mediods = NULL) 
