#Load smaller version of dataset with 300 variables 
Smallstore1 <- read.csv("/Users/scdavis6/Documents/Work/TowerData/TowerData/Smallclient1.csv", 
                   na.strings = "", head = TRUE)
#Convert csv to data.frame
frame <- as.data.frame(Smallstore1, stringsAsFactors = FALSE)
#Save data.frame
save(frame, file = "sdf.Rda")
#View data.frame
head(frame, n = 7)
#Info about data.frame
str(frame)
library(cluster)
#Remove other columns
Smallstore1new <- Smallstore1[c(-11,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28
                                -29,-30,-31,-32,-33,-34)] 
Smallstore1new$userid <- NULL
#Clean up titles of data.frame - CORRECT ANSWER FROM FORUM
names(Smallstore1) <- gsub("_|\\.\\.\\.|\\." , "", names(Smallstore1))
#Remove all rows with only NA values
SmallStoredf <- Smallstore1new[!!rowSums(!is.na(Smallstore1new)),]


#Create dissimilarity matrix
#Gower coefficient for finding distance between mixed variables
daisy1 <- daisy(SmallStoredf, metric = "gower", type = list(ordratio = c(1:14))) 
#k-medoid algorithm with 3 clusters 
kanswers <- pam(daisy1, 3, diss = TRUE)
#Get summary
summary(kanswers)
#Get number of observations per cluster
kanswers$id.med
#Information about clusters
kanswers$clusinfo
#Group User.ID with cluster
Groups <- kanswers$silinfo

#Still need to fix the id label
#Create index - USE THIS FOR PAM 
emailsmall <- SmallStoredf[, c(2)]
#Create table
clienttable <- table(emailsmall, kanswers$cluster)
clienttable1 <- kanswers$cluster
#Convert to csv
write.table(clienttable1, file <- "/Users/scdavis6/Desktop/Smallclient.csv")
#convert to csv - USE THIS 
write.table(Groups, file <- "/Users/scdavis6/Desktop/Smallclient1.csv")


#Create graphs
plot(Smallstore1[c("Age", "DiscountShopper")], col = 
       kanswers$cluster)

#Option 2: CLARA function
#Load library
library(cluster)
#Set seed for reproducibility
set.seed(2)
#CLARA algorithm with 3 clusters and 5 samples of 60 
clara1 <- clara(frame, 3, metric = "gower", stand = FALSE, samples = 5,
      sampsize = (60), medoids.x = TRUE, keep.data = TRUE, 
      rngR = TRUE, pamLike = TRUE)
#Get results
#Get number in each sample
clara1$sample
#Get medoids?
clara1$medoids
#Get each cluster with width and neighbor
clara1$silinfo
#Create table showing user.id in cluster
smallclienttable <- table(Smallstore1$userid, clara1$cluster)
#Show results
smallclienttable
#Coerce object to data.frame
smallclustgroups <- as.data.frame(smallclienttable, row.names = NULL)
#Create .CSV file 
write.table(smallclienttable, file <- "/Users/scdavis6/Desktop/SmallSolutions.csv")
#Create graphs
plot(Smallstore1$Age, Smallstore1$DiscountShopper)
#Cluster plot of data
clusplot(clara1, main = "CLARA Results", color = TRUE, labels = 5)

#TRY THIS FOR CLARA
emailsmall <- Smallstore1[, c(2)]
#Create table
clienttable <- table(emailsmall, clara1$cluster)
#Convert to csv
write.table(clienttable, file <- "/Users/scdavis6/Desktop/Smallclient.csv")

#Steps for k-mediod
#calculate distance between all points (daisy)
#calculate "L" for every point (L=total distances for every point)
#choose smallest "L" (aka mediod)

#difference between kmeans and kmediod
#k-means sensitive to outliers
#kmedios: instaed of taking the mean value of the object in the cluster (like kmeans)...
#mediod is used (most centrally located object in a cluster)

#kmediod less influenced by outliers or other extreme values than kmeans
#works for small data sets but doesn't scale well for large
#computationally difficult b/c we have to find a "REAL" point in the data...
#instead of thd mean(centroid) in kmeans


#Replace missing value of education with mode - idk
replacementVals <- c(Age = "45-54", Gender = "Male", HouseholdIncome = "50K-75K", 
                     MaritalStatus = "Single", PresenceofChildren = "No",
                     HomeOwnerStatus = "Own", HomeMarketValue = "350K-500K",
                     Occupation = "Professional", Education = "Completed High School",
                     LengthofResidence = "11-15yrs")
indx1 <- replacementVals[col(df2)][is.na(df2[,names(replacementVals)])]
df2[is.na(df2[,names(replacementVals)])]  <- indx1







indx <- which(is.na(SmallStoredf), arr.ind=TRUE)
SmallStoredf[indx] <- c("Own", "350K-500K", "Professional")[indx[,2]]
