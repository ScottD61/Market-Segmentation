#Load Data
Store2 <- read.csv("/Users/scdavis6/Documents/Work/TowerData/Data/Client2.csv", 
                   na.strings = "", head = TRUE)
#Clean up titles of data.frame - CORRECT ANSWER FROM FORUM
names(Store2) <- gsub("_|\\.\\.\\.|\\." , "", names(Store2))
#Remove other columns
Store2new <- Store2[c(-1,-2,-13,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,
                      -29,-30,-31,-32,-33,-34,-35,-36)] 
#Remove all rows with only NA values
Store2df <- Store2new[!!rowSums(!is.na(Store2new)),]
#Convert csv to data.frame
df2 <- as.data.frame(Store2df)
#Other method
indx <- which(is.na(df2), arr.ind=TRUE)
df2[indx] <- c("45-54", "Male", "50k-75k", "Single",
               "No", "Own", "350k-500k", "Professional",
               "Completed High School")[indx[,2]]
#Save data.frame
save(df2,file = "df2.Rda")
#Write df as csv
write.table(df2, file = "/Users/scdavis6/Desktop/df2.csv")
#View data.frame
head(df2)
#Info about data.frame
str(df2)
#Import cluster package
library(cluster)
#Create dissimilarity matrix
#Gower coefficient for finding distance between mixed variable
daisy2 <- daisy(df2, metric = "gower", type = list(ordratio = c(1:10))) 
#Pam algorithm with 3 clusters 
k2answers <- pam(daisy2, 3, diss = TRUE)
#Get number of observations per cluster
k2answers$id.med
#Information about clusters
k2answers$clusinfo
#Group row with cluster
Groups2 <- k2answers$clustering
#Coerce object to data.frame
clustgroups2 <- as.data.frame(Groups2, row.names = NULL)
#Create .CSV file
write.table(clustgroups2, file <- "/Users/scdavis6/Desktop/Solutions2.csv")


#Put old email column to dataset
#Load Data
Store2 <- read.csv("/Users/scdavis6/Documents/Work/TowerData/Data/Client2.csv", 
                   na.strings = "", head = TRUE)
#Clean up titles of data.frame - CORRECT ANSWER FROM FORUM
names(Store2) <- gsub("_|\\.\\.\\.|\\." , "", names(Store2))
#Remove row # and email from dataset
Store2new <- Store2[c(-1,-2)]
#Identify which rows have only NA values
which(rowSums(is.na(Store2new))==ncol(Store2new))
#Remove all rows with only NA values - WONT WORK!!!!
Store2df1 <- Store2new[!!rowSums(!is.na(Store2new)),]
#Remove other columns
Store2new1 <- Store2df1[c(2)] 



#Results
#Group User.ID with cluster
Groups2 <- k2answers$silinfo
#Coerce object to data.frame
clust2groups <- as.data.frame(Groups2, row.names = NULL)
#Create csv file
write.table(clus2groups, file <- "/Users/scdavis6/Desktop/client2results.csv")


#Option 2: CLARA function
#Exclude dissimilarity matrix
library(cluster)
#Clara algorithm
#Set seed for reproducibility
set.seed(1)
#Changing medoids.x and keep.data = TRUE - new way 
client2.clara <- clara(FinalData, 3, metric = "manhattan", stand = FALSE, samples = 5,
                       sampsize = (2600), medoids.x = TRUE, keep.data = TRUE, 
                       rngR = TRUE, pamLike = TRUE)
#Get results
#Get number of observations in each cluster
client2.clara$clusinfo
#Get medoids?
client2.clara$medoids
#Get each cluster with width and neighbor - this gets the best sample
client2.clara$silinfo
#Show list of emails according to cluster - WRONG 
#Create index
pamemail2 <- Store2[, c(1)]
#Create table showing email in cluster  
pamclienttable <- table(pamemail2, (client2.clara$cluster))
#Coerce object to data.frame
pamclustgroups2 <- data.frame(pamclienttable, row.names = NULL)
#Create .CSV file 
write.table(pamclienttable, file <- "/Users/scdavis6/Desktop/ClaraClient2Solutions.csv")

#Create graphs - bad shows dots
plot(Store2$Age, Store2$DiscountShopper)
#Cluster plot of data - doesn't work shows line and ovals
clusplot(client2.clara, main = "CLARA Results", color = TRUE, labels = 5)
#Cluster plot of partitions
clusplot.default(client2.clara)

#histogram Age - DONE
res <- ordered(Store2$Age, levels = c("18-20", "21-24", "25-34", 
                                                  "35-44", "45-54", "55-64", "65+"))

#Set dimensions
par(mar=c(6,5,4,1))
#Create plot
plot(res, main = "Distribution of Age Client 2", xlab = "", 
     ylab = "", las=2, ylim = c(0,2500))
mtext(text="Age", side=1, line=4.5)
mtext(text="Density", side=2, line=3.5)



#histogram Gender - DONE
plot(Store2$Gender, main = "Distribution of Gender Client 2", xlab = "Gender", ylab = "Density", ylim = c(0,7000))

#histogram HomeOwnerStatus
plot(Store2$HomeOwnerStatus, main = "Distribution of Home Ownership Client 2", xlab = "Home Ownership", 
     ylab = "Density", ylim = c(0,7000))


#Plot of Household Income - DONE 
res <- ordered(Store2$HouseholdIncome, levels = c("0-15k", "15k-25k", "25k-35k", 
                                                "35k-50k", "50k-75k", "75k-100k", 
                                                "100k-125k", "125k-150k", "150k-175k", 
                                                "175k-200k", "200k-250k", "250k+"))
#Set dimensions
par(mar=c(7,5,4,2))
#Create plot
plot(res, main = "Distribution of Household Income Client 2", xlab = "", 
     ylab = "", las=2, ylim = c(0,2500))
mtext(text="HouseholdIncome", side=1, line=5.5)
mtext(text="Density", side=2, line=3.5)


#histogram of Marital Status - Done 
plot(Store2$MaritalStatus, main = "Distribution of Marital Status Client 2", xlab = "Marital Status", 
     ylab = "Density", ylim = c(0,5000))

#histogram of prescence of children
plot(Store2$PresenceofChildren, main = "Distribution of Children Prescence Client 2", xlab = "Children Prescence", 
     ylab = "Density", ylim = c(0,7000))

#histogram Home Market Value - DONE 
#Set the order
res1 <- ordered(Store2$HomeMarketValue, levels = c("1k-25k", "25k-50k", "50k-75k",
                                                    "75k-100k", "100k-150k",
                                                    "150k-200k", "200k-250k", 
                                                   "250k-300k", "300k-350k", 
                                                    "350k-500k", "500k-1mm", "1mm+"))
#Set dimensions
par(mar=c(7,5,4,1))
#Create plot
plot(res1, main = "Distribution of Home Market Value Client 2", 
     xlab = "", ylab = "", las = 2, ylim = c(0,3000))
mtext(text="Home Market Value", side=1, line=5.5)
mtext(text="Density", side=2, line=3.5)


#histogram Occupation - DONE 
#set the order
res2 <- ordered(Store2$Occupation, levels = c("Blue Collar Worker", "Business Owner", 
                                              "Civil Service", "Executive/Upper Management", 
                                              "Health Services", "Homemaker", "Middle Management", 
                                              "Military Personnel", "Nurse", "Part Time", 
                                              "Professional", "Retired", "Secretary", 
                                              "Student", "Teacher", "Technology", 
                                              "White Collar Worker"))
#Set dimensions
par(mar=c(12.5,4,2,1))
#Create plot
plot(res2, main = "Distribution of Occupation Client 2", 
     xlab = "", ylab = "Density", las = 2)
mtext(text="Occupation", side=1, line=11)

#histogram education
res3 <- ordered(Store2$Education, levels = c("Completed High School", "Attended College", 
                                             "Completed College", "Completed Graduate School", 
                                             "Attended Vocational/Technical"))
#Set dimensions
par(mar=c(13,5,1,1))
#Create plot
plot(res3, main = "Distribution of Education Client 2", 
     xlab = "", ylab = "", las = 2, ylim = c(0,3500))
mtext(text="Education", side=1, line=12)
mtext(text="Density", side=2, line=3.5)



#histogram length of residence
res4 <- ordered(Store2$LengthofResidence, levels = c("Less than 1 year", "1 Year",  
                                                    "2 Years",  "3 Years",  "4 Years", 
                                                    "5 Years",  "6 Years",  "7 Years",  
                                                    "8 Years",  "9 Years",  "10 Years",  
                                                    "11-15 years", "16-19 years",  "20+ years"))
#Set dimensions
par(mar=c(9,5,3,1))
#Create plot
plot(res4, main = "Distribution of Length of Residence Client 2", 
     xlab = "", ylab = "", las = 2, ylim = c(0,2000))
mtext(text="Length of Residence", side=1, line=8)
mtext(text="Density", side=2, line=3.5)



#FORUM CODE - didn't work 
Store2new1 <- Store2$RowNoC
Store2df$RowNo <- NULL
Store2df$Email <- NULL
Store2df1 <- Store2df[!!rowSums(!is.na(Store2df[,-1])),] #Here you already get the new dataset with `RowNo` column
Store2new2 <- Store2new1[Store2new1 %in% Store2df1$RowNo]
Store2new1[Store2new1 %in% Store2df1$RowNo]

NewStore2 <- as.data.frame(Store2new2)
