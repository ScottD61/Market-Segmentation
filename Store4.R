#Load Data
Store4 <- read.csv("/Users/scdavis6/Documents/Work/TowerData/Data/Client4.csv", 
                   na.strings = "", head = TRUE)
#Clean up titles of data.frame - CORRECT ANSWER FROM FORUM
names(Store4) <- gsub("_|\\.\\.\\.|\\." , "", names(Store4))
#Identify which rows have only NA values
which(rowSums(is.na(Store4))==ncol(Store4))
#Remove other columns
Store4new <- Store4[c(-1,-11,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,
                      -29,-30,-31,-32,-33,-34,-35)] 
#Remove all rows with only NA values
Store4df <- Store4new[!!rowSums(!is.na(Store4new)),]
#Convert csv to data.frame
df4 <- as.data.frame(Store4df)
#Save data.frame
save(df4,file="df4.Rda")
#View data.frame
head(df4)
#Info about data.frame
str(df4)
#Write df as csv
write.table(df4, file = "/Users/scdavis6/Desktop/df4.csv")
#Import cluster package
library(cluster)
#Create dissimilarity matrix
#Gower coefficient for finding distance between mixed variable
daisy4 <- daisy(df4, metric = "gower", type = list(ordratio = c(1:11))) 
#Pam algorithm with 3 clusters 
k4answers <- pam(daisy4, 3, diss = TRUE)
#Get number of observations per cluster
k4answers$id.med
#Information about clusters
k4answers$clusinfo
#Group row with cluster
Groups4 <- k4answers$silinfo
#Coerce object to data.frame
clustgroups2 <- as.data.frame(Groups4, row.names = NULL)
#Create .CSV file
write.table(clustgroups2, file <- "/Users/scdavis6/Desktop/Solutions4.csv")


#Option 2: CLARA function
#Exclude dissimilarity matrix
library(cluster)
client4.clara <- clara(Store4, 3, metric = "euclidean", stand = FALSE, samples = 5,
                       sampsize = (3000), rngR = FALSE, pamLike = TRUE)
#Get results
#Get number in each sample
client4.clara$sample
#Get medoids?
client4.clara$medoids
#Get each cluster with width and neighbor
client4.clara$silinfo
#Create table showing user.id in cluster - WRONG 
client4table <- table(Store4$email, client4.clara$cluster)
#Show results
client4table
#Coerce object to data.frame
clust4groups <- as.data.frame(client4table, row.names = NULL)
#Create .CSV file 
write.table(client4table, file <- "/Users/scdavis6/Desktop/ClaraClient4Solutions.csv")d


#Create visualizations
#histogram Age 
res <- ordered(Store4$Age, levels = c("18-20", "21-24", "25-34", 
                                      "35-44", "45-54", "55-64", "65+"))

#Set dimensions
par(mar=c(6,5,4,1))
#Create plot
plot(res, main = "Distribution of Age Client 4", xlab = "", ylab = "", las=2, ylim = c(0,4000))
mtext(text="Age", side=1, line=4)
mtext(text="Density", side=2, line=3.5)



#histogram Gender - done 
plot(Store4$Gender, main = "Distribution of Gender Client 4", xlab = "Gender", 
     ylab = "Density", ylim = c(0,8000))

#histogram HomeOwnerStatus
plot(Store4$HomeOwnerStatus, main = "Distribution of Home Ownership Client 4", xlab = "Home Ownership", 
     ylab = "Density", ylim = c(0,10000))


#Plot of Household Income 
res <- ordered(Store4$HouseholdIncome, levels = c("0-15k", "15k-25k", "25k-35k", 
                                                  "35k-50k", "50k-75k", "75k-100k", 
                                                  "100k-125k", "125k-150k", "150k-175k", 
                                                  "175k-200k", "200k-250k", "250k+"))
#Set dimensions
par(mar=c(7,5,4,1))
#Create plot
plot(res, main = "Distribution of Household Income Client 4", xlab = "", 
     ylab = "", las=2, ylim = c(0,4000))
mtext(text="HouseholdIncome", side=1, line=5.5)
mtext(text="Density", side=2, line=3.5)

#histogram of Marital Status - DONE 
par(mar=c(7,5,4,1))
plot(Store4$MaritalStatus, main = "Distribution of Marital Status Client 4", xlab = "Marital Status", 
     ylab = "Density", ylim = c(0,6000))

#histogram of prescence of children - Done
plot(Store4$PresenceofChildren, main = "Distribution of Children Prescence Client 4", xlab = "Children Prescence", 
     ylab = "Density", ylim = c(0,10000))

#histogram Home Market Value - DONE 
#Set the order
res1 <- ordered(Store4$HomeMarketValue, levels = c("1k-25k", "25k-50k", "50k-75k",
                                                   "75k-100k", "100k-150k",
                                                   "150k-200k", "200k-250k", 
                                                   "250k-300k", "300k-350k", 
                                                   "350k-500k", "500k-1mm", "1mm+"))
#Set dimensions
par(mar=c(8,5,4,1))
#Create plot
plot(res1, main = "Distribution of Home Market Value Client 4", 
     xlab = "", ylab = "", las=2, ylim = c(0,3000))
mtext(text="HouseholdIncome", side=1, line=6)
mtext(text="Density", side=2, line=3.5)


#histogram Occupation - DONE 
#set the order
res2 <- ordered(Store4$Occupation, levels = c("Blue Collar Worker", "Business Owner", 
                                              "Civil Service", "Executive/Upper Management", 
                                              "Health Services", "Homemaker", "Middle Management", 
                                              "Military Personnel", "Nurse", "Part Time", 
                                              "Professional", "Retired", "Secretary", 
                                              "Student", "Teacher", "Technology", 
                                              "White Collar Worker"))
#Set dimensions
par(mar=c(13,4,1,2))
#Create plot
plot(res2, main = "Distribution of Occupation Client 4", 
     xlab = "", las=2, ylab = "Density", ylim = c(0,2000))
mtext(text="Occupation", side=1, line=12)


#histogram education - DONE
res3 <- ordered(Store4$Education, levels = c("Completed High School", "Attended College", 
                                             "Completed College", "Completed Graduate School", 
                                             "Attended Vocational/Technical"))
#Set dimensions
par(mar=c(13,4.5,1,1))
#Create plot
plot(res3, main = "Distribution of Education Client 4", 
     xlab = "", ylab = "", las=2, ylim = c(0,4500))
mtext(text="Education", side=1, line=12)
mtext(text="Density", side=2, line=3.4)


#histogram length of residence - DONE 
res4 <- ordered(Store4$LengthofResidence, levels = c("Less than 1 year", "1 Year",  
                                                     "2 Years",  "3 Years",  "4 Years", 
                                                     "5 Years",  "6 Years",  "7 Years",  
                                                     "8 Years",  "9 Years",  "10 Years",  
                                                     "11-15 years", "16-19 years",  "20+ years"))
#Set dimensions
par(mar=c(9,5,2,1))
#Create plot
plot(res4, main = "Distribution of Length of Residence Client 4", 
     xlab = "", ylab = "", las=2, ylim = c(0,2000))
mtext(text="Length of Residence", side=1, line=7)
mtext(text="Density", side=2, line=3.5)



#Fill in data with mode from histograms
#Other method - works 
indx4 <- which(is.na(df4), arr.ind=TRUE)
df4[indx4] <- c("55-64", "Female", "150k-175k", "Married",
               "Yes", "Rent", "500k-1mm", "Professional",
               "Completed College")[indx4[,2]]