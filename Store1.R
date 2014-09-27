#Load Data
Store1 <- read.csv("/Users/scdavis6/Documents/Work/TowerData/Data/Client1.csv", 
                   na.strings = "", head = TRUE)
#Clean up titles of data.frame - CORRECT ANSWER FROM FORUM
names(Store1) <- gsub("_|\\.\\.\\.|\\." , "", names(Store1))
#Identify which rows have only NA values
which(rowSums(is.na(Store1))==ncol(Store1))
#Remove other columns
Store1new <- Store1[c(-1,-2,-12,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,
                      -29,-30,-31,-32,-33,-34,-35,-36)] 
#Remove all rows with only NA values
Store1df <- Store1new[!!rowSums(!is.na(Store1new)),]
#Convert csv to data.frame
df <-as.data.frame(Store1df)
#Convert csv to data.frame
#Save data.frame
save(df, file = "df.Rda")
#View data.frame
head(df)
#Info about data.frame
str(df)
#Create index of half of df
#Part1 
df1a <- df[1:53426,]
#Part2
df1b <- df[53427:106852,]
#Save new dataframes
save(df1a, file = "df1a.Rda")
save(df1b, file = "df1b.Rda")
#Purpose of Clustering
#Identify feature space for finding structures of features
#Increase memory size to 70 Gb
memory.size(max = 70000)
#Import cluster package
library(cluster)
#Create dissimilarity matrix
#Gower coefficient for finding distance between mixed variables
daisy1 <- daisy(df, metric = "gower", type = list(ordratio = c(1:11))) 
#Pam algorithm with 3 clusters 
k1answers <- pam(daisy1, 3, diss = TRUE)
#Evaluate k-mediod clustering algorithm with 2 to 6 clusters


#Option 2: CLARA function
#Exclude dissimilarity matrix
library(cluster)
client1.clara <- clara(Store1, 3, metric = "euclidean", stand = FALSE, samples = 100,
                sampsize = (1331), rngR = FALSE, pamLike = TRUE)
#Get results
#Get number in each sample
client1.clara$sample
#Get medoids?
client1.clara$medoids
#Get each cluster with width and neighbor
client1.clara$silinfo
#Create table showing user.id in cluster - WRONG 
client1table <- table(Store1$userid, client1.clara$cluster)
#Show results
client1table
#Coerce object to data.frame
clust1groups <- as.data.frame(client1table, row.names = NULL)
#Create .CSV file 
write.table(client1table, file <- "/Users/scdavis6/Desktop/ClaraClient1Solutions.csv")


#Visualizations
#histogram Age - DONE
res <- ordered(Store1$Age, levels = c("18-20", "21-24", "25-34", 
                                      "35-44", "45-54", "55-64", "65+"))

#Set dimensions
par(mar=c(6,6,2,0.5))
#Create plot
plot(res, main = "Distribution of Age Client 1", xlab = "", ylab = "", las=2, ylim = c(0,20000))
mtext(text="Age", side=1, line=5)
mtext(text="Density", side=2, line=5)



#histogram Gender - DONE
plot(Store1$Gender, main = "Distribution of Gender Client 1", xlab = "Gender", 
     ylab = "Density", ylim = c(0,60000))

#histogram HomeOwnerStatus - DONE
par(mar=c(5,5,3,1))

plot(Store1$HomeOwnerStatus, main = "Distribution of Home Ownership Client 1", 
     xlab = "Home Ownership", ylab = "Density", ylim = c(0,30000))


#Plot of Household Income - DONE 
res <- ordered(Store1$HouseholdIncome, levels = c("0-15k", "15k-25k", "25k-35k", 
                                                  "35k-50k", "50k-75k", "75k-100k", 
                                                  "100k-125k", "125k-150k", "150k-175k", 
                                                  "175k-200k", "200k-250k", "250k+"))
#Set dimensions
par(mar=c(7,5,4,1))
#Create plot
plot(res, main = "Distribution of Household Income Client 1", xlab = "", 
     ylab = "", las=2, ylim = c(0,12000))
mtext(text="HouseholdIncome", side=1, line=5.5)
mtext(text="Density", side=2, line=4)



#histogram of Marital Status - DONE 
plot(Store1$MaritalStatus, main = "Distribution of Marital Status Client 1", xlab = "Marital Status", 
     ylim = c(0,35000), ylab = "Density")

#histogram of prescence of children - DONE 
plot(Store1$PresenceofChildren, main = "Distribution of Children Prescence Client 1", xlab = "Children Prescence", 
     ylim = c(0,45000), ylab = "Density")

#histogram Home Market Value - DONE 
#Set the order
res1 <- ordered(Store1$HomeMarketValue, levels = c("1k-25k", "25k-50k", "50k-75k",
                                                   "75k-100k", "100k-150k",
                                                   "150k-200k", "200k-250k", 
                                                   "250k-300k", "300k-350k", 
                                                   "350k-500k", "500k-1mm", "1mm+"))
#Set dimensions
par(mar = c(7,5,4,1))
#Create plot
plot(res1, main = "Distribution of Home Market Value Client 1", 
     xlab = "", ylab = "", las = 2, ylim = c(0,10000))
mtext(text = "Home Market Value", side = 1, line = 5.5)
mtext(text = "Density", side = 2, line = 4)



#histogram Occupation - DONE
#set the order
res2 <- ordered(Store1$Occupation, levels = c("Blue Collar Worker", "Business Owner", 
                                              "Civil Service", "Exec/Upper Manage", 
                                              "Health Services", "Homemaker", "Middle Manage", 
                                              "Military Personnel", "Nurse", "Part Time", 
                                              "Professional", "Retired", "Secretary", 
                                              "Student", "Teacher", "Technology", 
                                              "White Collar Worker"))
#Set dimensions
par(mar=c(11,5,2,1))
#Create plot
plot(res2, main = "Distribution of Occupation Client 1", 
     xlab = "", ylab = "", las = 2, ylim = c(0,6000))
mtext(text = "Home Market Value", side = 1, line = 10)
mtext(text = "Density", side = 2, line = 4)


#histogram education
res3 <- ordered(Store1$Education, levels = c("Completed High School", "Attended College", 
                                             "Completed College", "Completed Graduate School", 
                                             "Attended Vocational/Technical"))
#Set dimensions
par(mar=c(14,5,2,1))
#Create plot
plot(res3, main = "Distribution of Education  Client 1", 
     xlab = "", ylab = "", las = 2, ylim = c(0,20000))
mtext(text = "Education", side = 1, line = 12)
mtext( text = "Density", side = 2, line = 4)


#histogram length of residence - DONE 
res4 <- ordered(Store1$LengthofResidence, levels = c("Less than 1 year", "1 Year",  
                                                     "2 Years",  "3 Years",  "4 Years", 
                                                     "5 Years",  "6 Years",  "7 Years",  
                                                     "8 Years",  "9 Years",  "10 Years",  
                                                     "11-15 years", "16-19 years",  "20+ years"))
#Set dimensions
par(mar=c(10,5,2,1))
#Create plot
plot(res4, main = "Distribution of Length of Residence Client 1", 
     xlab = "", ylab = "", las = 2, ylim = c(0,10000))
mtext(text = "Length of Residence", side = 1, line = 8)
mtext(text = "Density", side = 2, line = 3.5)



#Fill in data with mode from histograms
#Other method - works 
indx1 <- which(is.na(df), arr.ind=TRUE)
df[indx1] <- c("25-34", "Male", "50k-75k", "Single",
                "No", "Own", "150k-200k", "Professional",
                "Completed High School")[indx1[,2]]


#Get the results
#Load Data
Store1 <- read.csv("/Users/scdavis6/Documents/Work/TowerData/Data/Client1.csv", 
                   na.strings = "", head = TRUE)
#Clean up titles of data.frame - CORRECT ANSWER FROM FORUM
names(Store1) <- gsub("_|\\.\\.\\.|\\." , "", names(Store1))
#Remove other columns
Store1new <- Store1[c(-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,
                      -29,-30,-31,-32,-33,-34,-35,-36)] 
#Remove all rows with only NA values
Store1df <- Store1new[!!rowSums(!is.na(Store1new)),]
