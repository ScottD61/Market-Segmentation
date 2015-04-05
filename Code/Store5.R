#Load Data
Store5 <- read.csv("/Users/scdavis6/Documents/Work/TowerData/Data/Client5.csv", 
                   na.strings = "", head = TRUE)
#Clean up titles of data.frame - CORRECT ANSWER FROM FORUM
names(Store5) <- gsub("_|\\.\\.\\.|\\." , "", names(Store5))
#Identify which rows have only NA values
which(rowSums(is.na(Store5))==ncol(Store5))
#Remove other columns
Store5new <- Store5[c(-1,-11,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,
                      -29,-30,-31,-32,-33,-34,-35)] 
#Remove all rows with only NA values
Store5df <- Store5new[!!rowSums(!is.na(Store5new)),]
#Fill in data with mode from histograms
#Other method - works 
indx5 <- which(is.na(df5), arr.ind=TRUE)
df5[indx5] <- c("35-44", "Female", "150k-175k", "Single",
                "No", "Own", "500k-1mm", "Professional",
                "Completed College")[indx5[,2]]
#Convert csv to data.frame
df5 <- as.data.frame(Store5df)
#Save data.frame
save(df5,file="df5.Rda")
#View data.frame
head(df5)
#Info about data.frame
str(df5)
#Write df as csv
write.table(df5, file = "/Users/scdavis6/Desktop/df5.csv")

#Import cluster package
library(cluster)
#Create dissimilarity matrix
#Gower coefficient for finding distance between mixed variable
daisy5 <- daisy(df5, metric = "gower", type = list(ordratio = c(1:11))) 
#Pam algorithm with 3 clusters 
k5answers <- pam(daisy5, 3, diss = TRUE, mediods = NULL)
#Get number of observations per cluster
k5answers$id.med
#Information about clusters
k5answers$clusinfo
#Group row with cluster
Groups5 <- k5answers$clustering
#Coerce object to data.frame
clustgroups5 <- as.data.frame(Groups5, row.names = NULL)
#Create .CSV file
write.table(clustgroups5, file <- "/Users/scdavis6/Desktop/Solutions5.csv")

#Create visualizations
#histogram Age 
res <- ordered(Store5$Age, levels = c("18-20", "21-24", "25-34", 
                                      "35-44", "45-54", "55-64", "65+"))

#Set dimensions
par(mar=c(6,5,4,1))
#Create plot
plot(res, main = "Distribution of Age Client 5", xlab = "", ylab = "", las=2, ylim = c(0,10000))
mtext(text="Age", side=1, line=4)
mtext(text="Density", side=2, line=4)


#histogram Gender  
plot(Store5$Gender, main = "Distribution of Gender Client 5", xlab = "Gender", 
     ylab = "Density", ylim = c(0,35000))


#histogram HomeOwnerStatus
plot(Store5$HomeOwnerStatus, main = "Distribution of Home Ownership Client 5", xlab = "Home Ownership", 
     ylab = "Density", ylim = c(0,30000))


#Plot of Household Income 
res <- ordered(Store5$HouseholdIncome, levels = c("0-15k", "15k-25k", "25k-35k", 
                                                  "35k-50k", "50k-75k", "75k-100k", 
                                                  "100k-125k", "125k-150k", "150k-175k", 
                                                  "175k-200k", "200k-250k", "250k+"))
#Set dimensions
par(mar=c(8,5,2,1))
#Create plot
plot(res, main = "Distribution of Household Income Client 5", xlab = "", 
     ylab = "", las=2, las=2, ylim = c(0,10000))
mtext(text="HouseholdIncome", side=1, line=6)
mtext(text="Density", side=2, line=3.5)


#histogram of Marital Status - DONE 
par(mar=c(5,4,4,1))
plot(Store5$MaritalStatus, main = "Distribution of Marital Status Client 5", xlab = "Marital Status", 
     ylab = "Density", ylim = c(0,20000))


#histogram of prescence of children - DONE
plot(Store5$PresenceofChildren, main = "Distribution of Children Prescence Client 5", xlab = "Children Prescence", 
     ylab = "Density", ylim = c(0,30000))


#histogram Home Market Value - DONE 
#Set the order
res1 <- ordered(Store5$HomeMarketValue, levels = c("1k-25k", "25k-50k", "50k-75k",
                                                   "75k-100k", "100k-150k",
                                                   "150k-200k", "200k-250k", 
                                                   "250k-300k", "300k-350k", 
                                                   "350k-500k", "500k-1mm", "1mm+"))
#Set dimensions
par(mar=c(7,5,4,1))
#Create plot
plot(res1, main = "Distribution of Home Market Value Client 5", 
     xlab = "", ylab = "", las=2, ylim = c(0,12000))
mtext(text="HouseholdIncome", side=1, line=5.5)
mtext(text="Density", side=2, line=3.5)


#histogram Occupation - DONE 
#set the order
res2 <- ordered(Store5$Occupation, levels = c("Blue Collar Worker", "Business Owner", 
                                              "Civil Service", "Executive/Upper Management", 
                                              "Health Services", "Homemaker", "Middle Management", 
                                              "Military Personnel", "Nurse", "Part Time", 
                                              "Professional", "Retired", "Secretary", 
                                              "Student", "Teacher", "Technology", 
                                              "White Collar Worker"))
#Set dimensions
par(mar=c(14,5,2,1))
#Create plot
plot(res2, main = "Distribution of Occupation Client 5", 
     xlab = "", ylab = "", las=2, ylim = c(0,5000))
mtext(text="Occupation", side=1, line=13)
mtext(text="Density", side=2, line=3.5)


#histogram education - DONE
res3 <- ordered(Store5$Education, levels = c("Completed High School", "Attended College", 
                                             "Completed College", "Completed Graduate School", 
                                             "Attended Vocational/Technical"))
#Set dimensions
par(mar=c(13,5,1.5,1))
#Create plot
plot(res3, main = "Distribution of Education Client 5", 
     xlab = "", ylab = "", las=2, ylim = c(0,14000))
mtext(text="Education", side=1, line=12)
mtext(text="Density", side=2, line=4)


#histogram length of residence - DONE 
res4 <- ordered(Store5$LengthofResidence, levels = c("Less than 1 year", "1 Year",  
                                                     "2 Years",  "3 Years",  "4 Years", 
                                                     "5 Years",  "6 Years",  "7 Years",  
                                                     "8 Years",  "9 Years",  "10 Years",  
                                                     "11-15 years", "16-19 years",  "20+ years"))
#Set dimensions
par(mar=c(9,5,3,1))
#Create plot
plot(res4, main = "Distribution of Length of Residence Client 5", 
     xlab = "", ylab = "", las=2, ylim = c(0,8000))
mtext(text="Length of Residence", side=1, line=7)
mtext(text="Density", side=2, line=3.5)