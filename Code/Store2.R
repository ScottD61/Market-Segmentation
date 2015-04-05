#Load Data
Store2 <- read.csv("/Users/scdavis6/Documents/Work/TowerData/Data/Client2.csv", 
                   na.strings = "", head = TRUE)
#Clean up titles of data.frame - CORRECT ANSWER FROM FORUM
names(Store2) <- gsub("_|\\.\\.\\.|\\." , "", names(Store2))
#Identify which rows have only NA values
which(rowSums(is.na(Store2))==ncol(Store2))
#Remove other columns
Store2new <- Store2[c(-1,-2,-13,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,
                      -29,-30,-31,-32,-33,-34,-35,-36)] 
#Remove all rows with only NA values
Store2df <- Store2new[!!rowSums(!is.na(Store2new)),]
#Convert csv to data.frame
df2 <- as.data.frame(Store2df)
#Insert mode into NA values
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
#Show row number next to cluster
write.table(clustgroups2, file <- "Users/scdavis6/Desktop/Solutions2.csv")

#Visualizations
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