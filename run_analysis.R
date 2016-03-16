setwd("/Users/telfreth/Desktop/Classes/Week4")
library(plyr)
library(knitr)

# Download the zip file and put it in my working directory.

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

unzip(zipfile="./data/Dataset.zip",exdir="./data")

path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)

### Read all data into R so the training and test sets can be merged. 
dataActivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)
dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
dataSubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)
dataFeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)

## Update the DataActivityTest & dataActivityTrain

temp <- as.data.frame(lapply(dataActivityTest, function(y) gsub("1", "WALKING", y)))
temp2<- as.data.frame(lapply(temp, function(y) gsub("2", "WALKING_UPSTAIRS", y)))
temp3 <- as.data.frame(lapply(temp2, function(y) gsub("3", "WALKING_DOWNSTAIRS", y)))
temp4 <- as.data.frame(lapply(temp3, function(y) gsub("4", "SITTING", y)))
temp5 <- as.data.frame(lapply(temp4, function(y) gsub("5", "STANDING", y)))
finalDataActivityTest <- as.data.frame(lapply(temp5, function(y) gsub("6", "LAYING", y)))

temp <- as.data.frame(lapply(dataActivityTrain, function(y) gsub("1", "WALKING", y)))
temp2<- as.data.frame(lapply(temp, function(y) gsub("2", "WALKING_UPSTAIRS", y)))
temp3 <- as.data.frame(lapply(temp2, function(y) gsub("3", "WALKING_DOWNSTAIRS", y)))
temp4 <- as.data.frame(lapply(temp3, function(y) gsub("4", "SITTING", y)))
temp5 <- as.data.frame(lapply(temp4, function(y) gsub("5", "STANDING", y)))
finalDataActivityTrain <- as.data.frame(lapply(temp5, function(y) gsub("6", "LAYING", y)))

### Merge the training and the test sets

dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataActivity<- rbind(finalDataActivityTrain, finalDataActivityTest)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)

### Appropriately label the data set with descriptive variable names

names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)
names(dataFeatures)<- dataFeaturesNames$V2

### Combine the above three data frames and get the final data frame

dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)

### Extract only the measurements on the mean and standard deviation 

subdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
Data<-subset(Data,select=selectedNames)

### Appropriaetly label the data set with descriptive variable names

names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

### From the data in the last step, create a second, independent tidy data set with the average of each variable 
### for each activity and each subject. 

Data2<-aggregate(. ~subject + activity, Data, mean)
Data2<-Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)
