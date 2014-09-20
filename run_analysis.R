#Read in the training data
setwd("~/data/Project1/UCI HAR Dataset/train")
x_train<-read.table("X_train.txt", header = FALSE, sep = "")
y_train<-read.csv("y_train.txt", header=FALSE)
subject_train<-read.csv("subject_train.txt", header=FALSE)

#Read in the test data
setwd("~/data/Project1/UCI HAR Dataset/test")
x_test<-read.table("X_test.txt", header = FALSE, sep = "")
y_test<-read.csv("y_test.txt", header=FALSE)
subject_test<-read.csv("subject_test.txt", header=FALSE)

##1. Merge the test and training data sets
alldata<-rbind(x_train, x_test)

##2. Extract only the measurements on the mean and standard deviation for each measurement
#Read in column names from the features data
setwd("~/data/Project1/UCI HAR Dataset")
features<-read.csv("features.txt", header=FALSE, sep="", colClasses = c("numeric", "character"))

#Assign features to column names
colnames(alldata)<-features[,2]

#Select the mean and standard deviation columns
library(dplyr)
mean_std_data<-select(alldata, matches("mean()"), matches("std()"), -contains("angle"), -contains("meanFreq"))

##3. Use descriptive activity names to name the activities in the data set
library(plyr)

#Combine subject IDs and make it a vector
subject<-rbind(subject_train, subject_test)
subject<-subject[,1]

#Combine activity labels and assign descriptive names
activity_labels<-read.csv("activity_labels.txt", header=FALSE, sep="", colClasses=c("numeric", "character"))
activity<-rbind(y_train, y_test)
activity<-mapvalues(activity[,1], 1:6, activity_labels[,2])

#Bind subject and activity vectors to mean_std_data set to make a new data set arranged by subject then activity
subject_activity_data<-cbind(subject, activity, mean_std_data)
subject_activity_data<-arrange(subject_activity_data, subject, activity)

##4. Appropriately label the data set with descriptive variable names
varnames<-colnames(subject_activity_data)
varnames<-tolower(varnames)
varnames<-sub("^t", "time_of", varnames)
varnames<-sub("^f", "frequency_of", varnames)
varnames<-gsub("acc", "_acceleration", varnames)
varnames<-gsub("jerk", "_jerk", varnames)
varnames<-gsub("mag", "_magnitude", varnames)
varnames<-gsub("body", "_body", varnames)
varnames<-gsub("gyro", "_gyroscopic", varnames)
varnames<-gsub("gravity", "_gravity", varnames)
varnames<-gsub("[()]", "", varnames)
varnames<-sub("std", "_standard_deviation", varnames)
varnames<-sub("mean", "_mean", varnames)
varnames<-sub("-x", " (x_axis)", varnames)
varnames<-sub("-y", " (y_axis)", varnames)
varnames<-sub("-z", " (z_axis)", varnames)
varnames<-gsub("-", "", varnames)
desc_var_names<-subject_activity_data
colnames(desc_var_names)<-varnames

##5. Create a second, independent tidy data set with the average of each variable for each activity and each subject
library(reshape2)
tidy_melt<-melt(desc_var_names, id.vars=c("subject", "activity"), na.rm=TRUE)
tidy_data<-dcast(tidy_melt, subject + activity ~ varnames[3:length(varnames)], mean)

#Make .csv file of clean data
setwd("~/data/Project1")
write.table(tidy_data,file="tidy_data.csv",sep=",",row.names=F) 