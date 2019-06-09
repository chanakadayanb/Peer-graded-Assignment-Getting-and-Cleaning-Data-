
library(dplyr)
## 1. Merges the training and the test sets to create one data set.
## Download Data Set  

dir.create("./dataset")

DataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(DataUrl,destfile="./dataset/Dataset.zip")

## unzip the data set 
unzip(zipfile="./dataset/Dataset.zip",exdir="./dataset")


features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

## Test set 
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

## Train  set
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

## MErging  to a one set  calling fullset 

m_train <- cbind(y_train, subject_train, x_train)
m_test <- cbind(y_test, subject_test, x_test)
fullset <- rbind(m_train, m_test)

fullset
dim(fullset)

## 2.Extracts only the measurements on the mean and standard deviation for each measurement

MeanandStd <- fullset %>% select(subject, code, contains("mean"), contains("std"))

MeanandStd

##Step 3: Uses descriptive activity names to name the activities in the data set.

MeanandStd$code <- activities[MeanandStd$code, 2]
MeanandStd$code

##Step 4: Appropriately labels the data set with descriptive variable names

names(MeanandStd)[2] = "activity"
names(MeanandStd)<-gsub("Acc", "Accelerometer", names(MeanandStd))
names(MeanandStd)<-gsub("Gyro", "Gyroscope", names(MeanandStd))
names(MeanandStd)<-gsub("BodyBody", "Body", names(MeanandStd))
names(MeanandStd)<-gsub("Mag", "Magnitude", names(MeanandStd))
names(MeanandStd)<-gsub("^t", "Time", names(MeanandStd))
names(MeanandStd)<-gsub("^f", "Frequency", names(MeanandStd))
names(MeanandStd)<-gsub("tBody", "TimeBody", names(MeanandStd))
names(MeanandStd)<-gsub("-mean()", "Mean", names(MeanandStd), ignore.case = TRUE)
names(MeanandStd)<-gsub("-std()", "STD", names(MeanandStd), ignore.case = TRUE)
names(MeanandStd)<-gsub("-freq()", "Frequency", names(MeanandStd), ignore.case = TRUE)
names(MeanandStd)<-gsub("angle", "Angle", names(MeanandStd))
names(MeanandStd)<-gsub("gravity", "Gravity", names(MeanandStd))

##Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
TidyData <- MeanandStd %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean))
write.table(TidyData, "TidyData.txt", row.name=FALSE)

