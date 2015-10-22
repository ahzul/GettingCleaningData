setwd("D:/SIU 2/Big Data Course/03 - Getting & Cleaning Data/Assignment/UCI HAR Dataset")
## The libraries used in this operation is dplyr
library(dplyr)
## The supporting metadata in this data are the name of the features and the name of the activities. They are loaded into variables featureNames and activityLabels
featureNames <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt", header = FALSE)
## Both training and test data sets are split up into subject, activity and features. They are present in three different files.
## Read Training Data
subjectTrain <- read.table("train/subject_train.txt", header = FALSE)
activityTrain <- read.table("train/y_train.txt", header = FALSE)
featuresTrain <- read.table("train/X_train.txt", header = FALSE)
## Read Test Data
subjectTest <- read.table("test/subject_test.txt", header = FALSE)
activityTest <- read.table("test/y_test.txt", header = FALSE)
featuresTest <- read.table("test/X_test.txt", header = FALSE)
## Part 1 - Merge the training and the test sets to create one data set
## We can use combine the respective data in training and test data sets corresponding to subject, activity and features. The results are stored in subject, activity and features
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
## Naming the columns to featureNames
colnames(features) <- t(featureNames[2])
## Merge the data in features, activity & subject and stored in completeData 
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)
## Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement
## Extract the column indices that have either mean or std in them.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
## Add activity and subject columns to the list and look at the dimension of completeData
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
## We create extractedData with the selected columns in requiredColumns. And again, we look at the dimension of requiredColumns.
extractedData <- completeData[,requiredColumns]
dim(extractedData)
## Part 3 - Uses descriptive activity names to name the activities in the data set
## The activity field in extractedData is originally of numeric type. We need to change its type to character so that it can accept activity names. The activity names are taken from metadata activityLabels
extractedData$Activity <- as.character(extractedData$Activity) for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
## We need to factor the activity variable, once the activity names are updated.
extractedData$Activity <- as.factor(extractedData$Activity)
## Part 4 - Appropriately labels the data set with descriptive variable names in extractedData
names(extractedData)
## Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
## Firstly, let us set Subject as a factor variable.
extractedData$Subject <- as.factor(extractedData$Subject)
## We create tidyData as a data set with average for each activity and subject. Then, we order the enties in tidyData and write it into data file Tidy.txt that contains the processed data
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)