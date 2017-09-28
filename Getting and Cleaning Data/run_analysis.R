


library(dplyr)

## download data

setwd("C:/Users/siarkmi2/Documents/GitHub/Cursera/Getting and Cleaning Data")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "data.zip"

if (!file.exists(fileName)){
        download.file(url, fileName)
}

filePath <-"UCI HAR Dataset"

if (!file.exists(filePath)){
        unzip(fileName) 
}

## load data 
# training set
tPh <- "train"
trainSubjects <- read.table(file.path(filePath, tPh, "subject_train.txt"))
trainData <-     read.table(file.path(filePath, tPh, "X_train.txt"))
trainActivity <- read.table(file.path(filePath, tPh, "y_train.txt"))

# test data
tPh <- "test"
testSubjects <- read.table(file.path(filePath, tPh, "subject_test.txt"))
testData <-     read.table(file.path(filePath, tPh, "X_test.txt"))
testActivity <- read.table(file.path(filePath, tPh, "y_test.txt"))

# activity labels
activities <- read.table(file.path(filePath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# read features, don't convert text labels to factors
features <- read.table(file.path(filePath, "features.txt"), as.is = TRUE)


## merge data  for training and test datasets then merge together

humanActivity <- rbind(
        cbind(trainSubjects, trainData,trainActivity),
        cbind(testSubjects, testData, testActivity)
)

# add colum names
colnames(humanActivity) <- c("subject", features[,2], "activity")

## Extract only the measurements on the mean and standard deviation for each measurement. 

pattern <- "subject|activity|\\bmean()\\b|std"
columnsToKeep <- grepl(pattern, colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

## Use descriptive activity names to name the activities in the data set

humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

## Appropriately label the data set with descriptive variable names.

humanActivityCol <- colnames(humanActivity)
humanActivityCol <- gsub("[\\(\\)-]","", humanActivityCol)
humanActivityCol <- gsub("^f", "frequencyDomain", humanActivityCol)
humanActivityCol <- gsub("^t", "timeDomain", humanActivityCol)
humanActivityCol <- gsub("Acc", "Accelerometer", humanActivityCol)
humanActivityCol <- gsub("Gyro", "Gyroscope", humanActivityCol)
humanActivityCol <- gsub("Mag", "Magnitude", humanActivityCol)
humanActivityCol <- gsub("mean", "Mean", humanActivityCol)
humanActivityCol <- gsub("std", "StandardDeviation", humanActivityCol)
humanActivityCol <- gsub("BodyBody", "Body", humanActivityCol)

colnames(humanActivity) <- humanActivityCol

## create a second, independent tidy data set with the average of each variable for each activity and each subject.

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)




