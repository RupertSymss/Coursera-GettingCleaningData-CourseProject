##########################################################################################

# Coursera Getting and Cleaning Data Course Project
# Rupert Symss
# 2015-07-26

# run_analysis.R File Description:

# This script will do the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

###########################################################################################


# clean workspace
rm(list=ls())

# 1. Merges the training and the test sets to create one data set.

#set working directory to the UCI HAR Dataset
setwd('F:/Data Science/3GettingAndCleaningData/Proj/UCI HAR Dataset');

# Read data from files
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assigin column names to imported data
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

# Create the final training dataset by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);
# Create the final test dataset by merging yTest, subjectTest, and xTest
testData = cbind(yTest,subjectTest,xTest);


# Combine training and test data get the final data set
mergedData = rbind(trainingData,testData);


# 2. Extract measurements on the mean and standard deviation. 

mergedData <- mergedData[c("activityId", "subjectId", "tBodyAccMag-mean()", "tBodyAccMag-std()", "tGravityAccMag-mean()", "tGravityAccMag-std()", "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()", "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()", "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()", "fBodyBodyAccJerkMag-std()", "fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()", "fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()")]

# 3. Use activity names to name the activities in the dataset

# Merge the mergedData set with acitivityType table to include activity names
mergedData = merge(mergedData,activityType,by='activityId',all.x=TRUE);

# the colNames vector where we will clean the column names
colNames  = colnames(mergedData); 

# 4. Label the data set with activity names. 

colNames = gsub("JerkMag","JerkMagnitude",colNames)
colNames = gsub("-mean","Mean",colNames)
colNames = gsub("[Gg]yro","Gyroscope",colNames)
colNames = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames)
colNames = gsub("([Bb]odyaccjerkmag)","BodyAccelerometerJerkMagnitude",colNames)
colNames = gsub("GyroMag","GyroscopeMagnitude",colNames)
colNames = gsub("^(f)","frequency",colNames)
colNames = gsub("([Gg]ravity)","Gravity",colNames)
colNames = gsub("AccMag","AccelerometerMagnitude",colNames)
colNames = gsub("^(t)","time",colNames)
colNames = gsub("\\()","",colNames)
colNames = gsub("-std$","StandardDeviation",colNames)


# Assigning the tidy column names to mergedData 
colnames(mergedData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# remove the activityType column
mergedData  = mergedData[,names(mergedData) != 'activityType'];

# Summarizing the mergedData table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(mergedData[,names(mergedData) != c('activityId','subjectId')],by=list(activityId=mergedData$activityId,subjectId = mergedData$subjectId),mean);

# Merging tidyData with activityType to include acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');