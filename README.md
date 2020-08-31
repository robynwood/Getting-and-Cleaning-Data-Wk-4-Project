# Getting-and-Cleaning-Data-Wk-4-Project
#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#You should create one R script called run_analysis.R that does the following.

#1.  Merges the training and the test sets to create one data set.

install.packages("data.table")
setwd("~/Desktop/Data_Science/Getting and Cleaning Data/UCI HAR Dataset")
trainingdirectory <- getwd()

## read the training data (the big data set; 7,352 rows, 561 columns)
training_data <- read.table("./train/X_Train.txt")
## read the training Activity designation (1 column, name it "Activity")
training_activities <- read.table("./train/Y_Train.txt", col.names = c("Activity"))
## read the subject designation (1 column, name it "SubjectID")
training_subjects <- read.table("./train/subject_train.txt", col.names = c("SubjectID"))
## merge all the training sets together (so now we have 563 columns)
training_data_all <- cbind(training_subjects, training_activities, training_data)

## read the test data (the big data set; 2,947 rows, 561 columns)
test_data <- read.table("./test/X_test.txt")
## read the test Activity designation (1 column, name it "Activity")
test_activities <- read.table("./test/Y_test.txt", col.names = c("Activity"))
## read the subject designation (1 column, name it "SubjectID")
test_subjects <- read.table("./test/subject_test.txt", col.names = c("SubjectID"))
## merge all the test sets together (so now we have 563 columns)
test_data_all <- cbind(test_subjects, test_activities, test_data)

## Merge the test and training rows into one data set (still 563 columns, 
## but now will have 2,947 + 7,352 = 10,299 data points/rows)
test_and_training <- rbind(training_data_all, test_data_all)

#2.  Extracts only the measurements on the mean and standard deviation 
#    for each measurement.

## The names of each of the columns in "test_and_training" are listed in the 
## "features.txt" file in the "UCI HAR Dataset" Folder.  I want to pick only 
## the columns that have the characters "Mean" or "std" in them (plus 
## "SubjectID" and "Activity" columns ), and write them to a new data set I'll 
## call "mean_std_data_total2"

## First, create a table from the features.txt file to break into 2 columns, 
## labeled as "column_index" and "feature_name".  
features <- read.table("./features.txt", col.names = c("column_index",
        "feature_name"))
## Now pick out the elements from the "features" table that have "mean" or "std" 
## in them.  This returns an index of which columns are relevant (86 columns).
mean_std_data_index <- grep("([Mm]ean|[Ss]td)", features$feature_name)
## Make a smaller data set of "test_and_training" to eliminate the 1st 2 columns
test_and_training_minus12 <- test_and_training[,3:563]
## Now pick those columns out of test_and_training_minus12 and put them in a 
## smaller data set 
mean_std_data_total <- test_and_training_minus12[, mean_std_data_index]
## Add back the first two column_index and feature_name columns to the front.
first_two_columns <- test_and_training[,1:2] 
mean_std_data_total2 <- cbind(first_two_columns, mean_std_data_total)

#3.  Uses descriptive activity names to name the activities in the data set

## This means I need to replace the 1, 2, 3, 4, 5, and 6 in the 2nd Column
## (column name = "Activity"), with the labels defined in "activity_labels.txt"
## file in the "UCI HAR Dataset" folder.  The activity_labels.txt file has 2 
## "columns" - the first is the activity number, and the second is the descriptive 
## name.  1 = Walking, 2 = Walking_Upstairs, 3 = Walking_Downstairs, 4 = Sitting, 
## 5 = Standing, and 6 = Laying.  

####this part I want to be able to use but wasn't able to figure it out.
####lookuptable <- read.table("./activity_labels.txt", col.names = c("Activity",
####        "activity_description"))

mean_std_data_total2$Activity = gsub("1", "walking", mean_std_data_total2$Activity)
mean_std_data_total2$Activity = gsub("2", "walking_upstairs", mean_std_data_total2$Activity)
mean_std_data_total2$Activity = gsub("3", "walking_downstairs", mean_std_data_total2$Activity)
mean_std_data_total2$Activity = gsub("4", "sitting", mean_std_data_total2$Activity)
mean_std_data_total2$Activity = gsub("5", "standing", mean_std_data_total2$Activity)
mean_std_data_total2$Activity = gsub("6", "laying", mean_std_data_total2$Activity)

#4.  Appropriately labels the data set with descriptive variable names.

## So I need to use the variable names in "features.txt" and name the columns
## accordingly (only of the ones with mean or std in the term!).  I'll call the
## final table "finaltable".

finaltable <- mean_std_data_total2
colnames(finaltable)[3:88] <- features$feature_name[mean_std_data_index]

#5.  From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

## Using the "aggregate function"
tidybySubjectIDandActivity <- aggregate(finaltable[,3:88], by=list(SubjectID = 
        finaltable$SubjectID, Activity = finaltable$Activity), FUN = mean, na.rm=TRUE)
