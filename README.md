---
title: "Readme for Getting and Cleaning Data Course Project"
author: "Jeronimo J Vogt"
date: "Mach 20, 2015"
---

## Project Description
This project goal is to prepare tidy data that can be used for later analysis.
Raw data from Human Activity Recognition Using Smartphones learning repository was transformed into a clean data file that follows the [principles of tidy data](http://vita.had.co.nz/papers/tidy-data.pdf).

To do this a R script called run_analysis.R was created that does the following:
 0. Downloads the .zip data to local folder and unzip it.
 1. Merges both training and test sets to create one data set.
 2. Extracts only the measurements on the mean and standard deviation for 
    each measurement. 
 3. Uses descriptive activity names to name the activities in the data set
 4. Appropriately labels the data set with descriptive variable names. 
 5. From the data set in step 4, creates a second, independent tidy data 
    set with the average of each variable for each activity and each subject.

## Data gathering
### Source data
The source data can be downloaded from [this web page](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
    or directly from [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

###Collection of the raw data
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained data set has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

###Attribute information
For each record in the data set it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

## Data processing
Data processing took place using the run_analysis script. This script takes the raw data and transforms it into a single tidy data set. In this section the code of this script is described.

```{r, results='hide', warning=FALSE, message=FALSE}
### Step 0: Download the file if not already done and unzip it

if (!file.exists("SmartphonesData.zip")){
	url  = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	file = "SmartphonesData.zip"
	meth = "internal"
	quit = TRUE
	mode = "wb"
	download.file(url, file, meth, quit, mode)	
	unzip("SmartphonesData.zip")
}

#Load the required libraries
library(dplyr)
library(tidyr)

### Step 1: Read in test data sets, set initial column names and combine them into a combined data set

#Read in the column names
features <- read.table("./UCI HAR Dataset/features.txt", header=FALSE, stringsAsFactors = FALSE)
features <- as.vector(as.matrix(features$V2))
features <- gsub("-", "_", features)
features <- gsub(",", "_", features)

#Read in the raw data files (test sets)
data_test <- read.table("./UCI HAR Dataset/test/x_test.txt", header=FALSE, stringsAsFactors = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE, stringsAsFactors = FALSE)
activity_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header=FALSE, stringsAsFactors = FALSE)

#Set column names (test sets)
colnames(data_test) <- features
colnames(subject_test) <- c('Subject')
colnames(activity_test) <- c('Activity')

#Bind the different raw data files (test sets)
df_test = cbind(subject_test,activity_test,data_test)

#Read in the raw data files (training sets)
data_train <- read.table("./UCI HAR Dataset/train/x_train.txt", header=FALSE, stringsAsFactors = FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE, stringsAsFactors = FALSE)
activity_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE, stringsAsFactors = FALSE)

#Set column names (training sets)
colnames(data_train) <- features
colnames(subject_train) <- c('Subject')
colnames(activity_train) <- c('Activity')

#Bind the different raw data files (training sets)
df_train = cbind(subject_train,activity_train,data_train)

#Combine both the train and test data sets
df_full <- rbind(df_test, df_train)

### Step 2: Extracts only activity, subject and the measurements columns with mean and standard deviation (std) 

cols <- c(grep("std()",features, ignore.case=TRUE, value=TRUE), grep("mean\\(",features, ignore.case=TRUE, value=TRUE))
df_small <- df_full[c("Subject","Activity",cols)]

### Step 3: Uses descriptive activity names to name the activities in the data set

#Read in activity names and set appropriate column names
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE, stringsAsFactors = FALSE)
colnames(activity_labels) <- c('Activity','ActivityName')

#Merge activity names with small data frame by Activity
df_activity = merge(df_small, activity_labels,by.x="Activity",by.y="Activity",all=TRUE)

#Move new activity names to first column and rename description
df_activity <- df_activity[,c(69, 2:68)]
colnames(df_activity)[1] <- "Activity"

### Step 4: Appropriately labels the data set with descriptive variable names

#Store the columns names in a temporary table
labels <- names(df_activity)

#Change de names with descriptive variable names
labels <- gsub("tBody","Time_Body_", labels)
labels <- gsub("BodyBody","Body", labels)
labels <- gsub("Acc","_Acceleration_", labels)
labels <- gsub("Mag","_Magnitude", labels)
labels <- gsub("Gyro","_Angular_Velocity_", labels)
labels <- gsub("^t","Time_", labels)
labels <- gsub("^f","Frequency_", labels)
labels <- gsub("mean","Mean_Value", labels)
labels <- gsub("std","Standard_Deviation", labels)
labels <- gsub("\\(\\)","", labels)
labels <- gsub("__","_", labels)

#Replace the names in data frame
colnames(df_activity) <- labels

### Step 5: Creates a second independent tidy data set with the average of each variable for each activity and each subject

#Collapse columns into key-value pairs
dftidy <- df_activity %>% gather(Variable, Value, 3:68)

#Aggregate the data
dftidy <- aggregate(Value ~ Variable + Subject + Activity, data=dftidy, mean, na.rm=TRUE) %>%
               select(Subject, Activity, Variable, Value) %>%
			   group_by(Subject, Activity, Variable)
			   
#Write tidy data set to a text file
write.table(dftidy, "tidy_data.txt", row.name=FALSE)
```

#Tidy data file
The result of the data processing were saved in a separate text file called tidy_data.txt and the result consists of 4 variables and 11880 rows. The variables used in the tidy file are the following:

```{r, echo=FALSE}
[1] "Subject"  "Activity" "Variable" "Value" 
```

All variables are detailed described in the [Code Book.md](Codebook.md). Please refer to this document for a more detailed explanation of the variables.

##Sources
1. [UCI Machine learning repostiory](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
