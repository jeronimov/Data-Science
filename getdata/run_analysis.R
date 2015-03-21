# ---------------------------------------------------------------------
# This script takes data collected by UCI HAR (Human Activity Recognition 
# Using Smartphones) from the accelerometers from the Samsung Galaxy S 
# smartphone and do the following:
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for 
#    each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data 
#    set with the average of each variable for each activity and each subject.
# 
# It is assumed the working directory is already set and this script is situated 
# in the same directory as the source data.
# ---------------------------------------------------------------------

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