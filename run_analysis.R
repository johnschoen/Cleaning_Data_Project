
# Clean up workspace
rm(list=ls())

## installing packages and libraries
library(plyr) 

path=getwd()

## downloading data zip file
url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file="Data.zip"
download.file(url,file.path(path,file))

## check file names in the zip folder
unzip("Data.zip",list=T)

# #Read the data

x_test <- read.table("data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt")

x_train <- read.table("data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt")

#1. Merge the data sets. (NOTE: We chose to perform the join the subsets at a later stage, clenaing and filtering them seprately first)

# Combine X, y subject data sets before joining them 

x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)


#STEP 2 Extract only the measurements on the mean and standard deviation for each measurement

features <- read.table("data/UCI HAR Dataset/features.txt")

# find/filter the columns with mean() or std()
mean_and_std_features <- grep(".*mean.*|.*std.*", features[,2])
x_data <- x_data[, mean_and_std_features]

# rename columns
names(x_data) <- features[mean_and_std_features, 2]

# STEP 3 Use descriptive activity names 

# read from activity_label file and update column names

activities <- read.table("data/UCI HAR Dataset/activity_labels.txt")
y_data[, 1] <- activities[y_data[, 1], 2]
names(y_data) <- "activity"

# STEP 4 Appropriately label the data set with descriptive variable names

# pull column names from subject_data
names(subject_data) <- "subject"

# Now join the tables into one dataset 
all_data <- cbind(x_data, y_data, subject_data)

# STEP 4. Appropriately label the data set with descriptive activity names. 

# Loop through the untidy names and assign cleaner labels

colNames  = colnames(all_data); 

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std-X","StdDevX",colNames[i])
  colNames[i] = gsub("-std-Y","StdDevY",colNames[i])
  colNames[i] = gsub("-std-Z","StdDevZ",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  
};

colnames(all_data) = colNames;

# STEP 5: Create a second, independent tidy data set with the average of each variable
# for each activity and each subject

tidy_averages <- aggregate(. ~ subject + activity, data=all_data, FUN = "mean")

#saving txt file 
write.table(tidy_averages, "data/UCI HAR Dataset/tidy_data.txt", row.name=FALSE)
