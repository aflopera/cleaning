## The directory "UCI HAR Dataset" was renamed for "UCI_HAR_Dataset"

##Load features (names of the variables)
features <- read.delim("UCI_HAR_Dataset/features.txt", header= FALSE, sep="", stringsAsFactors = F)

##Load the test data
X_test <- read.delim("UCI_HAR_Dataset/test/X_test.txt", header= FALSE, sep="")

##2. Extracts only the measurements on the mean and standard deviation for each measurement.
X_test <- X_test[,grepl("mean|std",features$V2)]

##set the name of the columns
colnames(X_test) <- grep("mean|std",features$V2, value = T)

##Load the activity
y_test <- read.delim("UCI_HAR_Dataset/test/y_test.txt", header= FALSE, sep="", col.names= c("Activity"))

##Load the subject
subject_test <- read.delim("UCI_HAR_Dataset/test/subject_test.txt", header= FALSE, sep="", col.names= c("Subject"))

##Adds label and subject to the data frame with the test data
X_test <- cbind(subject_test, y_test, X_test)

##Load the train data
X_train <- read.delim("UCI_HAR_Dataset/train/X_train.txt", header= FALSE, sep="")

##2. Extracts only the measurements on the mean and standard deviation for each measurement.
X_train <- X_train[,grepl("mean|std",features$V2)]

##set the name of the columns
colnames(X_train) <- grep("mean|std",features$V2, value = T)

##Load the activity
y_train <- read.delim("UCI_HAR_Dataset/train/y_train.txt", header= FALSE, sep="", col.names= c("Activity"))

##Load the subject
subject_train <- read.delim("UCI_HAR_Dataset/train/subject_train.txt", header= FALSE, sep="", col.names= c("Subject"))

##Adds label and subject to the data frame with the train data
X_train <- cbind(subject_train, y_train, X_train)

##1. Merges the training and the test sets to create one data set.
data<- rbind(X_train, X_test)

##Load Activity descriptions
activity_labels <- read.delim("UCI_HAR_Dataset/activity_labels.txt", header= FALSE, sep="", stringsAsFactors = F, col.names= c("Activity", "Activity_Description"))

##3. Uses descriptive activity names to name the activities in the data set
##Join between the data set and the activity description. The key is the activity code.
mergedData <- merge(activity_labels, data, by.x="Activity", by.y = "Activity", all=T)

##Drop the activity code, because now we have the activity descripcion
mergedData <- select(mergedData, -Activity)

##4. Appropriately labels the data set with descriptive variable names.
nombres <- names(mergedData)
nombres <- sub("^t", "time_", nombres)
nombres <- sub("^f", "frequency_", nombres)
nombres <- sub("Acc", "_acceleration_", nombres)
nombres <- sub("-meanFreq(.*)-X", "_mean_frequency_x_axis", nombres)
nombres <- sub("-meanFreq(.*)-Y", "_mean_frequency_y_axis", nombres)
nombres <- sub("-meanFreq(.*)-Z", "_mean_frequency_z_axis", nombres)
nombres <- sub("-mean(.*)-X", "_mean_x_axis", nombres)
nombres <- sub("-mean(.*)-Y", "_mean_y_axis", nombres)
nombres <- sub("-mean(.*)-Z", "_mean_z_axis", nombres)
nombres <- sub("-std(.*)-X", "_standard_deviation_x_axis", nombres)
nombres <- sub("-std(.*)-Y", "_standard_deviation_y_axis", nombres)
nombres <- sub("-std(.*)-Z", "_standard_deviation_z_axis", nombres)
nombres <- sub("-meanFreq(.*)$", "_mean_frequency", nombres)
nombres <- sub("-mean(.*)$", "_mean", nombres)
nombres <- sub("-std(.*)$", "_standard_deviation", nombres)
nombres <- sub("__", "_", nombres)
names(mergedData) <- nombres

##
## 5. creates a second, independent tidy data set with the average of each variable
## for each activity and each subject.
##
summary <- group_by(mergedData, Subject, Activity_Description)
summary <- summarize_all(summary,  funs(mean))
