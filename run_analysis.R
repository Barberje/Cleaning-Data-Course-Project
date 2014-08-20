# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# This script produces two outputs, dat_df and tidySet.
# dat_df is the merged test and train data with descriptive column names and activity names.
# tidySet contains measurement averages for each activity and each subject.

# Merge the training and test sets
# read test data, integrate into test_df
test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test_activities <- read.table("UCI HAR Dataset/test/y_test.txt")
test_measurements <- read.table("UCI HAR Dataset/test/X_test.txt")
test_df <- data.frame(test_subjects, test_activities, test_measurements)

# read train data, integrate into train_df
train_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train_activities <- read.table("UCI HAR Dataset/train/y_train.txt")
train_measurements <- read.table("UCI HAR Dataset/train/X_train.txt")
train_df <- data.frame(train_subjects, train_activities, train_measurements)

# combine both test and train data into dat_df
dat_df <- data.frame(rbind(train_df, test_df))

# obtain activity labels
labels <- read.table("UCI HAR Dataset/activity_labels.txt")

labRow <- c()

# create vector of activity labels
for (i in 1:nrow(dat_df)) {
  x <- dat_df[i, 2]
  y <- labels[x, 2]
  labRow <- c(labRow, as.character(y))
}

# merge test, train, and activity label data
dat_df <- data.frame(dat_df[,1:2], labRow, dat_df[3:ncol(dat_df)])

# assemble column names
mNames <- read.table("UCI HAR Dataset/features.txt")
mNames <- mNames[, 2]
mNames <- as.vector(mNames)
mNames <- c("Subject", "ActivityNum", "Activity", mNames)

# add column names to dat_df
names(dat_df) <- mNames

# find all columns containing standard deviation and mean measurements
stdmean_col <- grep("std", mNames, fixed = TRUE)
stdmean_col <- c(stdmean_col, grep("mean", mNames, fixed = TRUE))

# create new dataset
stdmean_dat <- dat_df[, c(1:3, stdmean_col)]

# create second tidy dataset
tidySet <- c()

# loop through each subject, then each activity, 
# then average each measurement column.
for (i in 1:30) {
  subRow <- c()
  x <- subset(stdmean_dat, stdmean_dat$Subject == i)
  for (z in 1:6) {
    mVec <- c()
    y <- subset(x, x$ActivityNum == z)
    for (c in 4:ncol(y)) {
      mVec <- c(mVec, mean(y[, c]))
    }
    actRow <- c(i, z, mVec)
    subRow <- rbind(subRow, actRow)
  }
  tidySet <- rbind(tidySet, subRow)
}

# reformat into a data frame with row numbers
tidySet <- suppressWarnings(data.frame(tidySet, row.names = NULL))

# recreate activity labels column for tidySet
labRow <- c()
for (i in 1:nrow(tidySet)) {
  x <- tidySet[i, 2]
  y <- labels[x, 2]
  labRow <- c(labRow, as.character(y))
}

# insert activity labels column into tidySet
tidySet <- data.frame(tidySet[,1:2], labRow, tidySet[3:ncol(tidySet)])

# add column names to tidySet
names(tidySet) <- names(stdmean_dat)

# write tidySet to file
write.table(tidySet, "tidySet.txt", row.name = FALSE)