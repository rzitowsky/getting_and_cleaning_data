#
#
#  Project 1:  Getting and Cleaning Data
#
#


#  Set working directory
setwd("~/Coursera/getting_data/project1")

#  Load the dplyr package
require(dplyr)

#
#
#  Step 1:  Merge the training and test data sets into one data set
#
#

#  This imports x_test.txt, x_train.txt
x.test <- read.table("x_test.txt")
x.train <- read.table("x_train.txt")

#  Combine x.test and x.train vertically
x.merge <- rbind(x.test, x.train)

#  Add variable names to x.merge data frame
variable_labels <- read.table("features.txt")
variable_labels <- as.vector(variable_labels$V2)
valid_variable_labels <- make.names(variable_labels, unique = FALSE, allow_ = TRUE)
colnames(x.merge) <- valid_variable_labels

#  This imports y_test.txt, y_train.txt
y.test <- read.table("y_test.txt")
y.train <- read.table("y_train.txt")
activity_labels <- read.table("activity_labels.txt")

#  Combine y.test and y.train vertically
y.merge <- rbind(y.test, y.train)

#  Add descriptions to y.merge data frame
y.merge <- left_join(y.merge, activity_labels)

#  Relabel V1 to be activity_id and V2 to be activity
y.merge <- rename(y.merge, activity_id = V1, activity = V2)

#  This imports subject_test.txt, subject_train.txt
subject.test <- read.table("subject_test.txt")
subject.train <- read.table("subject_train.txt")

#  Combine subject.test and subject.train vertically
subject.merge <- rbind(subject.test, subject.train)

#  Relabel V1 to be subject
subject.merge <- rename(subject.merge, subject = V1)

#  Merge x.merge, y.merge, and subject.merge into one data set
xy.merge <- cbind(x.merge, y.merge, subject.merge)

#
#
#  Step 2:  Extracts only the measurements on the mean and standard deviation for each measurement. 
#
#

xy.merge <- xy.merge[, !duplicated(colnames(xy.merge))]
xy.merge.subset <- select(xy.merge, contains("mean"), contains("std"), subject, activity, activity_id)


#
#
#  Step 3:  Uses descriptive activity names to name the activities in the data set
#  NOTE:  This is already done above (y.merge <- left_join(y.merge, activity_labels))
#

#
#
#  Step 4:  Appropriately labels the data set with descriptive variable names. 
#  
#

names(xy.merge.subset) <- gsub("^t", "Time", names(xy.merge.subset))
names(xy.merge.subset) <- gsub("^f", "Freq", names(xy.merge.subset))

#
#
#  Step 5:  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#  
#

#  Convert grouping variables into factors
xy.merge.subset$subject = as.factor(xy.merge.subset$subject)
xy.merge.subset$activity = as.factor(xy.merge.subset$activity)

str(xy.merge.subset)

sumdata <- 
  xy.merge.subset %>%
  group_by(subject, activity) %>%
  summarize_each(funs(mean)) 

#  Export sumdata
write.table(sumdata, "sumdata.txt", row.names=FALSE)
