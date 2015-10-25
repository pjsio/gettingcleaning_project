require(ggplot2)
require(data.table)
require(dplyr)

## Input your working directory with the UCI dataset
setwd('./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset')
train.features <- fread('./train/X_train.txt')
train.acti <- fread('./train/y_train.txt')
train.subject <- fread('./train/subject_train.txt')
test.features <- fread('./test/X_test.txt')
test.acti <- fread('./test/y_test.txt')
test.subject <- fread('./test/subject_test.txt')

## There are a total of five activities that are required by the course project
## (6 if you include outputting the tidy dataset into txt format):
## 1. merge subject, activities and features
## 2. Extract only measurement mean and standard deviation for each measurement (x,y and z)
## 3. Name the Activities
## 4. Give Appropriate Labels
## 5. From the data set in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject.


## 1. merge subject, activities and features
## Read in data for TEST data
name.stuff.test <- bind_cols(test.subject, test.acti, test.features)
colnames(name.stuff.test) <- c('subjectnum', 'activitynum', as.character(1:561))

## Read in data for TRAIN data
name.stuff.train <- bind_cols(train.subject, train.acti, train.features)
colnames(name.stuff.train) <- c('subjectnum', 'activitynum', as.character(1:561))

# Merge the TEST and TRAIN data
merged <- bind_rows(name.stuff.test, name.stuff.train)



## 2. Extract only measurement mean and standard deviation for each measurement (x,y and z)
## The features that we want to extract from a total of 561 features are as follows:
# 1 tBodyAcc-mean()-X
# 2 tBodyAcc-mean()-Y
# 3 tBodyAcc-mean()-Z
# 4 tBodyAcc-std()-X
# 5 tBodyAcc-std()-Y
# 6 tBodyAcc-std()-Z

six_measures <- merged %>%
  select(1:8)
colnames(six_measures) <- c('subjnum', 'actinum', seq(6))

## 3. Name the Activities
acti.labels <- fread('./activity_labels.txt')
six_measures$activity <- lapply(six_measures$actinum, function(x) acti.labels$V2[acti.labels$V1==x])

## 4. Give Appropriate Labels
colnames(six_measures) <- c('subjnum', 'actinum','taccmeanx',
                            'taccmeany','taccmeanz',
                            'taccstdx', 'taccstdy', 'taccstdz', 'activity')

## 5. From the data set in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject.
six_measures$subjnum <- as.factor(six_measures$subjnum)
six_measures$activity <- as.factor(unlist(six_measures$activity))
means <- six_measures %>%
  group_by(subjnum, activity) %>%
  summarise(mean(taccmeanx), mean(taccmeany), mean(taccmeanz), mean(taccstdx), mean(taccstdy), mean(taccstdz))

## 6. write the data out as a txt file
write.table(means, file = './tidydata.txt', row.name=FALSE)
