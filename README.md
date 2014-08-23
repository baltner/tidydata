# Explanation of the steps in the run\_analysis.R script

The Getting and Cleaning Data course project requires students to create
an R script called run\_analysis.R to perform several steps leading to a
final output of a tidy data set. This document explains some of the
logic behind the script (see run\_analysis.R in this github repository).

## 1. Merge the training and the test sets in the UCI HAR database to create one data set.

The first step reads in the data from the local R working directory. I
chose to read ALL the data needed by the script at one time even though
only the training and test data were needed for this explicit step. This
was done by first defining each file path and then executing the
read.table commands. The variables defined here are used later in the
script.
```r
        train_file <- "./UCI HAR Dataset/train/X_train.txt"
        test_file <- "./UCI HAR Dataset/test/X_test.txt"
        features_file <- "./UCI HAR Dataset/features.txt"
        labels_file <- "./UCI HAR Dataset/activity_labels.txt"
        activities_train_file <- "./UCI HAR Dataset/train/y_train.txt"
        activities_test_file  <- "./UCI HAR Dataset/test/y_test.txt"
        subject_train_file <- "./UCI HAR Dataset/train/subject_train.txt"
        subject_test_file  <- "./UCI HAR Dataset/test/subject_test.txt"
            
        xtrain <- read.table(train_file)
        xtest <- read.table(test_file)
        features <- read.table(features_file)[,2]      
        labels  <- read.table(labels_file)[,2]          
        activities_train <- read.table(activities_train_file)[,1]
        activities_test<- read.table(activities_test_file)[,1]
        subjects_train <- read.table(subject_train_file)[,1]
        subjects_test <- read.table(subject_test_file)[,1]
```

Note that all of the data columns are needed only for the training and
test data files. For the rest, specific columns were extracted using R
subsetting logic. Note also that the Samsung data is to be used as a
machine learning project, where the objective is to predict activities
by using the measurements. The convention often used is to use a capital
X for the matrix of variables, and a lower case y for the vector of
outcomes, in order to find the best prediction function f such as
y=f(X). Therefore, the activities derived from the data using SVM
classification techniques are in the y\_train.txt and y\_test.txt files.

Since the data sets are already column-aligned by feature 1--\> feature
561, all I needed to do to merge these is use rbind(x,y):

```r
            merged <- rbind(xtrain,xtest)
```

## 2. Extract only the measurements on the mean and standard deviation for each measurement.

For this step I assumed that measurements of the mean are designated by
"mean()" in the feature name and measurements of standard deviation
(sigma) are designated by "std()" in the feature name. These are the
only columns I care about. The first 33 columns of the extracted data
are the means, the next 33 columns are the standard deviations.

I use the grep() function to find the mean() and std() strings in the
feature names (stored in the 'features' variable) and create a data
frame with all 66 variables using R's cbind() function, storing it in a
variable named 'extracted.' Note that the grep function requires the
fixed=TRUE option to make an exact match with the input strings. Again I
use R's subsetting logic to extract the specific columns from the merged
data set created in step 1.

```r
            means  <- grep("mean()",features, fixed=TRUE)
            sigmas <- grep("std()" ,features, fixed=TRUE)
            extracted <- cbind(merged[means],merged[sigmas])
```

## 3. Use descriptive activity names to name the activities in the data set

For this step I need to merge the activities just as I did the data,
then convert to descriptive labels and add the labels as the first
column in the data set using cbind() again:

```r
	activities <- c(activities_train, activities_test)
	activities <- as.character(labels[activities])
	extracted<-cbind(activities,extracted)
```

Note here that the activity data is just a series of ordinal numbers. To
convert to descriptive labels I used the labels variable as a 'lookup'
table.

## 4. Appropriately label the data set with descriptive variable names.

For this step I had to construct an array of the extracted feature
names and then add these names to the extracted data frame by assigning
the column names via the names() function:

```r
            variables <- c(as.vector(features[means]), as.vector(features[sigmas]))  
            names(extracted) <- c("Activity",variables)
```

Note that in the last step I also had to add a column name for the
activities column I added in the previous step. After this, each column
is now properly labeled.

## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

The first task was to add the subjects to the extracted data set, as a
separate data set (called dataset), using cbind():

```r
            Subject <- c(subjects_train,subjects_test)
            dataset <- cbind(Subject,extracted)
```

For the next part, calculating the means of the extracted data for each
combinatin of subject and activity, I made several attempts to use
R functions that were discussed in the Week 3 lectures, but never really
felt comfortable with that approach. Therefore, I relied on the old
tried and true method I've used for years, looping. In the following code
block I loop through the data set and find the mean of each variable for
each subject and each activity. I store the results in a new
(initialized) data frame called newdata, renaming the variables to
indicate that we are reporting their mean (see codebook.docx for a
description of these variables).

```r
            n<-1
            newdata <- data.frame(matrix(vector(), 0, 68, dimnames=list(c(), character(68))), stringsAsFactors=F)
            names(newdata) <- paste("Mean(",names(dataset),")",sep="")
            names(newdata)[1] <-"Subject"
            names(newdata)[2] <-"Activity"

            for(i in 1:30){
                    dataset2 <- dataset[dataset$Subject==i,]                          # subset by subject i
                    for(j in 1:6) {
                            newdata[n,1] <- dataset2$Subject[1]
                            newdata[n,2] <- labs[j]
                            dataset3 <- dataset2[dataset2$Activity==labels[j],]       # subset by activity j
                            for(k in 1:length(variables)) {
                                    newdata[n,k+2] <- mean(dataset3[,k+2])            # average for this variable
                            }
                            n <- n+1  # next row
                    }
            }
```

The data frame newdata contained all the right values and at first I
attempted to write it to file as the final output in this form. However,
the long variable names did not align well with the columns they
represented so I was not happy with that result. Instead, I used the melt
function from the reshape2 package to convert these into row names, and
then output this as the final product (tidy\_mean.txt).

```r
            library(reshape2)
            tidy_means <-melt(newdata, id=c("Subject","Activity"),variable.name="Variable Name",value.name="Value")
            write.table(tidy_means, row.names=FALSE,file="tidy_means.txt",quote=FALSE)
```