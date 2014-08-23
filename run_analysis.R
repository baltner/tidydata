run_analysis <- function() {

## Getting and Cleaning Data Course Project

        # Read the data from the files in the UCI HAR Dataset distribution
        
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
        features <- read.table(features_file)[,2]       # we only need one column of the following data sets
        labels  <- read.table(labels_file)[,2]          
        activities_train <- read.table(activities_train_file)[,1]
        activities_test<- read.table(activities_test_file)[,1]
        subjects_train <- read.table(subject_train_file)[,1]
        subjects_test <- read.table(subject_test_file)[,1]

        
## Merge the training and the test data sets to create one data set
        
        # Since the data sets are already column-aligned by feature 1--> feature 561 all we need to
        # do to merge these is use rbind(x,y)

        merged <- rbind(xtrain,xtest)


## Extract only the measurements on the mean and standard deviation for each measurement

        # Here we assume that measurements of the mean are designated by "mean()" in the feature name and
        # measurements of standard deviation (sigma) are designated by "std()" in the feature name.
        # These are the only columns we care about.
        # The first 33 columns of the extracted data are the means, the next 33 columns are the 
        # standard deviations.

        means  <- grep("mean()",features, fixed=TRUE)
        sigmas <- grep("std()" ,features, fixed=TRUE)
        extracted=cbind(merged[means],merged[sigmas])


## Use descriptive activity names to name the activities in the data set

        # Merge the activities just as we did the data, then convert to descriptive labels
        activities <- c(activities_train, activities_test)
        activities <- as.character(labels[activities])
        
        # Add the labels as the first column in the data set
        extracted<-cbind(activities,extracted)
        

## Appropriately label the data set with descriptive variable names. 

        # Construct an array of the extracted feature names then
        # add these names to the extracted data frame.
        
        variables <- c(as.vector(features[means]), as.vector(features[sigmas]))  
        names(extracted) <- c("Activity",variables)


## Create a second, independent tidy data set with the average of each variable for each activity and 
## each subject. 

        # Add the subjects to the extracted data set as a separate data set (called dataset)
        Subject <- c(subjects_train,subjects_test)
        dataset <- cbind(Subject,extracted)
        labs <- pad_labels(labels)

        # Loop through the data set and find the mean of each variable for each subject and each activity
        # Store the results in the new (initialized) data frame called newdata. Rename the variables to
        # indicate that we are reporting their mean.
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
        # Melt the data frame to make it easier to read
        tidy_means <-melt(newdata, id=c("Subject","Activity"),variable.name="Variable Name",value.name="Value")
        
        #write out the data to a file
        write.table(tidy_means, row.names=FALSE,file="tidy_means.txt",quote=FALSE)
        tidy_means

}

pad_labels <- function(labels){
        #Convert factor labels to character
        labs <- as.character(labels)
        
        #Determine maximum width
        maxwidth <- nchar(labs[which(nchar(labs)==max(nchar(labs)))])
        
        #pad with spaces so all the same width
        labs<- format(labs, width=maxwidth)
        
        return(labs)
}