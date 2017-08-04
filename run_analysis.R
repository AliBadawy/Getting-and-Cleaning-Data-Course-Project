#1_Merges the training and the test sets to create one data set.
#loading data and merging it 
library(plyr)
library(data.table)
#the data loading
setwd("UCI HAR Dataset")
subject_Train = read.table('./train/subject_train.txt',header=FALSE)

x_Train = read.table('./train/x_train.txt',header=FALSE)

y_Train = read.table('./train/y_train.txt',header=FALSE)


subject_Test = read.table('./test/subject_test.txt',header=FALSE)

X_Test = read.table('./test/x_test.txt',header=FALSE)

Y_Test = read.table('./test/y_test.txt',header=FALSE)

X <- rbind(x_Train, X_Test) #the merge of train and test in X

Y <- rbind(y_Train, Y_Test) #the merge of train and test in Y

subject_DataSet <- rbind(subject_Train, subject_Test)

#dim(X)
#dim(Y)
#dim(subject_DataSet)

#2_Extracts only the measurements on the mean and standard deviation for each measurement.

X_mean_std <- X[, grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2])]

names(X_mean_std) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2] 

View(X_mean_std)

dim(X_mean_std)

#3. Use descriptive activity names to name the activities in the data set.

Y[, 1] <- read.table("activity_labels.txt")[Y[, 1], 2]

names(Y) <- "Activity"

View(Y)

#4. Appropriately label the data set with descriptive activity names.

names(subject_DataSet) <- "Subject"

summary(subject_DataSet)

# Organizing and combining all data sets into single one.

one_DataSet <- cbind(X_mean_std, Y, subject_DataSet)

# Defining descriptive names for all variables.

names(one_DataSet) <- make.names(names(one_DataSet))

names(one_DataSet) <- gsub('Acc',"Acceleration",names(one_DataSet))

names(one_DataSet) <- gsub('GyroJerk',"AngularAcceleration",names(one_DataSet))

names(one_DataSet) <- gsub('Gyro',"AngularSpeed",names(one_DataSet))

names(one_DataSet) <- gsub('Mag',"Magnitude",names(one_DataSet))

names(one_DataSet) <- gsub('^t',"TimeDomain.",names(one_DataSet))

names(one_DataSet) <- gsub('^f',"FrequencyDomain.",names(one_DataSet))

names(one_DataSet) <- gsub('\\.mean',".Mean",names(one_DataSet))

names(one_DataSet) <- gsub('\\.std',".StandardDeviation",names(one_DataSet))

names(one_DataSet) <- gsub('Freq\\.',"Frequency.",names(one_DataSet))

names(one_DataSet) <- gsub('Freq$',"Frequency",names(one_DataSet))

View(one_DataSet)

#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

names(one_DataSet)

Data2<-aggregate(. ~Subject + Activity, one_DataSet, mean)

Data2<-Data2[order(Data2$Subject,Data2$Activity),]

write.table(Data2, file = "tidydata.txt",row.name=FALSE)
