########Getting and Cleaning Data Project#########
##################################################

#1. Merges the training and the test sets to create one data set.#######
setwd('D:/Google/Coursera/3. Getting and Cleaning Data/UCI HAR Dataset');

# Read in the data #

#imports features.txt
features     = read.table('./features.txt',header=FALSE)
#imports activity_labels.txt
activityType = read.table('./activity_labels.txt',header=FALSE)
#imports subject_train.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
#imports x_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE)
#imports y_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE)
#imports subject_test.txt
subjectTest = read.table('./test/subject_test.txt',header=FALSE) 
#imports x_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE) 
#imports y_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE) 

# Assigin column names#

colnames(activityType)  = c('activity_Id','activity_Type')
colnames(subjectTrain)  = "subject_Id"
colnames(xTrain)        = features[,2];
colnames(yTrain)        = "activity_Id"
colnames(subjectTest) = "subject_Id"
colnames(xTest)       = features[,2] 
colnames(yTest)       = "activity_Id"

#Merge yTrain, subjectTrain, and xTrain
TrainingData = cbind(yTrain,subjectTrain,xTrain);

#Merge the xTest, yTest and subjectTest data
TestData = cbind(yTest,subjectTest,xTest);


# Combine training and test data to create a final data set
FinalData = rbind(TrainingData,TestData);

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 

# Create a logical Vector that contains TRUE values for  columns containing mean & stddev
colNames  = colnames(FinalData); 
keep = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
FinalData = FinalData[keep==TRUE];


#3.Uses descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
FinalData = merge(FinalData,activityType,by='activity_Id',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(FinalData); 

#4.Appropriately labels the data set with descriptive variable names. 
# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(FinalData) = colNames;


#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Create a new table without the activityType column
FinalData_NoActivityType  = FinalData[,names(FinalData) != c('activity_Type')];

# Summarizing the table to include just the mean 
TidyData    = aggregate(FinalData_NoActivityType[,names(FinalData_NoActivityType) != c('activity_Id','subject_Id')],by=list(activity_Id=FinalData_NoActivityType$activity_Id,subject_Id = FinalData_NoActivityType$subject_Id),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
TidyData    = merge(TidyData,activityType,by='activity_Id',all.x=TRUE);

# Export the tidyData set 
write.table(TidyData, './tidyData.txt',row.names=FALSE ,sep='\t');
#####################################################################################
