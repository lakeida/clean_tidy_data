print('Processing...')
# Get the data
if(!file.exists("./samsung_data")){
    dir.create("./samsung_data")
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl,destfile="./samsung_data/Dataset.zip",method="curl")
    unzip(zipfile="./samsung_data/Dataset.zip",exdir="./samsung_data")
}
path_to_files <- file.path("./samsung_data" , "UCI HAR Dataset")
current_wd = getwd() # house keeping on working directory
setwd(path_to_files)

# Read in the data from files
features        = read.table('./features.txt',header=FALSE)
activity_types  = read.table('./activity_labels.txt',header=FALSE)
subject_train   = read.table('./train/subject_train.txt',header=FALSE)
x_train         = read.table('./train/x_train.txt',header=FALSE)
y_train         = read.table('./train/y_train.txt',header=FALSE)
subject_test    = read.table('./test/subject_test.txt',header=FALSE)
x_test          = read.table('./test/x_test.txt',header=FALSE)
y_test          = read.table('./test/y_test.txt',header=FALSE)

# match column names before we merge
colnames(activity_types)  = c('activity_id','activity_type')
colnames(subject_train)  = "subject_id"
colnames(x_train)        = features[,2] 
colnames(y_train)        = "activity_id"
colnames(subject_test)   = "subject_id"
colnames(x_test)         = features[,2] 
colnames(y_test)         = "activity_id"

print('1. Merges the training and the test sets to create one data set.')
train_data = cbind(y_train,subject_train,x_train)
test_data = cbind(y_test,subject_test,x_test)
combined_data = rbind(train_data,test_data)


print('2. Extract only the measurements on the mean and standard deviation for each measurement.')
names(features) <- c('feature_id', 'feature_type')
subset_features <- grepl("mean|std", features$feature_type)
combined_data <- combined_data[, which(subset_features == TRUE)]

print('3. Use descriptive activity names to name the activities in the data set.')
names(activity_types) <- c('activity_id', 'activity_type')
combined_data <- merge(activity_types, combined_data, by='activity_id', all.x=TRUE)


print('4. Appropriately label the data set with descriptive activity names.') 
colNames  = colnames(combined_data)
for (i in 1:length(colNames)) 
{
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("Acc", "Accelerometer", colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("^(f)","Freq",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("Mag","Magnitude",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("-std","StdDev",colNames[i])
    colNames[i] = gsub("^(t)","Time",colNames[i])
}

colnames(combined_data) = colNames # reassign fixed columns

print('5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.')
tidy_data <- aggregate(
    combined_data[, 4:ncol(combined_data)],
    by=list(subject=combined_data$subject_id,
            activity=combined_data$activity_type),
    mean) # build tidy_data

names(tidy_data)[3:ncol(tidy_data)] <- paste0(names(tidy_data)[3:ncol(tidy_data)], '-avg')
write.table(tidy_data, 'tidy_data.txt', row.names=FALSE) # write out the tidy_data
print("Tidy Data generated")

setwd(current_wd) # reset wd back to original, this allows for multiple runs without multiple downloads