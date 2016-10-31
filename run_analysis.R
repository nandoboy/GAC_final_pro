library('dplyr')

#creating folder if not present already
if(!file.exists("./data")){
	dir.create("./data")
}

#download and unzip file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="./data/Dataset.zip")
unzip(zipfile="./data/Dataset.zip",exdir="./data")

#set file path
base_path <- file.path("./data" , "UCI HAR Dataset")


#reading training data
train_feature <- read.table(file.path(base_path,'train','X_train.txt'),header = FALSE,stringsAsFactors = FALSE)
train_activity <- read.table(file.path(base_path,'train','y_train.txt'),header = FALSE,stringsAsFactors = FALSE)
train_subject <- read.table(file.path(base_path,'train','subject_train.txt'),header = FALSE,stringsAsFactors = FALSE)

#put training frame together using 'cbind'
train_frame <- cbind(train_feature,train_activity,train_subject)

#reading test data
test_path <- file.path('D:/nanda/r_code/coursera/GAC_data/course_project/UCI HAR Dataset/test')

test_feature <- read.table(file.path(base_path,'test','X_test.txt'),header = FALSE)
test_labels <- read.table(file.path(base_path,'test','y_test.txt'),header = FALSE)
test_subject <- read.table(file.path(base_path,'test','subject_test.txt'),header = FALSE)

#put test frame together using 'cbind'
test_frame <- cbind(test_feature,test_labels,test_subject)

#combine training and test frame to get master frame
master_frame <- rbind(train_frame,test_frame)

#naming master frame columns
feature_list <- read.table(file.path(base_path,'features.txt'),header = FALSE,stringsAsFactors = FALSE)
names(master_frame)[1:(ncol(master_frame)-2)] <- feature_list$V2
names(master_frame)[562:563] <- c('Activity_ID','Subject_ID')

#extracting columns only with mean or standard deviation and activity and subject ID
mf_mean_std <- master_frame[,c(grep('std\\(\\)|mean\\(\\)',names(master_frame)),562,563)]


#labeling activites
activity_ref_frame <- read.table(file.path(base_path,'activity_labels.txt'),header = FALSE)
mf_mean_std_1 <- merge(mf_mean_std,activity_ref_frame,by.x = 'Activity_ID',by.y='V1',all.x = TRUE)
names(mf_mean_std_1)[ncol(mf_mean_std_1)] <- 'Activity'
mf_mean_std_1 <- mf_mean_std_1[,c(2:(ncol(mf_mean_std_1)-2),ncol(mf_mean_std_1),ncol(mf_mean_std_1)-1)]

names(mf_mean_std_1)<-gsub("BodyBody", "Body", names(mf_mean_std_1))
names(mf_mean_std_1)<-gsub("^t", "time", names(mf_mean_std_1))
names(mf_mean_std_1)<-gsub("^f", "frequency", names(mf_mean_std_1))
names(mf_mean_std_1)<-gsub("Acc", "Accelerometer", names(mf_mean_std_1))
names(mf_mean_std_1)<-gsub("Gyro", "Gyroscope", names(mf_mean_std_1))
names(mf_mean_std_1)<-gsub("Mag", "Magnitude", names(mf_mean_std_1))
names(mf_mean_std_1)<-gsub("\\(\\)", "", names(mf_mean_std_1))

#calculating mean of each variable for each activity and each subject
final_frame <- mf_mean_std_1 %>% group_by(Activity,Subject_ID) %>% summarise_each(funs(mean))
final_frame<-final_frame[order(final_frame$Subject_ID,final_frame$Activity),]
write.table(final_frame, file = "tidy_set.txt",row.name=FALSE)
