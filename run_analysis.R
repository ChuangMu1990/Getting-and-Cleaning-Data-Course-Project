##By Chuang Mu

setwd("C:/Users/Chuang/Desktop/Getting and Cleaning Data/UCI HAR Dataset")

## read features.txt and covnert to a data frame
f = "C:/Users/Chuang/Desktop/Getting and Cleaning Data/UCI HAR Dataset/features.txt"
features  = read.table(f,sep="",stringsAsFactors=F)


## read data and build database (type: test and train)
get_data <- function(type, features){
  f1 = paste(type,"/","subject_",type,".txt",sep="")
  subject_data = read.table(f1,sep="",stringsAsFactors=F, col.names="id")
  
  f2 = paste(type,"/","y_",type,".txt",sep="")
  y_data = read.table(f2,sep="",stringsAsFactors=F, col.names="activity")
  
  f3 = paste(type,"/","X_",type,".txt",sep="")
  x_data = read.table(f3,sep="",stringsAsFactors=F, col.names=features$V2)
  
  return (cbind(subject_data,y_data,x_data))
}

test = get_data("test", features)
train = get_data("train", features)


## 1.Merges the training and the test sets to create one data set.
data =  merge(train, test,all=TRUE)


## 2.Extracts only the measurements on the mean and standard deviation for each measurement.
## first col: id; second col: activity
mean_std = data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]


## 3.Uses descriptive activity names to name the activities in the data set
f = "C:/Users/Chuang/Desktop/Getting and Cleaning Data/UCI HAR Dataset/activity_labels.txt"
activity_labels = read.table(f,sep="",stringsAsFactors=F)


## 4.Appropriately labels the data set with descriptive variable names.
data$activity = factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)
mean_std$activity = factor(mean_std$activity, levels=activity_labels$V1, labels=activity_labels$V2)
#write.csv(data, file = "mergedata.csv")
#write.csv(mean_std, file = "mean_std.csv")


## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(plyr)
tidy_dataset =
  ddply(mean_std, .(id, activity), .fun = function(x){colMeans(x[,-c(1:2)],na.rm = TRUE)})

write.table(tidy_dataset, file = "tidy_dataset_byMC.txt", row.name=FALSE)
