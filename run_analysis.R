
## loading labels and features
activity_labels<-read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header = FALSE, strip.white=TRUE)
features<-read.csv("UCI HAR Dataset/features.txt", sep="", header = FALSE, strip.white=TRUE)

## loading test set
#body_acc_x_test<-read.csv("UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt", sep="", header = FALSE, strip.white=TRUE)
#body_acc_y_test<-read.csv("UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt", sep="", header = FALSE, strip.white=TRUE)
#body_acc_z_test<-read.csv("UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt", sep="", header = FALSE, strip.white=TRUE)
#body_gyro_x_test<-read.csv("UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt", sep="", header = FALSE, strip.white=TRUE)
#body_gyro_y_test<-read.csv("UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt", sep="", header = FALSE, strip.white=TRUE)
#body_gyro_z_test<-read.csv("UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt", sep="", header = FALSE, strip.white=TRUE)
#total_acc_x_test<-read.csv("UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt", sep="", header = FALSE, strip.white=TRUE)
#total_acc_y_test<-read.csv("UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt", sep="", header = FALSE, strip.white=TRUE)
#total_acc_z_test<-read.csv("UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt", sep="", header = FALSE, strip.white=TRUE)
subject_test<-read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header = FALSE, strip.white=TRUE)
X_test<-read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header = FALSE, strip.white=TRUE)
y_test<-read.csv("UCI HAR Dataset/test/y_test.txt", sep="", header = FALSE, strip.white=TRUE)

## loading train set
#body_acc_x_train<-read.csv("UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt", sep="", header = FALSE, strip.white=TRUE)
#body_acc_y_train<-read.csv("UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt", sep="", header = FALSE, strip.white=TRUE)
#body_acc_z_train<-read.csv("UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt", sep="", header = FALSE, strip.white=TRUE)
#body_gyro_x_train<-read.csv("UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt", sep="", header = FALSE, strip.white=TRUE)
#body_gyro_y_train<-read.csv("UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt", sep="", header = FALSE, strip.white=TRUE)
#body_gyro_z_train<-read.csv("UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt", sep="", header = FALSE, strip.white=TRUE)
#total_acc_x_train<-read.csv("UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt", sep="", header = FALSE, strip.white=TRUE)
#total_acc_y_train<-read.csv("UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt", sep="", header = FALSE, strip.white=TRUE)
#total_acc_z_train<-read.csv("UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt", sep="", header = FALSE, strip.white=TRUE)
subject_train<-read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header = FALSE, strip.white=TRUE)
X_train<-read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header = FALSE, strip.white=TRUE)
y_train<-read.csv("UCI HAR Dataset/train/y_train.txt", sep="", header = FALSE, strip.white=TRUE)

## merging

X <- rbind(X_train,X_test)
y <- rbind(y_train,y_test)
subject <- rbind(subject_train,subject_test)

labelactivity <- function(n) {
  as.character(activity_labels$V2[n])
}

names(features) <- c("featureNr", "featureName")
names(X) <- features$featureName

names(y)[1] <- "activityNr"
y["activity"]<-sapply(y$activityNr,labelactivity)
y["subject"]<-subject
y$activityNr <- NULL

featuresToKeep<-c(grep("mean[(]",features$featureName),grep("std[(]",features$featureName))
featuresToKeep<-featuresToKeep[order(featuresToKeep)]
X<-X[,featuresToKeep]

tidy<-unique(y)


for (n in 1:dim(tidy)[1])
{
  vals<-rep(1:dim(y)[1])[(tidy[n,]$activity==y$activity & tidy[n,]$subject==y$subject)]
  for(f in names(X))
  {
    tidy[n,f] <- mean(X[vals,f])
  }
}

write.table(tidy, "tidy.txt", row.names = FALSE)

