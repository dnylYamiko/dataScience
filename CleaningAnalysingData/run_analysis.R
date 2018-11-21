if(!file.exists("./data")){dir.create("./data")
  fileUrl<-
  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile = "./data/DataSet.zip")
  unzip(zipfile = "./data/DataSet.zip",exdir = "./data")
  #setting the full path for the uzipped files
  datapath <- file.path("./data" , "UCI HAR Dataset")
  datafiles<-list.files(datapath, recursive=TRUE)}

#reading the files from the unzipped directory; these are "train" and "test"
#please note that "Inertial Signals" are not relevant for this project
  x_train<-read.table(file.path(datapath,"train","X_train.txt"),header = FALSE,
                       fill = TRUE)
  y_train<-read.table(file.path(datapath,"train","y_train.txt"),header = FALSE,
                      fill = TRUE)
  subject_train<-read.table(file.path(datapath,"train","subject_train.txt"),header = FALSE,
                                                        fill = TRUE)
 
  x_test<-read.table(file.path(datapath,"test","X_test.txt"),header = FALSE,
                     fill = TRUE)
  y_test<-read.table(file.path(datapath,"test","y_test.txt"),header = FALSE,
                     fill = TRUE)
  subject_test<-read.table(file.path(datapath,"test","subject_test.txt"),header = FALSE,
                     fill = TRUE)

#reading the features and activitiy_labels files  
  features<-read.table(file.path(datapath,"features.txt"),header = FALSE,fill = TRUE)
  activity_labels<-read.table(file.path(datapath,"activity_labels.txt"),header = FALSE,
                                                            fill = TRUE)

#tagging the test and train data sets
  #sanity and column names for the train datasets
  colnames(x_train)<-features[,2]
  colnames(y_train)<-"activityID"
  colnames(subject_train)<-"subjectID"
  #sanity and column name for the test data sets
  colnames(x_test)<-features[,2]
  colnames(y_test)<-"activityID"
  colnames(subject_test)<-"subjectID"
  #sanity and column name for activity labels
  colnames(activity_labels)<-c("activityID","activityTYPE")
  
#merging 
  mergedTRAIN <-cbind(y_train,subject_train,x_train)
  mergedTEST <- cbind(y_test,subject_test,x_test)
  mergedALL <-rbind(mergedTRAIN,mergedTEST)
  
#Extracting only measurements on the mean and standard deviation for each measurement
  colNames<-colnames(mergedALL)
  meanandSD <-(grepl("activityID",colNames)|grepl("subjectID",colNames)|
                 grepl("mean..",colNames)|grepl("std..",colNames))
  meanandSDALL<-mergedALL[,meanandSD==TRUE]
  
#use descriptive activity names to name the activities
  WithActivityNames<-merge(meanandSDALL,activity_labels,by="activityID",all.x=TRUE)
  
#variables activity and subject and names of the activities have been labelled using 
  #_descriptive names.In this part, Names of features will labelled using 
  #_descriptive variable names. Replacing using the gsub() function
  names(WithActivityNames)<-gsub("^t", "time", names(WithActivityNames))
  names(WithActivityNames)<-gsub("^f", "frequency", names(WithActivityNames))
  names(WithActivityNames)<-gsub("Acc", "Accelerometer", names(WithActivityNames))
  names(WithActivityNames)<-gsub("Gyro", "Gyroscope", names(WithActivityNames))
  names(WithActivityNames)<-gsub("Mag", "Magnitude", names(WithActivityNames))
  names(WithActivityNames)<-gsub("BodyBody", "Body", names(WithActivityNames))
  
#secondary tidy data set
  SecondarySet<-aggregate(.~subjectID+activityID,WithActivityNames,mean)
  SecondarySet<-SecondarySet[order(SecondarySet$subjectID,SecondarySet$activityID),]
  
#saving the new data
  write.table(SecondarySet,"SecondaryTidyData.txt",row.names = FALSE)
  
#generating the codebook
  write(codebook(SecondarySet,survey_repetition = "single",survey_overview=FALSE,
                 missingness_report = TRUE,metadata_table = TRUE,metadata_json = FALSE),
        file = "CodeBook.Rmd")
  
  knit2html("CodeBook.Rmd")#the output of the is CodeBook.md and CodeBook.html
