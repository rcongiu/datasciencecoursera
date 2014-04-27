datadir <- "./UCI HAR Dataset"
labelFile <- paste(datadir,"activity_labels.txt",sep="/")
featureFile <- paste(datadir,"features.txt",sep="/")

labels <- read.table(labelFile, col.names=c("activityid", "activity") )
features <- gsub("[^\\w\\d]+","_",as.vector(read.table(featureFile)[,2]),perl=TRUE)

readmeasures <- function( d, wantedfeatures) {
  subject <- as.vector(read.table( sprintf("%s/%s/subject_%s.txt", datadir,d,d), 
                                   col.names=c("subjid")))
  measures <-  read.table( sprintf("%s/%s/X_%s.txt", datadir,d,d))
  activity <-  as.vector(read.table( sprintf("%s/%s/y_%s.txt", datadir,d,d)))
  activitylabels <- labels[activity[,1],2]
  
  data <- measures[,wantedfeatures]
 
  data$subject <- subject$subjid
  data$activity <- as.vector(activitylabels)
  colnames(data) <- c(features[wantedfeatures], "subject","activity")
  return( data )
}

wantedfeatures =  grep("(_mean_|_std_)", features)
test <- readmeasures("test", wantedfeatures)
train <- readmeasures("train", wantedfeatures)

data <- rbind(test,train)
tidy <- aggregate(data, list(data$subject, data$activity), FUN=mean)
write.table(tidy,"tidy.txt")