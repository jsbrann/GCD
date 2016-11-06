library(dplyr)
library(reshape2)


## Merge the data sets into one

root.dir <- "UCI HAR Dataset"
data.set <- list()

data.set$features <- read.table(paste(root.dir, "features.txt", sep="/"), col.names=c('id', 'name'), stringsAsFactors=FALSE)

data.set$activity_labels <- read.table(paste(root.dir, "activity_labels.txt", sep="/"), col.names=c('id', 'Activity'))

## Read the test set
data.set$test <- cbind(subject=read.table(paste(root.dir, "test", "subject_test.txt", sep="/"), col.names="Subject"),
                       y=read.table(paste(root.dir, "test", "y_test.txt", sep="/"), col.names="Activity.ID"),
                       x=read.table(paste(root.dir, "test", "x_test.txt", sep="/")))

## Read the training set
data.set$train <- cbind(subject=read.table(paste(root.dir, "train", "subject_train.txt", sep="/"), col.names="Subject"),
                        y=read.table(paste(root.dir, "train", "y_train.txt", sep="/"), col.names="Activity.ID"),
                        x=read.table(paste(root.dir, "train", "X_train.txt", sep="/")))

## Clean up the column names
rename.features <- function(thisCol) {
    thisCol <- gsub("tBody", "Time.Body", thisCol)
    thisCol <- gsub("tGravity", "Time.Gravity", thisCol)
    thisCol <- gsub("fBody", "FFT.Body", thisCol)
    thisCol <- gsub("fGravity", "FFT.Gravity", thisCol)

    thisCol <- gsub("\\-mean\\(\\)\\-", ".Mean.", thisCol)
    thisCol <- gsub("\\-std\\(\\)\\-", ".Std.", thisCol)
    thisCol <- gsub("\\-mean\\(\\)", ".Mean", thisCol)
    thisCol <- gsub("\\-std\\(\\)", ".Std", thisCol)

    return(thisCol)
}

tidy <- rbind(data.set$test, data.set$train)[,c(1, 2, grep("mean\\(|std\\(", data.set$features$name) + 2)]

names(tidy) <- c("Subject", "Activity.ID", rename.features(data.set$features$name[grep("mean\\(|std\\(", data.set$features$name)]))

tidy <- merge(tidy, data.set$activity_labels, by.x="Activity.ID", by.y="id")
tidy <- tidy[,!(names(tidy) %in% c("Activity.ID"))]

tidy.mean <- ddply(melt(tidy, id.vars=c("Subject", "Activity")), .(Subject, Activity), summarise, MeanSamples=mean(value))

write.csv(tidy.mean, file = "tidy.mean.txt",row.names = FALSE)
write.csv(tidy, file = "tidy.txt",row.names = FALSE)