features <- read.table("./features.txt")
labels <- read.table("./activity_labels.txt")

xtest <- read.table("./test/X_test.txt", col.names=features[,2])
ytest <- read.table("./test/y_test.txt", col.names="activity")
subjecttest <- read.table("./test/subject_test.txt", col.names="subject")

xtrain <- read.table("./train/X_train.txt", col.names=features[,2])
ytrain <- read.table("./train/y_train.txt", col.names="activity")
subjecttrain <- read.table("./train/subject_train.txt", col.names="subject")

## merging the test and train data sets

test <- cbind(xtest, ytest, subjecttest)

train <- cbind(xtrain, ytrain, subjecttrain)

data <- rbind(test, train)

data$activity <- factor(data$activity, levels = c(1,2,3,4,5,6), labels = c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))

library(dplyr)

## selecting only the relevant data, i.e. the mean and standard deviation measurements

data <- arrange(data, subject, activity)
selectdata <- select(data, subject, activity, contains("mean"), contains("std"), -contains("Freq"), -contains("angle"))

## reformatting the column names for easier separation

names(selectdata) <- gsub("BodyBody", "Body", names(selectdata))
names(selectdata) <- gsub("BodyAcc", "AccBody", names(selectdata))
names(selectdata) <- gsub("GravityAcc", "AccGrav", names(selectdata))
names(selectdata) <- gsub("BodyGyro", "GyrBody", names(selectdata))
names(selectdata) <- gsub(".mean...X", "_X.mean", names(selectdata), fixed=TRUE)
names(selectdata) <- gsub(".mean...Y", "_Y.mean", names(selectdata), fixed=TRUE)
names(selectdata) <- gsub(".mean...Z", "_Z.mean", names(selectdata), fixed=TRUE)
names(selectdata) <- gsub(".std...X", "_X.std", names(selectdata), fixed=TRUE)
names(selectdata) <- gsub(".std...Y", "_Y.std", names(selectdata), fixed=TRUE)
names(selectdata) <- gsub(".std...Z", "_Z.std", names(selectdata), fixed=TRUE)
names(selectdata) <- gsub("..", "", names(selectdata), fixed=TRUE)
names(selectdata) <- gsub("Mag", "_Mag", names(selectdata))
names(selectdata) <- gsub("Body_", "Body_0_", names(selectdata))
names(selectdata) <- gsub("Grav_", "Grav_0_", names(selectdata))
names(selectdata) <- gsub("BodyJerk", "Body_1", names(selectdata))

library(tidyr)

selectdata1 <- gather(selectdata, DomainSignal_Device_Component_Jerk_Axis_Measure, Measurement, -activity, -subject)
selectdata2 <- separate(selectdata1, DomainSignal_Device_Component_Jerk_Axis_Measure, into=c("DomainSignal", "Device_Component_Jerk_Axis_Measure"), sep=1)
selectdata3 <- separate(selectdata2, Device_Component_Jerk_Axis_Measure, into=c("Device", "Component_Jerk_Axis_Measure"), sep=3)
tidydata <- separate(selectdata3, Component_Jerk_Axis_Measure, into=c("Component", "Jerk", "Axis", "Measure")) 

tidydata <- arrange(tidydata, subject, activity)

tidydatatbl <- tbl_df(tidydata)

## taking the mean of the measurements

groupeddata <- group_by(tidydatatbl, subject, activity, DomainSignal, Device, Component, Jerk, Axis, Measure)
tidydata2 <- tbl_df(summarise(groupeddata, mean(Measurement)))

write.table(tidydata2, "./run_analysis_table.txt", row.names=FALSE)