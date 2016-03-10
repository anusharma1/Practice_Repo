

 
2 require(plyr) 
3 
 
4 # This r program creates a single TidyDataSet from compiling multiple raw files of  
5 # Human Activity Recognition Using Smartphones Data 
6 # This program needs to be run from the directory where the source data is extracted 
7 # 
8 
 
9 # Merge train and test data sets to a single TotalSet 
10 
 
11     # First read the raw data files 
12       TrainSet <- read.table("train/X_train.txt") 
13       Subject_train <- read.table("train/subject_train.txt") 
14       Activity_train <- read.table("train/y_train.txt") 
15       TestSet <- read.table("test/X_test.txt") 
16       Subject_test <- read.table("test/subject_test.txt") 
17       Activity_test <- read.table("test/y_test.txt") 
18       Features <- read.table("features.txt") 
19       ActivityLabels <- read.table("activity_labels.txt") 
20 
 
21     # Appropriately label the data set with descriptive variable names 
22       names(ActivityLabels) <- c("ActivityCode", "Activity") 
23       names(TrainSet) <- Features[,2] 
24       names(Subject_train) <- "Subject" 
25       names(Activity_train) <- "ActivityCode" 
26       names(TestSet) <- Features[,2] 
27       names(Subject_test) <- "Subject" 
28       names(Activity_test) <- "ActivityCode" 
29 
 
30     # Prepare a single data set for each of Train and Test 
31       TrainSet <- cbind(Subject_train, Activity_train, TrainSet) 
32       TestSet <- cbind(Subject_test, Activity_test, TestSet) 
33 
 
34     # Merge/combine train and test data sets into single TotalSet 
35       TotalSet <- rbind(TrainSet, TestSet) 
36       rm("TrainSet", "TestSet") 
37 
 
38 # Extract only the measurements on the mean and standard deviation for each measurement 
39 
 
40     # Identify required measurements 
41       varNames <- names(TotalSet) 
42       neededVars <- c("Subject", "ActivityCode",  
43                       varNames[sort(c(grep("mean()", varNames, fixed=TRUE),  
44                                       grep("std()", varNames, fixed=TRUE)))]) 
45 
 
46     # Create a data set with only required measurements 
47       TotalSet <- TotalSet[,neededVars] 
48       names(TotalSet) <- gsub("-", "", gsub("-std()", "Std",  
49                                             sub("-mean()", "Mean",  
50                                                 names(TotalSet), fixed=TRUE),  
51                                             fixed=TRUE), fixed=TRUE) 
52 
 
53 # Assign descriptive activity names to name the activities in the data set 
54 
 
55     TotalSet <- merge(TotalSet, ActivityLabels, by.x="ActivityCode", by.y = "ActivityCode") 
56 
 
57 # creates an independent tidy data set with the average of each variable for each activity and each subject 
58 
 
59     TidyDataSet <- ddply(TotalSet, .(Subject, Activity), summarise,  
60                      tBodyAccMeanX_avg = mean(tBodyAccMeanX, rm.na=TRUE), 
61                      tBodyAccMeanY_avg = mean(tBodyAccMeanY, rm.na=TRUE), 
62                      tBodyAccMeanZ_avg = mean(tBodyAccMeanZ, rm.na=TRUE), 
63                      tBodyAccStdX_avg = mean(tBodyAccStdX, rm.na=TRUE), 
64                      tBodyAccStdY_avg = mean(tBodyAccStdY, rm.na=TRUE), 
65                      tBodyAccStdZ_avg = mean(tBodyAccStdZ, rm.na=TRUE), 
66                      tGravityAccMeanX_avg = mean(tGravityAccMeanX, rm.na=TRUE), 
67                      tGravityAccMeanY_avg = mean(tGravityAccMeanY, rm.na=TRUE), 
68                      tGravityAccMeanZ_avg = mean(tGravityAccMeanZ, rm.na=TRUE), 
69                      tGravityAccStdX_avg = mean(tGravityAccStdX, rm.na=TRUE), 
70                      tGravityAccStdY_avg = mean(tGravityAccStdY, rm.na=TRUE), 
71                      tGravityAccStdZ_avg = mean(tGravityAccStdZ, rm.na=TRUE), 
72                      tBodyAccJerkMeanX_avg = mean(tBodyAccJerkMeanX, rm.na=TRUE), 
73                      tBodyAccJerkMeanY_avg = mean(tBodyAccJerkMeanY, rm.na=TRUE), 
74                      tBodyAccJerkMeanZ_avg = mean(tBodyAccJerkMeanZ, rm.na=TRUE), 
75                      tBodyAccJerkStdX_avg = mean(tBodyAccJerkStdX, rm.na=TRUE), 
76                      tBodyAccJerkStdY_avg = mean(tBodyAccJerkStdY, rm.na=TRUE), 
77                      tBodyAccJerkStdZ_avg = mean(tBodyAccJerkStdZ, rm.na=TRUE), 
78                      tBodyGyroMeanX_avg = mean(tBodyGyroMeanX, rm.na=TRUE), 
79                      tBodyGyroMeanY_avg = mean(tBodyGyroMeanY, rm.na=TRUE), 
80                      tBodyGyroMeanZ_avg = mean(tBodyGyroMeanZ, rm.na=TRUE), 
81                      tBodyGyroStdX_avg = mean(tBodyGyroStdX, rm.na=TRUE), 
82                      tBodyGyroStdY_avg = mean(tBodyGyroStdY, rm.na=TRUE), 
83                      tBodyGyroStdZ_avg = mean(tBodyGyroStdZ, rm.na=TRUE), 
84                      tBodyGyroJerkMeanX_avg = mean(tBodyGyroJerkMeanX, rm.na=TRUE), 
85                      tBodyGyroJerkMeanY_avg = mean(tBodyGyroJerkMeanY, rm.na=TRUE), 
86                      tBodyGyroJerkMeanZ_avg = mean(tBodyGyroJerkMeanZ, rm.na=TRUE), 
87                      tBodyGyroJerkStdX_avg = mean(tBodyGyroJerkStdX, rm.na=TRUE), 
88                      tBodyGyroJerkStdY_avg = mean(tBodyGyroJerkStdY, rm.na=TRUE), 
89                      tBodyGyroJerkStdZ_avg = mean(tBodyGyroJerkStdZ, rm.na=TRUE), 
90                      tBodyAccMagMean_avg = mean(tBodyAccMagMean, rm.na=TRUE), 
91                      tBodyAccMagStd_avg = mean(tBodyAccMagStd, rm.na=TRUE), 
92                      tGravityAccMagMean_avg = mean(tGravityAccMagMean, rm.na=TRUE), 
93                      tGravityAccMagStd_avg = mean(tGravityAccMagStd, rm.na=TRUE), 
94                      tBodyAccJerkMagMean_avg = mean(tBodyAccJerkMagMean, rm.na=TRUE), 
95                      tBodyAccJerkMagStd_avg = mean(tBodyAccJerkMagStd, rm.na=TRUE), 
96                      tBodyGyroMagMean_avg = mean(tBodyGyroMagMean, rm.na=TRUE), 
97                      tBodyGyroMagStd_avg = mean(tBodyGyroMagStd, rm.na=TRUE), 
98                      tBodyGyroJerkMagMean_avg = mean(tBodyGyroJerkMagMean, rm.na=TRUE), 
99                      tBodyGyroJerkMagStd_avg = mean(tBodyGyroJerkMagStd, rm.na=TRUE), 
100                      fBodyAccMeanX_avg = mean(fBodyAccMeanX, rm.na=TRUE), 
101                      fBodyAccMeanY_avg = mean(fBodyAccMeanY, rm.na=TRUE), 
102                      fBodyAccMeanZ_avg = mean(fBodyAccMeanZ, rm.na=TRUE), 
103                      fBodyAccStdX_avg = mean(fBodyAccStdX, rm.na=TRUE), 
104                      fBodyAccStdY_avg = mean(fBodyAccStdY, rm.na=TRUE), 
105                      fBodyAccStdZ_avg = mean(fBodyAccStdZ, rm.na=TRUE), 
106                      fBodyAccJerkMeanX_avg = mean(fBodyAccJerkMeanX, rm.na=TRUE), 
107                      fBodyAccJerkMeanY_avg = mean(fBodyAccJerkMeanY, rm.na=TRUE), 
108                      fBodyAccJerkMeanZ_avg = mean(fBodyAccJerkMeanZ, rm.na=TRUE), 
109                      fBodyAccJerkStdX_avg = mean(fBodyAccJerkStdX, rm.na=TRUE), 
110                      fBodyAccJerkStdY_avg = mean(fBodyAccJerkStdY, rm.na=TRUE), 
111                      fBodyAccJerkStdZ_avg = mean(fBodyAccJerkStdZ, rm.na=TRUE), 
112                      fBodyGyroMeanX_avg = mean(fBodyGyroMeanX, rm.na=TRUE), 
113                      fBodyGyroMeanY_avg = mean(fBodyGyroMeanY, rm.na=TRUE), 
114                      fBodyGyroMeanZ_avg = mean(fBodyGyroMeanZ, rm.na=TRUE), 
115                      fBodyGyroStdX_avg = mean(fBodyGyroStdX, rm.na=TRUE), 
116                      fBodyGyroStdY_avg = mean(fBodyGyroStdY, rm.na=TRUE), 
117                      fBodyGyroStdZ_avg = mean(fBodyGyroStdZ, rm.na=TRUE), 
118                      fBodyAccMagMean_avg = mean(fBodyAccMagMean, rm.na=TRUE), 
119                      fBodyAccMagStd_avg = mean(fBodyAccMagStd, rm.na=TRUE), 
120                      fBodyBodyAccJerkMagMean_avg = mean(fBodyBodyAccJerkMagMean, rm.na=TRUE), 
121                      fBodyBodyAccJerkMagStd_avg = mean(fBodyBodyAccJerkMagStd, rm.na=TRUE), 
122                      fBodyBodyGyroMagMean_avg = mean(fBodyBodyGyroMagMean, rm.na=TRUE), 
123                      fBodyBodyGyroMagStd_avg = mean(fBodyBodyGyroMagStd, rm.na=TRUE), 
124                      fBodyBodyGyroJerkMagMean_avg = mean(fBodyBodyGyroJerkMagMean, rm.na=TRUE), 
125                      fBodyBodyGyroJerkMagStd_avg = mean(fBodyBodyGyroJerkMagStd, rm.na=TRUE) 
126                     ) 
127 
 
128 write.table(TidyDataSet, file = "TidyDataSet.txt", row.names = FALSE, append = FALSE) 
129 
 
130 # TestTidy <- read.table("TidyDataSet.txt", sep = "", header=TRUE) 


Status
 API
 Training
 Shop
 Blog
 About
 Pricing
   © 2016 GitHub, Inc.
 Terms
 Privacy
 Security
 Contact
 Help
 
