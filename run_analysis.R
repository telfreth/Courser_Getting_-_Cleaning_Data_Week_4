library(dplyr)
library(tidyr)

## This function reads in the test and train datasets, merges them, calculates averages, 
## transforms the result into a tidy data set and saves it to a csv file.
## Note, the working directory must be set to the directory containing the 
## "UCI HAR Dataset" directory prior to calling this function.

meanMeasurements <- function() 
{
        ##
        ## Assignment task 1: Merge the test and train data sets
        ##

        # Load the features names, i.e. the variable/column names.
        df_features <- read.delim("./UCI HAR Dataset/features.txt", header = FALSE, sep = " ")
        # Extract only the feature names into a vector that will be later used for column names.
        # Note: when the features names are later specified as column names, some characters will
        # be replaced with dots due to those characters not being acceptable in column names, e.g. "-","(",")"
        v_features <- df_features[[2]]

        # Load the test subject list
        df_subject <- read.delim("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = "subject")
        # Create an id column which will be used for merging
        df_subject$id <- 1:nrow(df_subject)

        # Load the test activities
        df_activity <- read.delim("./UCI HAR Dataset/test/Y_test.txt", header = FALSE, col.names = "activity")
        # Create an id column which will be used for merging
        df_activity$id <- 1:nrow(df_activity)

        # Load the test measurements
        df_measures <- read.delim("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "", col.names = v_features)
        # Create an id column which will be used for merging
        df_measures$id <- 1:nrow(df_measures)

        # Merge the subjects and activities by the id column created earlier
        df_subj_act <- merge(df_subject, df_activity) 
        # Merge also the test measurements by the id column created earlier
        df_test <- merge(df_subj_act, df_measures) # will merge by "id" column
        
        # Drop the id column
        df_test <- df_test[, 2:ncol(df_test)]

        # Load the train subject list
        df_subject <- read.delim("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = "subject")
        # Create an id column which will be used for merging
        df_subject$id <- 1:nrow(df_subject)

        # Load the train activities
        df_activity <- read.delim("./UCI HAR Dataset/train/Y_train.txt", header = FALSE, col.names = "activity")
        # Create an id column which will be used for merging
        df_activity$id <- 1:nrow(df_activity)

        # Load the train measurements
        df_measures <- read.delim("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "", col.names = v_features)
        # Create an id column which will be used for merging
        df_measures$id <- 1:nrow(df_measures)

        # Merge the subjects and activities by the id column created earlier
        df_subj_act <- merge(df_subject, df_activity)
        # Merge also the train measurements by the id column created earlier
        df_train <- merge(df_subj_act, df_measures)

        # Drop the id column
        df_train <- df_train[, 2:ncol(df_train)] # drop id

        # Merge the test and train data sets vertically, 
        # i.e. add the observations of the one dataframe to the other
        df_merged <- rbind(df_test, df_train) 

        ##
        ## Assignment task 2: Extract only "mean" and "std" variables 
        ##

        # Extract the subject, activity variables and all mean and std variables 
        df_merged <- df_merged[, grep("subject|activity|\\.mean\\.|\\.std\\.", names(df_merged))]

        ##
        ## Assignment task 3: Use descriptive names for activities
        ##

        # Load all activity names/descriptions
        df_act_labels <- read.delim("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "", col.names = c("activity","activity.descr"), stringsAsFactors = FALSE)
        # Convert the activity variable to character, so that its value can be replaced with its description
        df_merged$activity <- as.character(df_merged$activity)

        # Replace all activities with their corresponding names/descriptions
        for(i in 1:nrow(df_act_labels))
        {
                df_merged$activity[df_merged$activity == df_act_labels[i, 1]] <- df_act_labels[i, 2]
        }
        
        ## Assignment task 4: Set appropritate names for the variables
        
        # Clean up the variable names
        names(df_merged) <- tolower(names(df_merged)) # convert all variables to lower case
        names(df_merged) <- sub("\\.\\.", "" , (names(df_merged))) # replace double dots with an empty string
        names(df_merged) <- sub("^t", "t\\.", names(df_merged)) # replace leading "t" with "t."
        names(df_merged) <- sub("^f", "f\\.", names(df_merged)) # replace leading "f" with "f."

        ## Assignment task 5.1: Calculate means per subject and activity
        
        # Group the data frame by subject and activity
        df_merged <- group_by(df_merged, subject, activity)
        # Calculate the mean for each column excluding the grouped by variables and ignoring NAs.
        df_merged <- summarise_each(df_merged, funs(mean(., na.rm = TRUE)))
        
        ## Assignment task 5.2: Create a tidy data set for the output 
        
        # Turn all measurement columns into rows
        df_tidy <- gather(df_merged, feature, value, -subject, -activity)
        
        # Separate/split the feature column into domain(f/t), feature(e.g. bodyaccmag),
        # measure.type(mean/std) and axis(x/y/z). Not all features have an axis.
        df_tidy <- separate(df_tidy, feature, into = c("domain", "feature", "measure.type", "axis"), sep = "\\.")
        
        # Sort final output data frame
        df_tidy <- arrange(df_tidy, subject, activity, domain, feature, measure.type)
        
        # Save the final output data frame
        write.table(df_tidy, file="tidy.txt", row.names = FALSE)
        
        df_tidy
}
