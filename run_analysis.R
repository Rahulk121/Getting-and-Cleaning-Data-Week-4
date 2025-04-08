
## run_analysis.R
## Original author: sjchan-ds (GitHub username)
## Optimized version
## 
## Purpose: Collect, clean, and prepare tidy data for later analysis
## 
## Input files: x_train.txt, x_test.txt, y_train.txt, y_test.txt,
##              subject_train.txt, subject_test.txt, features.txt, activity_labels.txt
## Output files: tidy_average_data.txt
##
## Required package: tidyverse (dplyr, tidyr)
##
## Usage in R:
## source("run_analysis.R")
## run_analysis()
##

run_analysis <- function() {
  # Check for required packages and load them
  required_packages <- c("dplyr", "tidyr")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if(length(missing_packages) > 0) {
    stop("Please install required packages: ", paste(missing_packages, collapse=", "), "\n")
  }
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
  })
  
  message("Starting data processing...")
  
  # Step 1: Read and merge training and test datasets
  message("Step 1: Merging training and test datasets...")
  
  # Function to read and combine datasets
  combine_datasets <- function(train_file, test_file) {
    train_data <- read.table(train_file)
    test_data <- read.table(test_file)
    rbind(train_data, test_data)
  }
  
  # Combine main data, labels and subjects
  data <- combine_datasets("./train/X_train.txt", "./test/X_test.txt")
  labels <- combine_datasets("./train/y_train.txt", "./test/y_test.txt")
  subjects <- combine_datasets("./train/subject_train.txt", "./test/subject_test.txt")
  
  # Step 2: Extract only mean and standard deviation measurements
  message("Step 2: Extracting mean and standard deviation measurements...")
  
  features <- read.table("features.txt")
  mean_std_indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
  filtered_data <- data[, mean_std_indices]
  
  # Clean column names
  col_names <- features[mean_std_indices, 2]
  col_names <- tolower(gsub("\\(|\\)", "", col_names))
  col_names <- gsub("-", ".", col_names)
  colnames(filtered_data) <- col_names
  
  # Step 3: Use descriptive activity names
  message("Step 3: Applying descriptive activity names...")
  
  activities <- read.table("activity_labels.txt")
  activities[, 2] <- tolower(gsub("_", "", activities[, 2]))
  labels[, 1] <- activities[labels[, 1], 2]
  colnames(labels) <- "activity"
  
  # Step 4: Label the dataset with descriptive names
  message("Step 4: Labeling dataset with descriptive names...")
  
  colnames(subjects) <- "subject"
  combined_data <- cbind(subjects, labels, filtered_data)
  
  # Step 5: Create tidy dataset with averages
  message("Step 5: Creating tidy dataset with averages...")
  
  tidy_data <- combined_data %>%
    group_by(activity, subject) %>%
    summarize(across(everything(), mean), .groups = "drop")
  
  # Write output to file
  write.table(tidy_data, "tidy_average_data.txt", row.names = FALSE, col.names = TRUE, sep = "\t")
  
  message("Done: Tidy data file has been created in the working directory!")
  return(TRUE)
}
