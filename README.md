Getting and Cleaning Data - Course Project

This is the course project for the Getting and Cleaning Data Coursera course. The R script, run_analysis.R, does the following:

a. Download the dataset if it does not already exist in the working directory
b. Load the activity and feature info
c. Loads both the training and test datasets, keeping only those columns which reflect a mean or standard deviation
d. Loads the activity and subject data for each dataset, and merges those columns with the dataset
e. Merges the two datasets
f. Converts the activity and subject columns into factors
g. Creates a tidy dataset that consists of the average (mean) value of each variable for each subject and activity pair.

The end result is shown in the file tidy.txt.
