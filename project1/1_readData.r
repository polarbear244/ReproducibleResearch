library(data.table)
library(ggplot2)

# Download and unzip the data
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(fileUrl, temp)
unzip(temp, exdir = "data")
unlink(temp)

# Load the data into a data.table
activityDT <- fread("data/activity.csv")
