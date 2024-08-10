# Load necessary libraries
library(data.table)
library(ggplot2)

# Step 1: Download and preprocess data
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(fileUrl, temp)
unzip(temp, exdir = "data")
unlink(temp)

# Load the data into a data.table
activityDT <- fread("data/activity.csv")

# Step 2: Calculate total steps per day and summarize statistics
Total_Steps <- activityDT[, .(steps = sum(steps, na.rm = TRUE)), by = date]

# Display the first 10 rows of Total_Steps
head(Total_Steps, 10)

# Plot histogram of total steps per day
ggplot(Total_Steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")

# Calculate mean and median total steps per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]

# Step 3: Average daily activity pattern
IntervalDT <- activityDT[, .(steps = mean(steps, na.rm = TRUE)), by = interval]

# Plot time series of average steps per interval
ggplot(IntervalDT, aes(x = interval, y = steps)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")

# Find interval with the maximum average steps
IntervalDT[which.max(steps), .(max_interval = interval)]

# Step 4: Impute missing values
# Count total missing values
missing_values <- activityDT[is.na(steps), .N]

# Impute missing values with the overall median
activityDT[is.na(steps), steps := median(activityDT$steps, na.rm = TRUE)]

# Save the tidy dataset
fwrite(activityDT, "data/tidyData.csv", quote = FALSE)

# Step 5: Recalculate total steps per day after imputation
Total_Steps <- activityDT[, .(steps = sum(steps)), by = date]

# Calculate new mean and median after imputation
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]

# Plot histogram of total steps per day after imputation
ggplot(Total_Steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps After Imputation", x = "Steps", y = "Frequency")

# Step 6: Analyze activity patterns between weekdays and weekends
activityDT[, date := as.IDate(date, format = "%Y-%m-%d")]
activityDT[, `weekday or weekend` := fifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday")]

# Calculate average steps by interval and day type
IntervalDT <- activityDT[, .(steps = mean(steps, na.rm = TRUE)), by = .(interval, `weekday or weekend`)]

# Plot the average daily steps by weekday/weekend
ggplot(IntervalDT, aes(x = interval, y = steps, color = `weekday or weekend`)) +
  geom_line() +
  labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") +
  facet_wrap(~`weekday or weekend`, ncol = 1)
