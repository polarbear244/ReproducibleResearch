# Recalculate total steps per day after imputation
Total_Steps <- activityDT[, .(steps = sum(steps)), by = date]

# Calculate new mean and median
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]

# Plot histogram of total steps per day after imputation
ggplot(Total_Steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps After Imputation", x = "Steps", y = "Frequency")
