# Calculate total steps per day
Total_Steps <- activityDT[, .(steps = sum(steps, na.rm = TRUE)), by = date]
head(Total_Steps, 10)

# Plot histogram of total steps per day
ggplot(Total_Steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")

# Calculate mean and median total steps per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
