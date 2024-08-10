# Average daily activity pattern
IntervalDT <- activityDT[, .(steps = mean(steps, na.rm = TRUE)), by = interval]

# Plot time series of average steps per interval
ggplot(IntervalDT, aes(x = interval, y = steps)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")

# Find interval with the maximum average steps
IntervalDT[which.max(steps), .(max_interval = interval)]
