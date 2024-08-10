# Count total missing values
missing_values <- activityDT[is.na(steps), .N]

# Impute missing values with the overall median
activityDT[is.na(steps), steps := median(activityDT$steps, na.rm = TRUE)]

# Save the tidy dataset
fwrite(activityDT, "data/tidyData.csv", quote = FALSE)
