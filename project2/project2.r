#project 2
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# Download the dataset
file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(file_url, destfile = "StormData.csv.bz2")

# Read the data
storm_data <- read_csv("StormData.csv.bz2")

# Convert the 'EVTYPE' column to a factor
storm_data$EVTYPE <- as.factor(storm_data$EVTYPE)

# Select relevant columns for analysis
storm_data_subset <- storm_data %>%
  select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

# Create a function to convert PROPDMGEXP and CROPDMGEXP to numeric multipliers
exp_to_num <- function(exp) {
  ifelse(exp %in% c("K", "k"), 1e3,
  ifelse(exp %in% c("M", "m"), 1e6,
  ifelse(exp %in% c("B", "b"), 1e9, 1)))
}

# Apply the function to the dataset
storm_data_subset <- storm_data_subset %>%
  mutate(PROPDMGEXP = exp_to_num(PROPDMGEXP),
         CROPDMGEXP = exp_to_num(CROPDMGEXP)) %>%
  mutate(PROPDMG_TOTAL = PROPDMG * PROPDMGEXP,
         CROPDMG_TOTAL = CROPDMG * CROPDMGEXP)

# Aggregate the data by event type for health impact
health_impact <- storm_data_subset %>%
  group_by(EVTYPE) %>%
  summarise(total_fatalities = sum(FATALITIES, na.rm = TRUE),
            total_injuries = sum(INJURIES, na.rm = TRUE)) %>%
  mutate(total_health_impact = total_fatalities + total_injuries) %>%
  arrange(desc(total_health_impact))

# Aggregate the data by event type for economic impact
economic_impact <- storm_data_subset %>%
  group_by(EVTYPE) %>%
  summarise(total_property_damage = sum(PROPDMG_TOTAL, na.rm = TRUE),
            total_crop_damage = sum(CROPDMG_TOTAL, na.rm = TRUE)) %>%
  mutate(total_economic_impact = total_property_damage + total_crop_damage) %>%
  arrange(desc(total_economic_impact))

# Plot the top 10 events with the highest health impact
top_health_impact <- health_impact[1:10,]

ggplot(top_health_impact, aes(x = reorder(EVTYPE, -total_health_impact), y = total_health_impact)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Event Type", y = "Total Health Impact (Fatalities + Injuries)",
       title = "Top 10 Weather Events with Highest Health Impact")

# Plot the top 10 events with the highest economic impact
top_economic_impact <- economic_impact[1:10,]

ggplot(top_economic_impact, aes(x = reorder(EVTYPE, -total_economic_impact), y = total_economic_impact)) +
  geom_bar(stat = "identity", fill = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Event Type", y = "Total Economic Impact (Property + Crop Damage)",
       title = "Top 10 Weather Events with Highest Economic Impact")

