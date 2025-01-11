# Loading necessary libraries
library(tidyverse)  
library(lubridate)   
library(dplyr)      
library(ggplot2)     
library(tidyr)       


# Importing the data
activity <- read.csv('/Users/bimsaradias/Downloads/FitBit Fitness Tracker Data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/Used files/dailyActivity_merged.csv')
calories <- read.csv('/Users/bimsaradias/Downloads/FitBit Fitness Tracker Data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/Used files/hourlyCalories_merged.csv')
intensities <- read.csv('/Users/bimsaradias/Downloads/FitBit Fitness Tracker Data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/Used files/hourlyIntensities_merged.csv')
sleep <- read.csv('/Users/bimsaradias/Downloads/FitBit Fitness Tracker Data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/Used files/sleepDay_merged.csv')
weight <- read.csv('/Users/bimsaradias/Downloads/FitBit Fitness Tracker Data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/Used files/weightLogInfo_merged.csv')



# Data Cleaning
# Create a list of your data frames for summary analysis
data_list <- list(activity, calories, intensities, sleep, weight)

# Apply summary() to each data frame in the list to see basic statistics
lapply(data_list, summary)

# Check if there are any missing values across all datasets
any(is.na(data_list))


# Data Preprocessing
# Convert time columns to proper date-time format
# intensities
intensities$ActivityHour = as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")

# calories
calories$ActivityHour = as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")

# activity
activity$ActivityDate = as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%Y-%m-%d")

# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
colnames(sleep)[colnames(sleep) == "SleepDay"] <- "date"
View(sleep)


# Exploratory Data Analysis

# Show a summary of key columns in activity
activity %>%
  select(TotalSteps, TotalDistance, SedentaryMinutes, Calories) %>%
  summary()

# Explore the number of active minutes per category
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

# Show a summary of calories data
calories %>%
  select(Calories) %>%
  summary()

# Show a summary of sleep data
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

# Show a summary of weight data
weight %>%
  select(WeightKg, BMI) %>%
  summary()

# Merge the 'sleep' and 'activity' datasets based on the 'Id' and 'date' columns
merged_data <- merge(sleep, activity, by=c('Id', 'date'))
head(merged_data)

# Visualizations

# Visualization: Total Steps vs. Calories
ggplot(data=activity, aes(x=TotalSteps, y=Calories, color=Calories)) + 
  geom_point() + 
  geom_smooth(color='black') + 
  labs(title="Total Steps vs. Calories") + 
  scale_color_gradient(low = "blue", high = "red")  # Custom gradient from blue to red

# Histogram: Distribution of Total Minutes Asleep
ggplot(data=sleep, aes(x=TotalMinutesAsleep)) +
  geom_histogram(binwidth=10, fill="purple", color="white") +
  labs(title="Distribution of Total Minutes Asleep", x="Minutes Asleep", y="Count")

# Scatter plot: Total Minutes Asleep vs. Total Time in Bed with custom color gradient
ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed, colour=TotalSleepRecords)) + 
  geom_point() + 
  geom_smooth(color='black') +
  labs(title="Total Minutes Asleep vs. Total Time in Bed") +
  scale_color_gradient(low = "orange", high = "green")

# Data Transformation

# Create a new column 'TimeOfDay' for classification of calories by time of day
calories <- calories %>%
  mutate(TimeOfDay = case_when(
    hour(ActivityHour) >= 5 & hour(ActivityHour) < 12 ~ "Morning",
    hour(ActivityHour) >= 12 & hour(ActivityHour) < 17 ~ "Afternoon",
    hour(ActivityHour) >= 17 & hour(ActivityHour) < 21 ~ "Evening",
    TRUE ~ "Night"
  ))

# Boxplot: Calories Burned by Time of Day
ggplot(data=calories, aes(x=factor(TimeOfDay), y=Calories, fill=TimeOfDay)) +
  geom_boxplot() +
  labs(title="Calories Burned by Time of Day", x="Time of Day", y="Calories") +
  scale_fill_brewer(palette="Set2")  

# Line plot: Daily Steps Trend over time
ggplot(data=activity, aes(x=ActivityDate, y=TotalSteps)) +
  geom_line() +
  labs(title="Daily Steps Trend", x="Date", y="Total Steps")

# Reshaping the data for activity levels
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  gather(key="ActivityLevel", value="Minutes") %>%
  mutate(ActivityLevel = factor(ActivityLevel, 
                                levels = c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes"),
                                labels = c("Very Active", "Fairly Active", "Lightly Active"))) %>%
  ggplot(aes(x=ActivityLevel, y=Minutes, fill=ActivityLevel)) +
  geom_bar(stat="identity") +
  labs(title="Active Minutes per Category", x="Activity Level", y="Minutes")

# Scatter plot: Minutes Asleep vs. Sedentary Minutes
ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='darkblue') + geom_smooth(color='red') +
  labs(title="Minutes Asleep vs. Sedentary Minutes", x="Total Minutes Asleep", y="Sedentary Minutes")
