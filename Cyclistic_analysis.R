install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("stringr")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(stringr)



trips_01_2023 <- read.csv("202301-divvy-tripdata.csv")
trips_02_2023 <- read.csv("202302-divvy-tripdata.csv")
trips_03_2023 <- read.csv("202303-divvy-tripdata.csv")
trips_04_2023 <- read.csv("202304-divvy-tripdata.csv")
trips_05_2023 <- read.csv("202305-divvy-tripdata.csv")
trips_06_2023 <- read.csv("202306-divvy-tripdata.csv")
trips_07_2023 <- read.csv("202307-divvy-tripdata.csv")
trips_08_2023 <- read.csv("202308-divvy-tripdata.csv")
trips_09_2023 <- read.csv("202309-divvy-tripdata.csv")
trips_10_2023 <- read.csv("202310-divvy-tripdata.csv")
trips_11_2023 <- read.csv("202311-divvy-tripdata.csv")
trips_12_2023 <- read.csv("202312-divvy-tripdata.csv")


glimpse(trips_01_2023)

cyclistic_trips_2023 <- rbind(trips_01_2023,
                              trips_02_2023,
                              trips_03_2023,
                              trips_04_2023,
                              trips_05_2023,
                              trips_06_2023,
                              trips_07_2023,
                              trips_08_2023,
                              trips_09_2023,
                              trips_10_2023,
                              trips_11_2023,
                              trips_12_2023)

glimpse(cyclistic_trips_2023)

# Eliminate any duplicates in ride_id. This is the unique primary ID field. 
cyclistic_trips_2023 %>%
  distinct(ride_id, .keep_all = TRUE)

# Look for NA values
sapply(cyclistic_trips_2023, function(x) sum(is.na(x)))
# Remove NA entries
cyclistic_trips_2023_clean <- na.omit(cyclistic_trips_2023)

glimpse(cyclistic_trips_2023_clean)
# 6990 entries were removed.

# Checking to see if "casual" and "member" are the only 2 options in member_casual
unique(cyclistic_trips_2023_clean$member_casual)
# Checking to see if "electric_bike" "classic_bike" and "docked_bike" are the only options in rideable_type.
unique(cyclistic_trips_2023_clean$rideable_type)

# Add a ride length column
cyclistic_trips_2023_clean$ride_length <- difftime(cyclistic_trips_2023_clean$ended_at, cyclistic_trips_2023_clean$started_at)
range(cyclistic_trips_2023_clean$ride_length)

# Converting the ride_length into hours, min, seconds
cyclistic_trips_2023_clean = cyclistic_trips_2023_clean %>%
  mutate(hours = hour(seconds_to_period(cyclistic_trips_2023_clean$ride_length)),
         minutes = minute(seconds_to_period(cyclistic_trips_2023_clean$ride_length)),)

# Add date
cyclistic_trips_2023_clean$date <- as.Date(cyclistic_trips_2023_clean$started_at)

# Add trip_month column
cyclistic_trips_2023_clean$month <- format(as.Date(cyclistic_trips_2023_clean$date), "%m")

# Add day_of_week column
cyclistic_trips_2023_clean$day_of_week <- format(as.Date(cyclistic_trips_2023_clean$date), "%A")

# Remove entries with a negative trip length and docked bikes. Docked bikes have been taken out of circulation so we remove them from our analysis.
trip_data = cyclistic_trips_2023_clean[!(cyclistic_trips_2023_clean$rideable_type == "docked_bike" | cyclistic_trips_2023_clean$ride_length<0),]

glimpse(trip_data)

# Remove any rows with empty strings for the start_station_name, end station, or trip id
trip_data = trip_data[!trip_data$ride_id=="",]
trip_data = trip_data[!trip_data$start_station_name=="",]
trip_data = trip_data[!trip_data$end_station_name=="",]


# Analyzing the data
trip_data$day_of_week <- factor(trip_data$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday"))

#Average ride length by day of the week
trip_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_duration = mean(minutes), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(width = 0.4, position = position_dodge(width = 0.5)) +
  labs(title = "Average ride length by day of the week", x = 'Day of Week', y = 'Average Trip Duration')

#Total Ride Count by day of the week
trip_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = format(number_of_rides, scientific = FALSE), fill = member_casual)) +
  geom_col(width = 0.4, position = position_dodge(width = 0.5)) +
  labs(title = "Total Ride Count by day of the week", x = 'Day of Week', y = 'Total Rides')

# average ride length by month
trip_data %>%
  group_by(member_casual, month) %>%
  summarise(average_duration = mean(minutes), .groups = "drop") %>%
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(width = 0.4, position = position_dodge(width = 0.5)) +
  labs(title = "Average ride length by Month", x = 'Month', y = 'Average Trip Duration')

# Total Ride count by month
trip_data %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = month, y = format(number_of_rides, scientific = FALSE), fill = member_casual)) +
  geom_col(width = 0.4, position = position_dodge(width = 0.5)) +
  labs(title = "Total Ride Count by Month", x = 'Month', y = 'Total Rides')

# Preferred bike
trip_data %>%
  group_by(rideable_type, member_casual)%>%
  summarize(number_of_trips = n(), .groups = "drop")%>%
  ggplot(aes(x = rideable_type, y = number_of_trips, fill = member_casual)) +
  geom_bar(stat = 'identity') +
  labs(title = "Total Rides by Bike Type") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

