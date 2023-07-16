library(tidyverse)
library(geosphere)
library(data.table)  
library(janitor)
library(dplyr)
library(lubridate)

## Import my datasets
June22 <- read_csv("CSVs/202206-divvy-tripdata.csv")
July22 <- read_csv("CSVs/202207-divvy-tripdata.csv")
Aug22 <- read_csv("CSVs/202208-divvy-tripdata.csv")
Sept22 <- read_csv("CSVs/202209-divvy-tripdata.csv")
Oct22 <- read_csv("CSVs/202210-divvy-tripdata.csv")
Nov22 <- read_csv("CSVs/202211-divvy-tripdata.csv")
Dec22 <- read_csv("CSVs/202212-divvy-tripdata.csv")
Jan23 <- read_csv("CSVs/202301-divvy-tripdata.csv")
Feb23 <- read_csv("CSVs/202302-divvy-tripdata.csv")
Mar23 <- read_csv("CSVs/202303-divvy-tripdata.csv")
Apr23 <- read_csv("CSVs/202304-divvy-tripdata.csv")
May23 <- read_csv("CSVs/202305-divvy-tripdata.csv")

## Check my column names
colnames(June22)
colnames(July22)
colnames(Aug22)
colnames(Sept22)
colnames(Oct22)
colnames(Nov22)
colnames(Dec22)
colnames(Jan23)
colnames(Feb23)
colnames(Mar23)
colnames(Apr23)
colnames(May23)

## Let's make sure all of our csvs have the same column names
compare_df_cols_same(June22, July22, Aug22, Sept22, Oct22, Nov22, Dec22, 
                         Jan23, Feb23, Mar23, Apr23, May23)

## They have the same column names so let's get them all into one frame so we can work with it.
trips <- rbind(June22, July22, Aug22, Sept22, Oct22, Nov22, Dec22, 
               Jan23, Feb23, Mar23, Apr23, May23)

## Now let's take a look at all my columns
glimpse(trips)
## Next we're gonna change some column names
trips$bike_type <- trips$rideable_type
trips$start_time <- trips$started_at
trips$end_time <- trips$ended_at
trips$member_type <- trips$member_casual

## Check to make sure our names took
glimpse(trips)
## Then get rid of our now duplicate columns
trips$rideable_type <- NULL
trips$started_at <- NULL
trips$ended_at <- NULL
trips$member_casual <- NULL

## Next we add a bunch of columns for date and time information.
trips$date <- as.Date(trips$start_time)
trips$month <- format(as.Date(trips$date), "%m")
trips$day <- format(as.Date(trips$date), "%d")
trips$year <- format(as.Date(trips$date), "%Y")
trips$weekday <- format(as.Date(trips$date), "%A")
trips$start_hour <- as.numeric(format(as.POSIXct(trips$start_time), "%H"))
trips$end_hour <- as.numeric(format(as.POSIXct(trips$end_time), "%H"))
trips$quarter <- ifelse(trips$month %in% c('01', '02', '03'), "First",
                 ifelse(trips$month %in% c('04', '05', '06'), "Second",
                 ifelse(trips$month %in% c('07', '08', '09'), "Third",
                 ifelse(trips$month %in% c('10', '11', '12'), "Fourth", NA))))

## Next we calculate the duration of each trip and make a new column for that
trips <- trips %>% 
  mutate(trips, duration_min = 
    as.numeric(round(difftime(end_time, start_time, units = "min"), 0)))

## Now we check our new expanded data frame and make sure our data types are all good
glimpse(trips)
str(trips)

## Now we're going to clean up our dataset and make sure we don't have any rows
## with null values
trips_v2 <- na.omit(trips)

## Now we look at the summary, we can see we have some outlier and invalid data
## points in end_lat, end_lng, and duration_min so we're gonna clean those out next
summary(trips_v2)

trips_v3 <- subset(trips_v2, duration_min > 0)
trips_v3 <- subset(trips_v3, end_lat != 0)
trips_v3 <- subset(trips_v3, end_lng != 0)
trips_v3 <- subset(trips_v3, start_lat != 0)
trips_v3 <- subset(trips_v3, start_lng != 0)

## make a data frame sorted by duration and look at the
## longest trips to identify a good cut off point to remove
## obviously anomalous duration values
trips_by_duration <- arrange(trips_v3, duration_min)
tail(trips_by_duration, 30)

## We can see that there's a steady increase until the
## duration hits 1500 minutes(25 hours), and this seems
## as reasonable a cut off as any.
trips_v3 <- subset(trips_v3, duration_min < 1500)
## one final look at our dataframe and it looks good.
summary(trips_v3)

aggregate(trips_v3$duration_min ~ trips_v3$member_type, FUN = mean)
aggregate(trips_v3$duration_min ~ trips_v3$member_type, FUN = median)
aggregate(trips_v3$duration_min ~ trips_v3$member_type, FUN = max)
aggregate(trips_v3$duration_min ~ trips_v3$member_type, FUN = min)

## Now let's make some visualizations. Starting with daily rides by member types.
trips_v3 %>% group_by(member_type, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(duration_min)) %>%
  arrange(member_type, weekday)

trips_v3$weekday<- ordered(trips_v3$weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(trips_v3, aes(x = weekday, fill = member_type)) +
  geom_bar(position = "dodge") +
  ggtitle("Daily Rides by Member Type", subtitle = "June 2022 to May 2023") +
  xlab("Day of Week") + 
  labs(fill = "Member Type") +
  scale_y_continuous("Number of Trips", labels = scales::comma)

##Next let's look at the average ride duration by member type and weekday.
trips_v3 %>%
  group_by(member_type, weekday) %>%
  summarise(number_of_rides = n(), 
            average_duration = mean(duration_min)) %>%
  arrange(member_type, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_type)) +
  geom_col(position = "dodge") +
  ggtitle("Average Ride Duration by Member Type and Weekday", subtitle = "June 2022 to May 2023") +
  xlab("Weekday") + scale_y_continuous("Ride Duration in Minutes", labels = scales::comma) +
  labs(fill = "Member Type")

## Now some total number of rides per quarter by member type.
trips_v3$quarter <- ordered(trips_v3$quarter, levels = c("First", "Second", "Third", "Fourth"))
trips_v3 %>% count(quarter, member_type)

ggplot(trips_v3, aes(x = quarter, fill = member_type)) +
  geom_bar(position = "dodge") +
  ggtitle("Quarterly Trends by Member Type", subtitle = "June 2022 to May 2023") +
  xlab("Quarter") + scale_y_continuous("Ride Count", labels = scales::comma)

## Now average ride duration by member type per quarter.
quarterly_avg_dur <- trips_v3 %>%
  group_by(member_type, quarter) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(member_type, quarter)

ggplot(quarterly_avg_dur, aes(x = quarter, y = average_duration, fill = member_type)) +
  geom_col(position = "dodge") +
  ggtitle("Average Ride Duration by Member Type and Quarter", subtitle = "June 2022 to May 2023") +
  xlab("Quarter") + scale_y_continuous("Ride Duration in Minutes", labels = scales::comma)

