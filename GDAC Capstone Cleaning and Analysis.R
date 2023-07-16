library(tidyverse)
library(geosphere)
library(data.table)  
library(janitor)
library(dplyr)
library(lubridate)

if(!compare_df_cols_same(June22, July22, Aug22, Sept22, Oct22, Nov22, Dec22, 
                         Jan23, Feb23, Mar23, Apr23, May23))
{
  stop("These have different column names :(")
}

print("they all have the same columns!")

## Let's combine these all into one dataframe!
trips <- rbind(June22, July22, Aug22, Sept22, Oct22, Nov22, Dec22, 
               Jan23, Feb23, Mar23, Apr23, May23)

## Now let's take a look at all my columns
#glimpse(trips)
## Next we're gonna change some column names
trips$bike_type <- trips$rideable_type
trips$start_time <- trips$started_at
trips$end_time <- trips$ended_at
trips$member_type <- trips$member_casual
## Check to make sure our names took
#glimpse(trips)
## Then get rid of our now duplicate columns
trips$rideable_type <- NULL
trips$started_at <- NULL
trips$ended_at <- NULL
trips$member_casual <- NULL
print("make columns")
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
print("calculate duration")
trips <- trips %>% 
  mutate(trips, duration_min = 
    as.numeric(round(difftime(end_time, start_time, units = "min"), 0)))

## Uncomment
##str(trips)
print("remove null rows")
## Now we're going to clean up our dataset and make sure we don't have any rows
## with null values
trips_v2 <- na.omit(trips)

## looking at the summary, we see we have some outlier data points so we're
## gonna clean those out next
##summary(trips_v2)

summary(trips_v2)
print("remove duration 0 and less, latitude 0, longitude 0")
trips_v3 <- subset(trips_v2, duration_min > 0)
trips_v3 <- subset(trips_v3, end_lat != 0)
trips_v3 <- subset(trips_v3, end_lng != 0)


print("get rid of the top end")
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

print("Daily Rides by Member Type")
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

print("Average Ride Duration by Member Type and Weekday")
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

print("Quarterly Trends by Member Type")
trips_v3$quarter <- ordered(trips_v3$quarter, levels = c("First", "Second", "Third", "Fourth"))
trips_v3 %>% count(quarter, member_type)

ggplot(trips_v3, aes(x = quarter, fill = member_type)) +
  geom_bar(position = "dodge") +
  ggtitle("Quarterly Trends by Member Type", subtitle = "June 2022 to May 2023") +
  xlab("Quarter") + scale_y_continuous("Ride Count", labels = scales::comma)

quarterly_avg_dur <- trips_v3 %>%
  group_by(member_type, quarter) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(member_type, quarter)

ggplot(quarterly_avg_dur, aes(x = quarter, y = average_duration, fill = member_type)) +
  geom_col(position = "dodge") +
  ggtitle("Average Ride Duration by Member Type and Quarter", subtitle = "June 2022 to May 2023") +
  xlab("Quarter") + scale_y_continuous("Ride Duration in Minutes", labels = scales::comma)

