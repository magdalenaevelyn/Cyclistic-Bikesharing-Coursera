# Install Packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

# Load Library
library(tidyverse)
library(lubridate)
library(ggplot2)

# Get working directory
getwd()

#=====================
# STEP 1: COLLECT DATA
#=====================
# Dataset from November 2022 until October 2023
november_2022 <- read_csv("divvy-cyclistic-dataset/202211-divvy-tripdata.csv")
december_2022 <- read_csv("divvy-cyclistic-dataset/202212-divvy-tripdata.csv")
january_2023 <- read_csv("divvy-cyclistic-dataset/202301-divvy-tripdata.csv")
february_2023 <- read_csv("divvy-cyclistic-dataset/202302-divvy-tripdata.csv")
march_2023 <- read_csv("divvy-cyclistic-dataset/202303-divvy-tripdata.csv")
april_2023 <- read_csv("divvy-cyclistic-dataset/202304-divvy-tripdata.csv")
may_2023 <- read_csv("divvy-cyclistic-dataset/202305-divvy-tripdata.csv")
june_2023 <- read_csv("divvy-cyclistic-dataset/202306-divvy-tripdata.csv")
july_2023 <- read_csv("divvy-cyclistic-dataset/202307-divvy-tripdata.csv")
august_2023 <- read_csv("divvy-cyclistic-dataset/202308-divvy-tripdata.csv")
september_2023 <- read_csv("divvy-cyclistic-dataset/202309-divvy-tripdata.csv")
october_2023 <- read_csv("divvy-cyclistic-dataset/202310-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
colnames(november_2022)
colnames(december_2022)
colnames(january_2023)
colnames(february_2023)
colnames(march_2023)
colnames(april_2023)
colnames(may_2023)
colnames(june_2023)
colnames(july_2023)
colnames(august_2023)
colnames(september_2023)
colnames(october_2023)

# Inspect the dataframes and look for incongruencies
str(november_2022)
str(december_2022)
str(january_2023)
str(february_2023)
str(march_2023)
str(april_2023)
str(may_2023)
str(june_2023)
str(july_2023)
str(august_2023)
str(september_2023)
str(october_2023)

# Convert ride_id and rideable_type to character so that they can stack correctly
november_2022 <- mutate(november_2022, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
december_2022 <- mutate(december_2022, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
january_2023 <- mutate(january_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
february_2023 <- mutate(february_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
march_2023 <- mutate(march_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
april_2023 <- mutate(april_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
may_2023 <- mutate(may_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
june_2023 <- mutate(june_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
july_2023 <- mutate(july_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
august_2023 <- mutate(august_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
september_2023 <- mutate(september_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
october_2023 <- mutate(october_2023, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))

#  Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(november_2022, december_2022, january_2023, february_2023, march_2023, april_2023, may_2023, june_2023, july_2023, august_2023, september_2023, october_2023)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
# 1. List of coloumn names
colnames(all_trips)

# 2. How many rows are in data frame?
nrow(all_trips)

# 3. Dimensions of the data frame?
dim(all_trips)

# 4. Head of data frame
head(all_trips)

# 5. Tail of data frame
tail(all_trips)

# 6. See list of columns and data types (numeric, character, etc)
str(all_trips)

#7. Statistical summary data
summary(all_trips)

# There are a few problems we will need to fix:
# (1) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (2) We will add "ride_length" to the entire dataframe for consistency.
# (3) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# Check the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) # The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$start_hour = format(as.POSIXct(all_trips$started_at), "%H")
all_trips$end_hour = format(as.POSIXct(all_trips$ended_at), "%H")

# Add a "ride_length" calculation to all_trips (in minutes)
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at, units = "mins")

# Inspect new column
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove missing values (NA)
all_trips <- na.omit(all_trips)

# Remove negative ride length and weirdly station name
all_trips_v2 <- all_trips[
  !(all_trips$start_station_name == "CHECK" | all_trips$start_station_name == "TEST" | all_trips$start_station_name == "DIVVY" |
    all_trips$start_station_name == "" |
    all_trips$ride_length < 0
),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in minutes)
mean(all_trips_v2$ride_length) # straight average (total ride length / rides)
median(all_trips_v2$ride_length) # midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) # longest ride
min(all_trips_v2$ride_length) # shortest ride

summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# --------------- WEEKDAYS TRENDS --------------- 
# 1. Analyze weekday ridership by type
weekdays_count <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  # creates weekday field using wday()
  group_by(member_casual, weekday) %>%                  # groups by usertype and weekday
  summarise(number_of_rides = n()						# calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 		    # calculates the average duration
  arrange(member_casual, weekday)						# sorts
print(weekdays_count)

ggplot(all_trips_v2, aes(x = day_of_week, fill = member_casual)) +
  geom_bar(position = 'dodge') +
  ggtitle('Daily Ridership by User Type', subtitle = 'November 2022 - October 2023') + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Day') + ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#5DADE2", "#AED6F1"), labels = c("Casual", "Member"))

# 2. Analyze weekday ridership by duration & type
weekdays_duration_count <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  # creates weekday field using wday()
  group_by(member_casual, weekday) %>%                  # groups by usertype and weekday
  summarise(number_of_rides = n()						# calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 		    # calculates the average duration
  arrange(member_casual, weekday)    				    # sorts
print(weekdays_duration_count)

ggplot(weekdays_duration_count, aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = 'dodge') +
  ggtitle('Ride Duration by User Type and Day', subtitle = 'November 2022 - October 2023') + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Day') + ylab('Ride Duration (sec)') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#5DADE2", "#AED6F1"), labels = c("Casual", "Member"))

# --------------- RIDE TYPE ---------------
ride_type <- all_trips_v2 %>%
  count(rideable_type, member_casual)  %>%
  mutate(percentage = (n / nrow(all_trips_v2)) * 100)
print(ride_type)

ggplot(all_trips_v2, aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = 'dodge') +
  ggtitle('Ride Type', subtitle = 'November 2022 - October 2023') + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Ride Type') + ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#5DADE2", "#AED6F1"), labels = c("Casual", "Member"))

# --------------- MONTHLY TRENDS --------------- 
# 1. Analyze monthly ridership by type
monthly_count <- all_trips_v2 %>% count(month, member_casual)
print(monthly_count, n = 24)

ggplot(all_trips_v2, aes(x = month, fill = member_casual)) +
  geom_bar(position = 'dodge') +
  ggtitle('Monthly Ridership by User Type', subtitle = 'November 2022 - October 2023') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Month') + ylab('Ride Count') + 
  labs(fill='User Type') +
  labs(caption = 'Months represented in number (MM)') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#5DADE2", "#AED6F1"), labels = c("Casual", "Member"))

# 2. Analyze monthly ridership by duration & type
monthly_duration_count <- all_trips_v2 %>%
  group_by(member_casual, month) %>%                    # groups by usertype and month
  summarise(number_of_rides = n()						# calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 		    # calculates the average duration
  arrange(member_casual, month)
print(monthly_duration_count, n = 24)

ggplot(monthly_duration_count, aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = 'dodge') +
  ggtitle('Ride Duration (Monthly) by User Type and Day', subtitle = 'November 2022 - October 2023') + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Day') + ylab('Ride Duration (sec)') + 
  labs(fill='User Type') +
  labs(caption = 'Months represented in number (MM)') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#5DADE2", "#AED6F1"), labels = c("Casual", "Member"))

# --------------- STATION TRENDS --------------- 
popular_10_start_station <- all_trips_v2 %>% count(start_station_name, member_casual)
popular_10_end_station <- all_trips_v2 %>% count(end_station_name, member_casual)

# 1. Casual - Start Station
casual_station_start <- filter(popular_10_start_station, member_casual == 'casual')
casual_station_start <- casual_station_start %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) 

ggplot(casual_station_start, aes(x = reorder(start_station_name, -n), y = n)) +
  geom_bar(stat = 'identity', fill = '#5DADE2') +
  ggtitle('10 Popular Start Stations by Casuals', subtitle = 'November 2022 - October 2023') + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip()

# 2. Casual - End Station
casual_station_end <- filter(popular_10_end_station, member_casual == 'casual')
casual_station_end <- casual_station_end %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) 

ggplot(casual_station_end, aes(x = reorder(end_station_name, -n), y = n)) +
  geom_bar(stat = 'identity', fill = '#5DADE2') +
  ggtitle('10 Popular End Stations by Casuals', subtitle = 'November 2022 - October 2023') + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip()

# 3. Member - Start Station
member_station_start <- filter(popular_10_start_station, member_casual == 'member')
member_station_start <- member_station_start %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) 

ggplot(member_station_start, aes(x = reorder(start_station_name, -n), y = n)) +
  geom_bar(stat = 'identity', fill = '#AED6F1') +
  ggtitle('10 Popular Start Stations by Member', subtitle = 'November 2022 - October 2023') + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip()

# 4. Member - End Station
member_station_end <- filter(popular_10_end_station, member_casual == 'member')
member_station_end <- member_station_end %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) 

ggplot(member_station_end, aes(x = reorder(end_station_name, -n), y = n)) +
  geom_bar(stat = 'identity', fill = '#AED6F1') +
  ggtitle('10 Popular End Stations by Member', subtitle = 'November 2022 - October 2023') + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip()

# --------------- HOUR TRENDS --------------- 
# 1. Start Hour
ggplot(all_trips_v2, aes(x = start_hour, fill = member_casual)) +
  geom_bar(position = 'dodge') + 
  ggtitle('Start Hour Variation by User Type', subtitle = 'November 2022 - October 2023') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('Start Hour') + ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#5DADE2", "#AED6F1"), labels = c("Casual", "Member"))

# 2. End Hour
ggplot(all_trips_v2, aes(x = end_hour, fill = member_casual)) +
  geom_bar(position = 'dodge') +
  ggtitle('End Hour Variation by User Type', subtitle = 'November 2022 - October 2023') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +    
  xlab('End Hour') + ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#5DADE2", "#AED6F1"), labels = c("Casual", "Member"))


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
write.csv(all_trips_v2, file = "C:/Users/Lenovo/Desktop/avg_ride_length_v2.csv")

