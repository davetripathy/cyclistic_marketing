#Join the dataframes vertically
#Make separate dataframes for the Year 2020 and the year 2021

#DataFrame for the YEAR 2020
data_cyclistic_2020 <- rbind(X202012_divvy_tripdata , X202011_divvy_tripdata,
                             X202010_divvy_tripdata , X202009_divvy_tripdata,
                             X202008_divvy_tripdata , X202007_divvy_tripdata,
                             X202006_divvy_tripdata , X202005_divvy_tripdata,
                             X202004_divvy_tripdata )
#DataFrame for the Year 2021

data_cyclistic_2021 <- rbind(X202104_divvy_tripdata ,X202103_divvy_tripdata ,
                             X202102_divvy_tripdata ,X202101_divvy_tripdata )

#Combine the two dataframe to get the Final DataFrame Having data
#for all 12 months
data_cyclistic_all_unclean <- rbind(data_cyclistic_2021 ,data_cyclistic_2020)


data_cyclistic_all <- rbind(data_cyclistic_2021 ,data_cyclistic_2020)

#Install and Load the Packages

install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
library(tidyverse)
library(dplyr)
library(tidyr)


#Drop the N/A and Blank Columns
data_cyclistic_all <- drop_na(data_cyclistic_all)

#Drop the Duplicated rows based on ride_id
data_cyclistic_all <- distinct(data_cyclistic_all,ride_id, .keep_all = TRUE)


#Drop the rows where STARTING TIME IS LESS THAN ENDING TIME
data_cyclistic_all_v2 <- data_cyclistic_all[!(data_cyclistic_all$start_station_name == "HQ QR" | data_cyclistic_all$ride_length<0),]

#OR use this

data_cyclistic_all <- subset( data_cyclistic_all , data_cyclistic_all$started_at <= data_cyclistic_all$ended_at)

#Calculate the duration of the ride in MINUTES using difftime()
data_cyclistic_all <- data_cyclistic_all %>% 
  mutate(ride_length = round(difftime(ended_at , started_at , units = "mins"), 2))

# Determine the day of the week in which the ride started using weekdays function
data_cyclistic_all <- data_cyclistic_all %>% 
  mutate(day_of_week = weekdays(started_at))

#Sort your Data with respect to start_date in descending order
#That is recent date come first

data_cyclistic_all <- data_cyclistic_all %>% 
  arrange(rev(started_at))


#CHECK the datatype of each column

str(data_cyclistic_all)

#Add additional labels such as Day , Month , Year etc for the purpose of aggregation

data_cyclistic_all$date <- data_cyclistic_all$started_at
#The default format is yyyy-mm-dd
data_cyclistic_all$month <- format(data_cyclistic_all$date, "%m")
data_cyclistic_all$day <- format(data_cyclistic_all$date, "%d")
data_cyclistic_all$year <- format(data_cyclistic_all$date, "%Y")

#Remove all the LATITUDE AND LONGITUDE CORDINATES


data_cyclistic_all <- data_cyclistic_all %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

#Summary of the Data
summary(data_cyclistic_all)

#Convert ride length to numberic
is.factor(data_cyclistic_all$ride_length)
data_cyclistic_all$ride_length <- as.numeric(as.character(data_cyclistic_all$ride_length))
is.numeric(data_cyclistic_all$ride_length)

#A Descriptive Summary of ride length
summary(data_cyclistic_all$ride_length)


#Summary based on Ride Type
# Compare members and casual users

aggregate(data_cyclistic_all$ride_length ~ data_cyclistic_all$member_casual, FUN = mean)
aggregate(data_cyclistic_all$ride_length ~ data_cyclistic_all$member_casual, FUN = median)
aggregate(data_cyclistic_all$ride_length ~ data_cyclistic_all$member_casual, FUN = max)
aggregate(data_cyclistic_all$ride_length ~ data_cyclistic_all$member_casual, FUN = min)


# See the average ride time by each day for members vs casual users
aggregate(data_cyclistic_all$ride_length ~ (data_cyclistic_all$member_casual + data_cyclistic_all$day_of_week), FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
data_cyclistic_all$day_of_week <- ordered(data_cyclistic_all$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(data_cyclistic_all$ride_length ~ data_cyclistic_all$member_casual + data_cyclistic_all$day_of_week, FUN = mean)


# Now, let's run the median ride time by each day for members vs casual users
aggregate(data_cyclistic_all$ride_length ~ data_cyclistic_all$member_casual + data_cyclistic_all$day_of_week, FUN = median)

#Now lets calculate the Average Duration and Number of rides wrt Weekday
table_weekly <- data_cyclistic_all %>% 
  
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length) , median_duration = median(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)


#Now lets see this with respect to months to take into account seasons

#Now lets calculate the Average Duration and Number of rides wrt months

table_monthly <- data_cyclistic_all %>% 
  
  group_by(member_casual, month) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, month)

#Now lets calculate the Average Duration and Number of rides wrt year and month


table_monthly_yearly <- #Now lets calculate the Average Duration and Number of rides wrt year and month
  data_cyclistic_all %>% 
  
  group_by(member_casual,year, month) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length), median_duration = median(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, year, month)

#Save the Dataset 
write.csv(data_cyclistic_all, file = "C:\\Users\\lenovo\\Desktop\\DebasishPD\\PythonDS\\Google Data Analysis\\Cyclistic_data.csv")

#Save the other Datasets
write.csv(table_weekly , file = "C:\\Users\\lenovo\\Desktop\\DebasishPD\\PythonDS\\Google Data Analysis\\Cyclistic_weekly.csv")

write.csv(table_monthly_yearly, file = "C:\\Users\\lenovo\\Desktop\\DebasishPD\\PythonDS\\Google Data Analysis\\Cyclistic_yearmonth.csv")

