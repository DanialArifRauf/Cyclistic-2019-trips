## Case Study Scenario <br>
In this case study, I am performing real-world tasks as a junior analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve my recommendations, so they must be backed up with compelling data insights and professional data visualizations.

I will be using Cyclistic's 2019 trip data to analyze and identify trends. ***The data has been made available to public by Motivate International Inc. under this [license](https://divvybikes.com/data-license-agreement)***.

## About Cyclistic <br>
A bike-share program that features more than 5,800 bicycles and 600 docking stations. Cyclistic sets itself apart by also offering reclining bikes, hand tricycles, and cargo bikes, making bike-share more inclusive to people with disabilities and riders who can’t use a standard two-wheeled bike. The majority of riders opt for traditional bikes; about 8% of riders use the assistive options. Cyclistic users are more likely to ride for leisure, but about 30% use them to commute to work each day.

Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.

Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno (the director of marketing and my manager) believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.

Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends.

## Tech Stack
- R for data cleaning and analysis
- R packages ~ tidyverse, lubridate, ggplot2
- Tableu for visualizations

### Tableu dashboard can be viewed [here](https://public.tableau.com/views/Cyclistic2019trips/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link) 
<br>

## R code for cleaning and analysis

```
# Installing packages for data cleaning, manipulation
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

# Load packages
library("tidyverse")
library("lubridate")
library("ggplot2")

# Load CSV files
q1 <- read.csv("Divvy_Trips_2019_Q1.csv")
q2 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4 <- read.csv("Divvy_Trips_2019_Q4.csv")

#Comparing column names before combining CSV files
colnames(q1)
colnames(q2)
colnames(q3)
colnames(q4)

#Comparing data to look for inconsistencies
str(q1)
str(q2)
str(q3)
str(q4)

#Rename Q2 column names for consistency
q2 <- rename(q2, trip_id = "X01...Rental.Details.Rental.ID",
		start_time = "X01...Rental.Details.Local.Start.Time",
		end_time = "X01...Rental.Details.Local.End.Time",
		bikeid = "X01...Rental.Details.Bike.ID",
		tripduration = "X01...Rental.Details.Duration.In.Seconds.Uncapped",
		from_station_id = "X03...Rental.Start.Station.ID",
		from_station_name = "X03...Rental.Start.Station.Name",
		to_station_id = "X02...Rental.End.Station.ID",
		to_station_name = "X02...Rental.End.Station.Name",
		usertype = "User.Type",
		gender = "Member.Gender",
		birthyear = "X05...Member.Details.Member.Birthday.Year")

#Converting "trip_id" and "bikeid" to character data type to stack properly
q1 <- mutate(q1, trip_id = as.character(trip_id), bikeid = as.character(bikeid))
q2 <- mutate(q2, trip_id = as.character(trip_id), bikeid = as.character(bikeid))
q3 <- mutate(q3, trip_id = as.character(trip_id), bikeid = as.character(bikeid))
q4 <- mutate(q4, trip_id = as.character(trip_id), bikeid = as.character(bikeid))

#Combining CSV files into one dataframe
#q3q4 <- bind_rows(q3, q4)

#Comparing column names before combining CSV fiels into one dataframe
colnames(q1)
colnames(q2)
colnames(q3)
colnames(q4)

#Inspecting dataframes for inconsistencies
str(q1)
str(q2)
str(q3)
str(q4)

#Convert 'bikeid', 'trip_id' to character datatype for Q2
q2 <- mutate(q2, bikeid = as.character(bikeid),
		trip_id = as.character(trip_id))

#Combining quarterly files into one dataframe
trips_2019 <- bind_rows(q1, q2, q3, q4)

#Removing individual csv files to clear up environment space
remove(q1, q2, q3, q4)

#Inspecting merged dataframe
colnames(trips_2019) #Column names
dim(trips_2019) #Dataframe dimensions (rows and columns)
summary(trips_2019) #Summary for dataframe
head(trips_2019) #First 6 rows

#Checking how many users fall under each 'user type'
table(trips_2019$usertype)

#Adding columns for date, month, day, and year to aggregate data
trips_2019$date <- as.Date(trips_2019$start_time)
trips_2019$month <- format(as.Date(trips_2019$date), "%m")
trips_2019$day <- format(as.Date(trips_2019$date), "%d")
trips_2019$year <- format(as.Date(trips_2019$date), "%Y")
trips_2019$day_of_week <- format(as.Date(trips_2019$date), "%A")

#Adding column for trip length in mins
trips_2019$ride_length <- difftime(trips_2019$end_time, trips_2019$start_time)

#Converting ride_length from factor to numeric type
trips_2019$ride_length <- as.numeric(as.character(trips_2019$ride_length))

#Adding new dataframe; removing negative 'ride_length' values
trips_2019v2 <- trips_2019[!(trips_2019$ride_length<=0),]

#Removing NA rows
trips_2019v2 <- na.omit(trips_2019v2)

#Adding new column for ride_length in seconds
trips_2019v2 = trips_2019v2 %>% mutate(ride_length_s = ride_length * 60)

#Conducting analysis
nrow(trips_2019v2) #Number of rides
mean(trips_2019v2$ride_length_s) #Average ride length (total ride length/rides)
median(trips_2019v2$ride_length_s) #Midpoint for ride lengths
max(trips_2019v2$ride_length_s) #Longest ride
min(trips_2019v2$ride_length_s) #Shortest ride

#Viewing 4 lines above in one summary line
summary(trips_2019v2$ride_length_s)

#Comparing 'customer' usertypes versus 'subscriber'
aggregate(trips_2019v2$ride_length_s ~ trips_2019v2$usertype, FUN = mean) #average ride length by usertype
aggregate(trips_2019v2$ride_length_s ~ trips_2019v2$usertype, FUN = median) #median ride length by usertype
aggregate(trips_2019v2$ride_length_s ~ trips_2019v2$usertype, FUN = max) #longest ride by usertype
aggregate(trips_2019v2$ride_length_s ~ trips_2019v2$usertype, FUN = min) #shortest ride by usertype

#Selecting average ride time by user type for each day of the week
aggregate(trips_2019v2$ride_length_s ~ trips_2019v2$usertype + trips_2019v2$day_of_week, FUN=mean)

#Modifying days_of_week to start with Sunday
trips_2019v2$day_of_week <- ordered(trips_2019v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Running average ride time by user type for day of the week again
aggregate(trips_2019v2$ride_length_s ~ trips_2019v2$usertype + trips_2019v2$day_of_week, FUN=mean)

#Analyze data by usertype and weekday
trips_2019v2 %>% mutate(weekday = wday(start_time, label=TRUE)) %>% group_by(usertype, weekday) %>% summarise(number_of_rides=n(), average_duration = mean(ride_length)) %>% arrange(usertype, weekday)

#Visualizing number of riders by rider_type
trips_2019v2 %>% mutate(weekday = wday(start_time, label=TRUE)) %>% group_by(usertype, weekday) %>% summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% arrange(usertype, weekday) %>% ggplot(aes(x = weekday, y = number_of_rides, fill=usertype)) + geom_col(position="dodge") + scale_y_continuous(name="number_of_rides", labels = scales::comma)

#Visualizing average duration by usertype
trips_2019v2 %>% mutate(weekday = wday(start_time, label=TRUE)) %>% group_by(usertype, weekday) %>% summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% arrange(usertype, weekday) %>% ggplot(aes(x=weekday, y=average_duration, fill=usertype)) + geom_col(position = "dodge") + scale_y_continuous(name="number_of_rides", labels = scales::comma)

#Adding new dataframe with selected columns needed for analysis and visualization
trips_tableu2 <- trips_2019v2 %>% select(-c(trip_id, from_station_name, to_station_name, from_station_id, to_station_id, gender, birthyear, bikeid))

#Writing cleaned dataframe to .csv, to be used in Tableu for visualizations
write.csv(trips_tableu2, file="C:/Users/dania/Desktop/trips_tableu2.csv")
```

## Tableu Dashboard (interactive dashboard can be viewed [here](https://public.tableau.com/views/Cyclistic2019trips/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link))

![Cyclistic Dashboard](https://github.com/DanialArifRauf/Cyclistic-2019-trips/assets/154701480/e1428de9-c2c7-4482-9dbf-99f18932e4d3)

## Conclusion
Once differences between customers (casual riders) and subscribers (members) riders are identified, marketing strategies can be tailored to engage casual riders to encourage them into becoming members. 

Recommendations include:
- Conduct marketing initiatives during the spring and summer seasons at ride hotspots frequented by casual riders.
- Given that casual riders are particularly active on weekends and during the warmer seasons, consider introducing seasonal or weekend-exclusive membership options.
- Seeing that casual riders use bikes for longer durations, consider introducing discounts for longer rides. This may serve as a benefit for casual riders while also enticing existing members to extend their riding durations.
