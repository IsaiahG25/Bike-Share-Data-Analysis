#
# Cyclistic Bike Share Data Analysis: Process and Analyze Steps


# Background Summary
#
# This is an R Markdown document for Cyclistic. This will be the Process and Analyze steps of our Data Analysis. I will be conducting the following steps:
# 
# 1. Loading tidyverse, data.table, and ggplot2 packages.
# 2. Import data .csv files, verify their data structure, and combine datasets.
# 3. Calculate ride lengths.
# 4. Clean the dataset (remove ride lengths less than 0, remove nulls, remove test runs, and remove duplicates).
# 5. Understand the dataset with regards to ride type and average ride time. This will give us insight as to why riders may choose to opt out of the membership and go for single use or day passes.
# 6. Create visualizations and save visualizations.

#- - - - - - - - - - - - - - - - - - - - - - 

# Step 1: Load Packages

# The tidyverse, data.table, and ggplot2 packages will be needed in this step. 

# Run the code chunk below to install and load tidyverse, dplyr, data.table, and ggplot2. This may take a few minutes to load up.

install.packages('tidyverse')
library(tidyverse)

install.packages('dplyr')
library(dplyr)

install.packages('data.table')
library(data.table)

install.packages('ggplot2')
library(ggplot2)

#- - - - - - - - - - - - - - - - - - - - - - 

# Step 2: Combine datasets

# We will need upload all of our datasets into R to be able to manipulate further. The following code will enable us to upload our datasets. Running the code below will create variables for all of our datasets.

Apr_20 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Apr2020.csv")
May_20 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\May2020.csv")
Dec_20 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Dec2020.csv")
Jan_21 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Jan2021.csv")
Feb_21 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Feb2021.csv")
Mar_21 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Mar2021.csv")
Nov_21 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Nov2021.csv")
Dec_21 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Dec2021.csv")
Jan_22 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Jan2022.csv")
Feb_22 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Feb2022.csv")
Mar_22 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Mar2022.csv")
Apr_22 <- read.csv("C:\\Users\\USERNAME\\Desktop\\Case Study 1\\Apr2022.csv")

# We can use the following code to verify if there are any different string types.
# This code will be used to verify structure summary is type 'chr'.

str(Apr_20)
str(May_20)
str(Dec_20)
str(Jan_21)
str(Feb_21)
str(Mar_21)
str(Nov_21)
str(Dec_21)
str(Jan_22)
str(Feb_22)
str(Mar_22)
str(Apr_22)

# Some columns are not chr type for start_station_id and end_station_id. We need
# to convert data types in order for tables to be merged. I will mutate() function
# to convert data types to chr from type int.

Apr_20 <- mutate(
  Apr_20,
  start_station_id = as.character(start_station_id),
  end_station_id = as.character(end_station_id)
)

May_20 <- mutate(
  May_20,
  start_station_id = as.character(start_station_id),
  end_station_id = as.character(end_station_id)                            
)

# We will now merge our tables with the code below.

all_ctrips <- bind_rows(Apr_20, May_20, Dec_20, Jan_21, Feb_21, Mar_21, 
  Nov_21, Dec_21, Jan_22, Feb_22, Mar_22, Apr_22)

# We now have one large date frame to work with!

# Before we start to calculate ride lengths, let's inspect our new dataset.

colnames(all_ctrips) # Lets us see column names
nrow(all_ctrips)     # Lets us know the number of rows we have

# Let's create new columns in our table for date, month, day, year, and day of the week

all_ctrips$date <- as.Date(all_ctrips$started_at)
all_ctrips$month <- format(as.Date(all_ctrips$date), "%m")
all_ctrips$day <- format(as.Date(all_ctrips$date), "%d")
all_ctrips$year <- format(as.Date(all_ctrips$date), "%Y")
all_ctrips$day_of_week <- format(as.Date(all_ctrips$date), "%A")

#- - - - - - - - - - - - - - - - - - - - - - 

# Step 3: Let's calculate ride lengths and create a new column for this calculation

all_ctrips$ride_length <- difftime(
  all_ctrips$ended_at, 
  all_ctrips$started_at,
  units = "secs"
) 

# We will convert ride length into a numeric value. This will be useful for analysis later

all_ctrips$ride_length <- as.numeric(
  as.character(all_ctrips$ride_length)
)

#- - - - - - - - - - - - - - - - - - - - - - 

# Step 4: Let's clean up our data

# First, we will remove rows with ride lengths less than 0, as it is impossible to have negative ride time
# We will also create a new data frame called "cleaned_all_ctrips".

cleaned_all_ctrips <- all_ctrips %>%
  filter(!(ride_length < 0))

# Next, we will remove nulls.
# There were a few columns with no start or end station data entered. 
# This could have been a glitch in the system, but is dirty data that will not be helpful to us.

cleaned_all_ctrips <- cleaned_all_ctrips %>%
  filter(
    !(is.na(start_station_name) |
        start_station_name == "")
  ) %>% 
  
  filter(
    !(is.na(end_station_name) |
        end_station_name == "")
  )

# Lastly, we will check for duplicates and only keep 1 instance of a duplicate row

cleaned_all_ctrips %>% distinct()

# Time to check our data again to see how many rows we are left with after cleaning

nrow(cleaned_all_ctrips)

#- - - - - - - - - - - - - - - - - - - - - - 

# Step 5: Let's analyze our data and find meaningful insights

mean(cleaned_all_ctrips$ride_length) # Straight average (total ride length / rides)
# Average ride length is 1143.063 seconds

median(cleaned_all_ctrips$ride_length) # Midpoint number in the ascending array of ride lengths
# Our midpoint ride length is 627 seconds

max(cleaned_all_ctrips$ride_length) # Longest ride
# Our longest ride is 3523202 seconds

min(cleaned_all_ctrips$ride_length) # Shortest ride
# Our shortest ride length is 0 seconds

# Alternatively, the above can be found using the summary function()
summary(cleaned_all_ctrips$ride_length)


# Let's compare the average, max, and shortest ride lengths of casual riders and members

aggregate(cleaned_all_ctrips$ride_length ~ cleaned_all_ctrips$member_casual, FUN = mean)
aggregate(cleaned_all_ctrips$ride_length ~ cleaned_all_ctrips$member_casual, FUN = median)
aggregate(cleaned_all_ctrips$ride_length ~ cleaned_all_ctrips$member_casual, FUN = max)
aggregate(cleaned_all_ctrips$ride_length ~ cleaned_all_ctrips$member_casual, FUN = min)

# Interestingly enough, casual riders have a longer average ride time of 2037 seconds,
# and members have an average ride time of 770 seconds

# Let's compare average ride lengths by day for casual riders and members

aggregate(cleaned_all_ctrips$ride_length ~ cleaned_all_ctrips$member_casual + cleaned_all_ctrips$day_of_week, FUN = mean)

# Because the data is out of order for days, let's sort it by days of the week 

cleaned_all_ctrips$day_of_week <- ordered(cleaned_all_ctrips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Let's do our comparison again

aggregate(cleaned_all_ctrips$ride_length ~ cleaned_all_ctrips$member_casual + cleaned_all_ctrips$day_of_week, FUN = mean)

# Once again, we see that casual riders actually use our bikes longer than members!

# Lastly, let's analyze ridership data by type and weekday

cleaned_all_ctrips %>% 
  mutate(weekday = wday(started_at)) %>%    # Creates weekday field using wday()
  group_by(member_casual, weekday) %>%                    # Groups by user type and weekday
  summarise(number_of_rides = n()						             	# Calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# Calculates the average duration
  arrange(member_casual, weekday)								          # Sorts our data

#- - - - - - - - - - - - - - - - - - - - - - 

# Step 6: Lastly, we will create data visualizations of our findings

# Let's create a visualization for average duration
cleaned_all_ctrips %>% 
  mutate(weekday = wday(started_at)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's also create a visualizaton for number of rides for casual riders and members

cleaned_all_ctrips %>% 
  mutate(weekday = wday(started_at)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# The finding we have is that casual riders use our bikes for longer, while members
# Have more rides than casual riders.

# Let's save our data set!

write.csv(cleaned_all_ctrips, "C:\\Users\\Isaiah\\Desktop\\Case Study 1\\case_study_data.csv",
          col.names = TRUE,
          row.names = FALSE
          )
