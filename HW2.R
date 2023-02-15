

# load libraries
library(tidyverse)
library(lubridate)

#Naming my csv file data and viewing the first 20 rows
data = read_csv("restaurant_inspections.csv")
View(data[1:20,])

#Creating a histogram

ggplot(data=data, aes(SCORE)) +
  geom_histogram()
#It doesn't look very good, but I can see that the scires range from around 75 to a little over 100, with most scores nearing 100.

#Older versus newer restaurants. 
#Let's create a year_opened variable and a year variable to make it simpler, I can subtract these two numbers to get the number of years opened. 

#Creating Year Opened variable
data$RESTAURANTOPENDATE = ymd_hms(data$RESTAURANTOPENDATE)
data$Year_Opened = year(data$RESTAURANTOPENDATE)

#Creating Year variable
data = rename(data, Year = "DATE_")
data$Year = ymd_hms(data$Year)
data$Year = year(data$Year)

#Creating years opened variable
data$Years_Opened = data$Year - data$Year_Opened
View(data[1:20,])
#I can see I am missing a lot of this data.

#grouping by years opened to see if average score was different
date_by_years_opened = group_by(data, Years_Opened) %>%
  summarize(
   Average_score=mean(SCORE))
#Viewing the first 20 rows, initially I don't see much of a difference
View(date_by_years_opened[1:20,])

#I created a visualization to view the differences but don't see a trend here.
ggplot(date_by_years_opened, aes(x = Years_Opened, y = Average_score)) +
  geom_col()

#Looking at each city
#First making sure they are all the same
#and looking at what cities are in the column
#using str to upper to make them all uppercase
data$CITY = str_to_upper(data$CITY)
unique(data$CITY)
#Recoding so they are all the same
#Confused why NORTH CAROLINA IS a city name, but leaving as is
data$CITY = recode(data$CITY, "HOLLY SPRINGS"="HOLLY SPRING", "FUQUAY VARINA" = "FUQUAY_VARINA", "RTP" = "RESEARCH TRIANGLE PARK")
#Checking cities again to make sure it worked.
unique(data$CITY)
#Looks good now

#Now I can group by city and see how the average scores varied

By_City = group_by(data, CITY) %>%
  summarize(
    Average_score=mean(SCORE))

# I will visualize these scores with a chart

ggplot(By_City, aes(x = CITY, y = Average_score)) +
  geom_col()
#Do not see huge differences in average scores

#Do inspector scores vary by inspector, let's check
By_Inspector = group_by(data, INSPECTOR) %>%
  summarize(
    Average_score=mean(SCORE))

#Visualizing
ggplot(By_Inspector, aes(x = INSPECTOR, y = Average_score)) +
  geom_col()


