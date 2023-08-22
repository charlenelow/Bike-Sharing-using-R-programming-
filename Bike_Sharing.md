Bike_Sharing
================
Charlene
2023-08-11

## Import CSV files

2019 Q1 - 2019 Q4

``` r
library(readr)
library(here)
```

    ## here() starts at /Users/charlenelow/Desktop/My Projects/ Bike Sharing

``` r
Q1_2019 <- read.csv(here("data","Divvy_Trips_2019_Q1.csv.gz"))
Q2_2019 <- read.csv(here("data","Divvy_Trips_2019_Q2.csv.gz"))
Q3_2019 <- read.csv(here("data","Divvy_Trips_2019_Q3.csv.gz"))
Q4_2019 <- read.csv(here("data","Divvy_Trips_2019_Q4.csv.gz"))
```

NOTE: Dataset is too large therefore uploading a compressed file \##
Preview data


``` r
colnames(Q1_2019)
```

    ##  [1] "trip_id"           "start_time"        "end_time"         
    ##  [4] "bikeid"            "tripduration"      "from_station_id"  
    ##  [7] "from_station_name" "to_station_id"     "to_station_name"  
    ## [10] "usertype"          "gender"            "birthyear"

``` r
colnames(Q2_2019)
```

    ##  [1] "X01...Rental.Details.Rental.ID"                   
    ##  [2] "X01...Rental.Details.Local.Start.Time"            
    ##  [3] "X01...Rental.Details.Local.End.Time"              
    ##  [4] "X01...Rental.Details.Bike.ID"                     
    ##  [5] "X01...Rental.Details.Duration.In.Seconds.Uncapped"
    ##  [6] "X03...Rental.Start.Station.ID"                    
    ##  [7] "X03...Rental.Start.Station.Name"                  
    ##  [8] "X02...Rental.End.Station.ID"                      
    ##  [9] "X02...Rental.End.Station.Name"                    
    ## [10] "User.Type"                                        
    ## [11] "Member.Gender"                                    
    ## [12] "X05...Member.Details.Member.Birthday.Year"

``` r
colnames(Q3_2019)
```

    ##  [1] "trip_id"           "start_time"        "end_time"         
    ##  [4] "bikeid"            "tripduration"      "from_station_id"  
    ##  [7] "from_station_name" "to_station_id"     "to_station_name"  
    ## [10] "usertype"          "gender"            "birthyear"

``` r
colnames(Q4_2019)
```

    ##  [1] "trip_id"           "start_time"        "end_time"         
    ##  [4] "bikeid"            "tripduration"      "from_station_id"  
    ##  [7] "from_station_name" "to_station_id"     "to_station_name"  
    ## [10] "usertype"          "gender"            "birthyear"

## Standardise column name of Q2_2019

Q2 has different column names compared to the rest, so we have to fix
that.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
Q2_2019 <- Q2_2019 %>% 
  rename(trip_id = X01...Rental.Details.Rental.ID,
         start_time = X01...Rental.Details.Local.Start.Time,
         end_time = X01...Rental.Details.Local.End.Time,
         bikeid = X01...Rental.Details.Bike.ID,
         tripduration = X01...Rental.Details.Duration.In.Seconds.Uncapped,
         from_station_id = X03...Rental.Start.Station.ID,
         from_station_name =X03...Rental.Start.Station.Name,
         to_station_id = X02...Rental.End.Station.ID,
         to_station_name = X02...Rental.End.Station.Name,
         usertype = User.Type,
         gender = Member.Gender,
         birthyear = X05...Member.Details.Member.Birthday.Year)
```

## Combining all the 4 quaters to a full year data

``` r
data_2019 <- rbind(Q1_2019, Q2_2019, Q3_2019, Q4_2019)
```

## Data Cleaning & Transformation

In order to proceed with our analysis, we need to have two additional
information: 1. Ride Duration (end time - start time) 2. Day of week of
bike rental

Getting ride duration for each trip:

``` r
# check if there is any rows with start_time later than end_time:
sum(data_2019$start_time > data_2019$end_time) # 13 rows affected
```

    ## [1] 13

``` r
which(data_2019$start_time > data_2019$end_time) #identify which rows affected
```

    ##  [1] 3498429 3498434 3498436 3498437 3498438 3498440 3498441 3498442 3498443
    ## [10] 3498444 3498446 3498447 3498448

To fix affected rows so start time will be later than end time:

``` r
#change data type to dttm format + create start and end column that satisfy condition start is earlier than end. 
library(dplyr) 
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
data_2019<- data_2019 %>%
  mutate(start = if_else(as_datetime(start_time) > as_datetime(end_time), as_datetime(end_time), as_datetime(start_time)), end = if_else(as_datetime(start_time) > as_datetime(end_time), as_datetime(start_time), as_datetime(end_time))) 

# double check if any start is later than end 
sum(data_2019$start > data_2019$end) 
```

    ## [1] 0

Create a new column to show ride duration:

``` r
data_2019 <- data_2019 %>% 
  mutate(ride_duration = end - start)
```

Create a new column to show day of week of bike rental

``` r
data_2019$day = strftime(data_2019$start_time,'%a') #%a means short form of week name
```

## Descriptive Analysis

Average, min and max ride duration (mins):

``` r
data_2019 %>% 
  group_by(usertype) %>% 
  summarise(mean(ride_duration),max(ride_duration),min(ride_duration))
```

    ## # A tibble: 2 × 4
    ##   usertype   `mean(ride_duration)` `max(ride_duration)` `min(ride_duration)`
    ##   <chr>      <drtn>                <drtn>               <drtn>              
    ## 1 Customer   57.01793 mins         177200.4 mins        1.016667 mins       
    ## 2 Subscriber 14.32788 mins         150943.9 mins        1.016667 mins

Total number of trips made by Customer VS Subscriber:

``` r
data_2019 %>% 
  group_by(usertype) %>%
  summarise(Number_customer= sum(usertype == "Customer"),
            Number_subscriber = sum(usertype == "Subscriber")) 
```

    ## # A tibble: 2 × 3
    ##   usertype   Number_customer Number_subscriber
    ##   <chr>                <int>             <int>
    ## 1 Customer            880637                 0
    ## 2 Subscriber               0           2937367

Number of trips made by Customer VS Subscriber in each day of the week:

``` r
data_2019 %>% 
  group_by(usertype,day) %>% 
  summarise(n_distinct(trip_id)) %>% 
  arrange(day)
```

    ## `summarise()` has grouped output by 'usertype'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 14 × 3
    ## # Groups:   usertype [2]
    ##    usertype   day   `n_distinct(trip_id)`
    ##    <chr>      <chr>                 <int>
    ##  1 Customer   Fri                  121141
    ##  2 Subscriber Fri                  456966
    ##  3 Customer   Mon                  101489
    ##  4 Subscriber Mon                  458780
    ##  5 Customer   Sat                  208056
    ##  6 Subscriber Sat                  287163
    ##  7 Customer   Sun                  170179
    ##  8 Subscriber Sun                  256241
    ##  9 Customer   Thu                  101372
    ## 10 Subscriber Thu                  486915
    ## 11 Customer   Tue                   88655
    ## 12 Subscriber Tue                  497025
    ## 13 Customer   Wed                   89745
    ## 14 Subscriber Wed                  494277

Average ride duration each day of the week Customer VS Subscriber:

``` r
data_2019 %>% 
  group_by(usertype,day) %>%
  summarise(mean(ride_duration)) %>% 
  arrange(day)
```

    ## `summarise()` has grouped output by 'usertype'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 14 × 3
    ## # Groups:   usertype [2]
    ##    usertype   day   `mean(ride_duration)`
    ##    <chr>      <chr> <drtn>               
    ##  1 Customer   Fri   60.17561 mins        
    ##  2 Subscriber Fri   13.89748 mins        
    ##  3 Customer   Mon   54.49989 mins        
    ##  4 Subscriber Mon   14.24928 mins        
    ##  5 Customer   Sat   54.06111 mins        
    ##  6 Subscriber Sat   16.30271 mins        
    ##  7 Customer   Sun   56.18474 mins        
    ##  8 Subscriber Sun   15.40377 mins        
    ##  9 Customer   Thu   59.95112 mins        
    ## 10 Subscriber Thu   13.77979 mins        
    ## 11 Customer   Tue   57.41328 mins        
    ## 12 Subscriber Tue   14.15259 mins        
    ## 13 Customer   Wed   60.33407 mins        
    ## 14 Subscriber Wed   13.80984 mins

## Data Viz

Bike usage trend over the week

``` r
library(ggplot2)
days_of_the_week <- c("Sun", "Mon", "Tue","Wed","Thu","Fri","Sat")

ggplot(data = data_2019) +
  geom_bar(mapping=aes(x= factor (day,days_of_the_week),fill= usertype), width=.5, position = "dodge")+ labs(title="Bike Usage Trend over the week", x= "Day of Week", y= "Number of Rides", subtitle = "2019") + scale_y_continuous(labels = scales::comma)
```

![](https://github.com/charlenelow/Bike-Sharing-using-R-programming-/blob/main/figures/Bike%20Usage%20Trend%20over%20the%20Week.png?raw=true)<!-- --> 

Avg bike rental duration trend over the week

``` r
data_2019 %>% 
  group_by(usertype,day) %>%
  summarise(avg_duration = mean(ride_duration)) %>% 
  arrange(day) %>% 
  ggplot(aes(x= factor(day,days_of_the_week),y=avg_duration,fill= usertype)) +
  geom_bar(width=.5, position = "dodge", stat="identity")+ labs(title="Avg Ride Duration Trend over the week", x= "Day of Week", y= "Avgerage Ride Duration (mins)", subtitle = "2019")
```

    ## `summarise()` has grouped output by 'usertype'. You can override using the
    ## `.groups` argument.
    ## Don't know how to automatically pick scale for object of type <difftime>.
    ## Defaulting to continuous.

![](https://github.com/charlenelow/Bike-Sharing-using-R-programming-/blob/main/figures/Avgerage%20Ride%20Duration%20Trend%20over%20the%20week.png?raw=true)<!-- -->

## Insights 
Based on the Bike usage bar chart, it shows that the usage
trend for Customers is opposite that of Subscribers where we are seeing
higher usage on weekends for Customers and weekdays for Subscribers.

When looking into the average ride duration of each type of bike users,
most Customer rides for at least 50 mins each trip regardless of which
day of the week. While, most Subscriber rides between 10 - 20 mins each
trip regardless which day of the week.

## Recommendation

To increase revenue of the bike rental service, management can look into
ways to attract new customers by having promotional bundle for longer
trips such as: 
1. Full day ride pass on weekends for a fixed price.
2. Half day ride pass for a fixed price.

Additional promotions for weekday bundle to attract more customers to
use the rental service on weekdays: 
1. Ride for 40 mins and get an additional 20 mins for free

Since most subscribers uses the bike rental for short trips (\< 20
mins), they are most likely using it as a mode of transport to get from
one place to another (eg: work to office). Management can come up with
promotional bundles for peak hours such as: lunch hour, morning &
evening rush hour when people are commuting to/from work. This will help
to give more incentives for users to subscribe and be a member of the
bike rental.
