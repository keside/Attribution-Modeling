
# Name: Akano, KEside
# Attribution Modeling

# How to attribute contribution to an TV or online advertisement in a company


#Libraries

if (!require("pacman")) install.packages("pacman")

pacman::p_load(ggplot2,dplyr, 
               lubridate,timeSeries, )


data <- read.csv('Customerattributiondata (1).csv')


# Removing SEA-NON-BRAND FROM CUSTOMERID AND ALSO CORRECTING MISPELLED NAMES LIKE ADWORDS/

data <- subset(data, CUSTOMERID!= "SEA_NON-BRAND\"")

data1$MARKETINGCHANNEL <- sub("Adwords/", "Adwords", data1$MARKETINGCHANNEL)

data1$MARKETINGCHANNEL <- gsub("SEA_NON-BRAND\"", "SEA_NON-BRAND", data1$MARKETINGCHANNEL)

# Replacing all NA to zero
data1$REVENUE[which(is.na(dstv1$REVENUE))] <-0


## Preprocessing
data1_cookies <- data1 %>% tidyr::gather(cookies, count_cookies, `CUSTOMERID`)

# Removing unwanted colume created 
data1_cookies$cookies <- NULL

# Timestamp to POSIXct format
data1$time_posix <- as.POSIXct(data1$TIMESTAMP_TOUCHPOINT, format = "%m/%d/%Y %H:%M")

#Group hourly
data1 %>% 
  group_by(hour = hour(time_posix),
           day = day(time_posix),
           month = month(time_posix),
           channels = (MARKETINGCHANNEL),
           year = year(time_posix)) %>%
  summarise(count = n(), total_revenue = sum(REVENUE, na.rm = TRUE)) %>% 
  arrange(year, month, day, hour) %>% 
  tibble::rownames_to_column() -> data_time


# Group by day,hour and min
data1_cookies %>% group_by(day= day(time_posix), 
                           hour=hour(time_posix),
                           minute = minute(time_posix),
                           channels=(MARKETINGCHANNEL)) %>% arrange(hour) %>%  
  summarise(count=n(),revenue= sum(REVENUE)) -> data_count_cookies


# Vis using Hour Vs Revenue.
# I noticed increase only in 13,18,20 & 22 indicating channel SEO BRAND(13,20,22)
# and DIRECT NON BRAND(18).

ggplot(data_count_cookies, aes(hour, revenue, fill= channels)) + 
  geom_bar(position = "dodge", stat="identity")


# Vis using Day Vs Revenue
# I noticed increase in day 6, 9, 21, 22, 23, and 28 indicating 
# SEO BRAND(6-23) and DIRECT NON BRAND(28)

ggplot(data_count_cookies, aes(day, revenue, fill= channels)) + 
  geom_bar(position = "dodge", stat="identity")


# Vis using Minute Vs Revenue
ggplot(data_count_cookies, aes(minute, revenue, fill= channels)) + 
  geom_bar(position = "dodge", stat="identity")


# Vis using Count Vs Revenue
# I noticed increase in count2 (showing revenue high in DIRECT NON BRAND & SEO BRAND while
# in count4 (only SEO BRAND channel showed with max revenue))

ggplot(data_count_cookies, aes(count, revenue, fill= channels)) + 
  geom_bar(position = "dodge", stat="identity")



# Investigating the data by using granularity ie more clearity is need by specificity
# I decided to choose a day and hour to get more insight.

# Day 30 hour 21
granular_day30 <-dplyr::filter(data_count_cookies, day ==30, hour==21) 

# Day 30 hour 20
granular_Hday30 <-dplyr::filter(data_count_cookies, day ==30, hour==20)

## Creating a time series for Day 30 hour 21
# Here no time was constant indicating no TV ad was watched
 ts_day30_h21 <- ts(granular_day30$revenue, frequency=1)
 
plot.ts(ts_day30_h21)

# # Creating a time series for Day 30 hour 20
# Here in 10, 15,25,30 minutes, customers watch TV advert as showned in the chart
ts_day30_h20 <- ts(granular_Hday30$revenue, frequency=1)

plot.ts(ts_day30_h20)



## Let look at another day to see grandularity

# Day 31 hour 18

granular_day31 <-dplyr::filter(data_count_cookies, day ==31, hour==18)


# Day 31 hour 19
granular_Hday31 <-dplyr::filter(data_count_cookies, day ==31, hour==19)



## Time Series for day 30 with hour 18 & 19

# Here at 4min and 12mins customers watched TV ad
ts_H18_day31 <- ts(granular_day31$revenue, frequency = 1)

# Vis
plot.ts(ts_H18_day31)

# Day 31 hour 19
# Here no customer watched TV ad as showned in the plot

ts_H19_day31 <- ts(granular_Hday31$revenue, frequency = 1)

# Vis
plot.ts(ts_H19_day31)



## Now i want to check if the number of counts in channel actually determines revenue
# because in reality and can generate traffic but no revenue. I will use counts as a point
# of determinant in my time series.

# Day 30 hour 20 and Day 31 Hour 18

# Time Series.
# Customer watch TV ad in 15,25,30 minutes in a range of 40mins

ts_count30H20 <- ts(granular_Hday30$count, frequency=1)

plot.ts(ts_count30H20)



ts_count31H18 <- ts(granular_day31$count, frequency = 1)

# Vis
plot.ts(ts_count31H18)

#----

# Increase in 5mins, 25mins and 40mins
ts_day30_h21_co <- ts(granular_day30$count, freq=1)

plot.ts(ts_day30_h21_co) # 5,23mins, 39mins

ts_day30_h21_co_re <- ts(granular_day30$count, start = granular_day30$revenue, freq= 1)

plot.ts(ts_day30_h21_co_re)

# # Creating a time series for Day 30 hour 20 using count
# increase 10mins 15mins, 28min and 30mins
ts_day30_h20_co <- ts(granular_Hday30$count, frequency=1)

plot.ts(ts_day30_h20_co)


