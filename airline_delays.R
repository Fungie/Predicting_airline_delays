# Preamble ----------------------------------------------------------------
# Loading packages
library(ggplot2)
library(dplyr)
library(data.table)
library(testthat)
library(stringr)
library(lubridate)
library(lazyeval)

# Reading in data
setwd("/Users/aurenferguson/Documents/Predicting_airline_delays/")
data <- fread("data/DelayedFlights.csv")


# High level EDA ----------------------------------------------------------
# High level overview of data
summary(data)

# Getting number of NA's by columns
sapply(data, function(x){sum(is.na(x))})

# Creating a delay marker
# ArrDelay column will be used to create delayed marker
# It's used instread of DepDelay since flights can make up for departure delays and still arrive on time.
# Although, they are probably highly correlated

# dropping rows where ArrDelay is missing
data <- data %>% filter(!is.na(ArrDelay))

# Correlation between arrival and departure delay times
cor(data$ArrDelay, data$DepDelay)

# plotting departure delay time vs arrival delay time
ggplot(data, aes(x = DepDelay, y = ArrDelay)) +
  geom_point()


# quantiles of ArrDelay
quantile(data$ArrDelay, probs = seq(0,1,0.05))

# capping ArrDelay
data <- data %>% mutate(ArrDelay = ifelse(ArrDelay < -20, -20,
                                   ifelse(ArrDelay > 250, 250, ArrDelay)))

# getting histogram of ArrDelay
ggplot(data, aes(x = ArrDelay)) +
  geom_histogram()

# going to create a binary target 
data <- data %>% mutate(delay_marker = ifelse(ArrDelay > 40, 1, 0))

# bar chart of amount of delay_marker
ggplot(data, aes(x = as.factor(delay_marker))) +
  geom_bar(aes(y = (..count..) / sum(..count..)), fill = '#4287a5') +
  scale_y_continuous(labels = scales::percent) +
  ylab('') +
  xlab('Flight status') +
  ggtitle("The percentage of all flights being delayed") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c('Not delayed', 'Delayed'))


# More indepth EDA --------------------------------------------------------
# what are the best and worst airlines in terms of punctuality
airlines <- data %>% group_by(UniqueCarrier) %>%
  summarise(n_flights = n(),
            no_delayed = sum(delay_marker),
            amt_delay = no_delayed / n_flights)

# mean amount delayed
mean_delay <- mean(airlines$amt_delay)

airlines <- airlines %>% mutate(mean_diff = amt_delay - mean_delay) %>%
  arrange(mean_diff)

ggplot(airlines, aes(x = reorder(UniqueCarrier, mean_diff), y = mean_diff, fill = UniqueCarrier)) +
  geom_bar(stat = 'identity') +
  ggtitle('Percentage of flights delayed by airline relative to the mean number of delays') +
  xlab('Airline') +
  ylab('') +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept  = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Best and worst departing  large airports
airports <- data %>%
  group_by(Origin) %>%
  summarise(n_airports = n(),
            no_delayed = sum(delay_marker),
            amt_delay = no_delayed / n_airports) %>%
  arrange(desc(n_airports)) %>%
  head(20)

mean_airport_delay <- mean(airports$amt_delay)

airports <- airports %>% mutate(mean_diff = amt_delay - mean_airport_delay) 

ggplot(airports, aes(x = reorder(Origin, mean_diff), y = mean_diff, fill = Origin)) +
  geom_bar(stat = 'identity') +
  ggtitle('Percentage of flights delayed by origin airport relative to the mean number of delays') +
  xlab('Airport') +
  ylab('') +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept  = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# what are biggest factors that contribute to a delay
# to do

# Splitting the data into train and test sets -----------------------------
# creating index
data$index <- 1:nrow(data)

# setting seed and taking sample from index for train
set.seed(1234)
index_train <- sample(data$index, 0.8 * nrow(data))

# creating train and test sets
train <- data %>% filter(index %in% index_train)
test <- data %>% filter(!(index %in% index_train))

# unit test
testthat::test_that("Length of train and test sets equal the base dataset",{
  
  testthat::expect_equal(nrow(data), nrow(train) + nrow(test))
})


# Preparing data for modelling --------------------------------------------
# finding columns with only one unique value
unique_cols <- which(sapply(train, function(x){length(unique(x))}) == 1)

# removing them
train <- train %>% select(-one_of(names(unique_cols)))

# removing other columns
train <- train %>% select(-V1, -DayofMonth, -DepTime, -FlightNum, -ActualElapsedTime, -DepDelay, -(TaxiIn:LateAircraftDelay), -ArrDelay, -ArrTime)

# feature engineering
# Time of departure would be an obvious reason why flights are delayed, i.e later flights are more likely to be delayed due to knock on from morning
# The exact departure time is perhaps too granular. Instread, we will take the departure time

# dropping rows with bad departure date column
train <- train %>% filter(nchar(CRSDepTime) >= 3 & nchar(CRSDepTime) <= 4)

# taking only the hour 
train <- train %>% mutate( CRSDepTime = str_sub(CRSDepTime, 1, nchar(CRSDepTime) - 2),
                           CRSDepTime = as.integer((CRSDepTime)))

train <- train %>% filter(nchar(CRSArrTime) >= 3 & nchar(CRSArrTime) <= 4)
train <- train %>% mutate( CRSArrTime = str_sub(CRSArrTime, 1, nchar(CRSArrTime) - 2),
                           CRSArrTime = as.integer((CRSArrTime)))

# creating scores for factor variables in order for them to be allowed 
# Month
month_change <- train %>% group_by(Month) %>%
  summarise(n_flights = n(),
            no_delayed = sum(delay_marker),
            amt_delay = no_delayed / n_flights)

# mean amount delayed
mean_month <- mean(month_change$amt_delay)

# calculating score
month_change <- month_change %>% mutate(Month_score = round(amt_delay - mean_delay,4)) %>%
  select(Month, Month_score)

# joining back to train
train <- inner_join(train, month_change, on = 'Month')

##########################
# Day of week
day_change <- train %>% group_by(DayOfWeek) %>%
  summarise(n_flights = n(),
            no_delayed = sum(delay_marker),
            amt_delay = no_delayed / n_flights)

# mean amount delayed
mean_day <- mean(day_change$amt_delay)

# calculating score
day_change <- day_change %>% mutate(Day_score = round(amt_delay - mean_delay,4)) %>%
  select(DayOfWeek, Day_score)

# joining back to train
train <- inner_join(train, day_change, on = 'DayOfWeek')

######################
# carrier
carrier_change <- train %>% group_by(UniqueCarrier) %>%
  summarise(n_flights = n(),
            no_delayed = sum(delay_marker),
            amt_delay = no_delayed / n_flights)

# mean amount delayed
mean_carrier <- mean(carrier_change$amt_delay)

# calculating score
carrier_change <- carrier_change %>% mutate(carrier_score = round(amt_delay - mean_delay,4)) %>%
  select(UniqueCarrier, carrier_score)

# joining back to train
train <- inner_join(train, carrier_change, on = 'UniqueCarrier')
#######################
# Tail number
tail_change <- train %>% group_by(TailNum) %>%
  summarise(n_flights = n(),
            no_delayed = sum(delay_marker),
            amt_delay = no_delayed / n_flights)

# mean amount delayed
mean_tail <- mean(tail_change$amt_delay)

# calculating score
tail_change <- tail_change %>% mutate(tail_score = round(amt_delay - mean_delay,4)) %>%
  select(TailNum, tail_score)

# joining back to train
train <- inner_join(train, tail_change, on = 'TailNum')
########################
# Origin
origin_change <- train %>% group_by(Origin) %>%
  summarise(n_flights = n(),
            no_delayed = sum(delay_marker),
            amt_delay = no_delayed / n_flights)

# mean amount delayed
mean_origin <- mean(origin_change$amt_delay)

# calculating score
origin_change <- origin_change %>% mutate(origin_score = round(amt_delay - mean_delay,4)) %>%
  select(Origin, origin_score)

# joining back to train
train <- inner_join(train, origin_change, on = 'TailNum')
########################
# Destination
dest_change <- train %>% group_by(Dest) %>%
  summarise(n_flights = n(),
            no_delayed = sum(delay_marker),
            amt_delay = no_delayed / n_flights)

# mean amount delayed
mean_dest <- mean(dest_change$amt_delay)

# calculating score
dest_change <- dest_change %>% mutate(dest_score = round(amt_delay - mean_delay,4)) %>%
  select(Dest, dest_score)

# joining back to train
train <- inner_join(train, dest_change, on = 'Dest')
#########################


# dummy glm
a <- glm(delay_marker ~ Month_score + Day_score + carrier_score + tail_score + origin_score + dest_score, family = 'binomial', data = train)
summary(a)
