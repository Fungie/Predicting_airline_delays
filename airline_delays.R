# Preamble ----------------------------------------------------------------
# Loading packages
library(ggplot2)
library(dplyr)
library(data.table)


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


