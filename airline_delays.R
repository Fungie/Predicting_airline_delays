# Loading packages
library(ggplot2)
library(dplyr)
library(data.table)

# Reading in data
setwd("/Users/aurenferguson/Documents/Predicting_airline_delays/")
data <- fread("data/DelayedFlights.csv")

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

