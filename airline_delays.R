# Preamble ----------------------------------------------------------------
# Loading packages
library(ggplot2)
library(dplyr)
library(data.table)
library(testthat)
library(caret)
library(corrplot)
library(pROC)
library(xgboost)


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

# # plotting departure delay time vs arrival delay time
# ggplot(data, aes(x = DepDelay, y = ArrDelay)) +
#   geom_point()


# quantiles of ArrDelay
quantile(data$ArrDelay, probs = seq(0,1,0.05))


# capping ArrDelay
data <- data %>% mutate(ArrDelay = ifelse(ArrDelay < -20, -20,
                                   ifelse(ArrDelay > 250, 250, ArrDelay)))

# getting histogram of ArrDelay
ggplot(data, aes(x = ArrDelay)) +
  geom_density(fill = '#c11313') +
  theme_bw() +
  xlab("Arrival delay time (minutes)")

# going to create a binary target 
data <- data %>% mutate(delay_marker = ifelse(ArrDelay > 40, 1, 0))

# bar chart of amount of delay_marker
ggplot(data, aes(x = as.factor(delay_marker))) +
  geom_bar(aes(y = (..count..) / sum(..count..)), fill = '#4287a5') +
  scale_y_continuous(labels = scales::percent) +
  ylab('') +
  xlab('Flight status') +
  # ggtitle("The percentage of all flights being delayed") +
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
 # ggtitle('Percentage of flights delayed by airline relative to the mean number of delays') +
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
  # ggtitle('Percentage of flights delayed by origin airport relative to the mean number of delays') +
  xlab('Airport') +
  ylab('') +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept  = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# what are biggest factors that contribute to a delay
# taking rows that have a delay
data_delay <- data %>% filter(delay_marker == 1) %>%
  
  select(CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>%
  
  mutate(CarrierDelay = ifelse(CarrierDelay > 0, 1, 0),
         WeatherDelay = ifelse(WeatherDelay > 0, 1, 0),
         NASDelay = ifelse(NASDelay > 0, 1, 0),
         SecurityDelay = ifelse(SecurityDelay > 0, 1, 0),
         LateAircraftDelay = ifelse(LateAircraftDelay > 0, 1, 0)) %>%
  
  gather(reason, delay,CarrierDelay:LateAircraftDelay) %>% 
  
  group_by(reason) %>%
  
  summarise(total = sum(delay)) %>%
  
  mutate(pct_total = total / sum(total))

# plotting
ggplot(data_delay, aes(x = reorder(reason, pct_total), y = pct_total, fill = reason)) +
  geom_bar(stat = 'identity') +
  # ggtitle('Percentage of flights delayed by origin airport relative to the mean number of delays') +
  xlab('Delay reason') +
  ylab('') +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  #geom_hline(yintercept  = 0) +
  theme_bw() +
  scale_x_discrete(labels = c('Security', 'Weather', "Carrier", "National air system", "Late aircraft"))
  #theme(plot.title = element_text(hjust = 0.5))

# Preparing data for clustering --------------------------------------------
# finding columns with only one unique value
unique_cols <- which(sapply(data, function(x){length(unique(x))}) == 1)

# removing them
data <- data %>% select(-one_of(names(unique_cols)))

# removing other columns
data <- data %>% select(-V1, -DayofMonth, -DepTime, -FlightNum, -ActualElapsedTime, -DepDelay, -(TaxiIn:LateAircraftDelay), -ArrDelay, -AirTime, -ArrTime, - TailNum)

# feature engineering
# Time of departure would be an obvious reason why flights are delayed, i.e later flights are more likely to be delayed due to knock on from morning
# The exact departure time is perhaps too granular. Instread, we will take the departure time

# taking only the hour 
data <- data %>% mutate( CRSDepTime = trunc(CRSDepTime / 100),
                           CRSArrTime = trunc(CRSArrTime / 100))

# Clustering --------------------------------------------------------------
# #### K-means ------------------------------------------------------------

# taking continous columns
data_contin <- data %>% select(CRSDepTime, CRSArrTime, CRSElapsedTime, Distance)

# scaling the data
data_contin <- scale(data_contin)

# Initial Kmeans
# It would be nice if we could separate the data into 2 clusters, delayed and not delayed
set.seed(876)
k_means_2 <- kmeans(x =  data_contin, centers = 2, nstart = 20)

# comparing cluster value to delayed marker
prop.table(table(data$delay_marker, k_means_2$cluster), margin = 2) # two clusters dont predict if flight is delayed v well

# Checking other values of k
# Initialize total within sum of squares error
wss <- 0

# For 1 to 5 cluster centers
for (i in 1:5) {
  k_means <- kmeans(data_contin, centers = i, nstart = 20)

  wss[i] <- k_means$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:5, wss, type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

# An elbow appears at 3 clusters which is an interesting result
set.seed(6364)
k_means_3 <- kmeans(data_contin, centers = 3 , nstart = 20)
table(data$delay_marker, k_means_3$cluster)
prop.table(table(data$delay_marker, k_means_3$cluster), margin = 2) # cluster 3 (in this case) has a higher proportion of delayed, this could be useful for our logistic regression model
#
# visualisting results
# since more than 2 variables will use PCA
# Perform scaled PCA: pr.out
pca_data_contin <- prcomp(data_contin, scale = F)

# Inspect model output
summary(pca_data_contin)

# visualising the pca results
# Variability of each principal component
pr_var <- pca_data_contin$sdev ^ 2

# Variance explained by each principal component: pve
pve <- pr_var / sum(pr_var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Plrincipal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

pred <- as.data.frame(predict(pca_data_contin, newdata=data_contin))

component_df <- data.frame(pca1 = pred$PC1,
                           pca2 = pred$PC2,
                           cluster = k_means_3$cluster)

# taking sample for scatterplot
# creating index
component_df$index <- 1:nrow(component_df)

# setting seed and taking sample from index for train
set.seed(97843)
index_component <- sample(component_df$index, 1000)

component_df_slim <- component_df %>% filter(index %in% index_component)

ggplot(component_df_slim,aes(x=pca1,y=pca2, color = as.factor(cluster))) + 
  geom_point()

######################
# try pca then clustering
k_means_pca <- kmeans(component_df %>% select(pca1,pca2), centers = 2, nstart = 20)
prop.table(table(data$delay_marker, k_means_pca$cluster), margin = 2) # achieved same results with two clusters than above

# appending pca clustering to component
component_df$cluster_pca <- k_means_pca$cluster
component_df$delay_marker <- data$delay_marker

set.seed(97843)
index_component <- sample(component_df$index, 1000)

component_df_slim <- component_df %>% filter(index %in% index_component)

ggplot(component_df_slim,aes(x=pca1,y=pca2, color = as.factor(cluster_pca), shape = as.factor(delay_marker))) + 
  geom_point()

# slimming down component_df for only columns we want for model
component_df <- component_df %>% select(pca1, pca2, cluster_pca)

# append cols to data
data <- bind_cols(data, component_df)

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


# Logistic Regression model -----------------------------------------------

# reducing number of level in Origin and Destination variables
# origin
airports_origin_model <- train %>%
  group_by(Origin) %>%
  summarise(
            n_airports = n(),
            no_delayed = sum(delay_marker),
            amt_delay = no_delayed / n_airports) %>%
  
  arrange(amt_delay) %>% 
  
  mutate(
          Origin_alter =  ifelse(amt_delay <= 0.28, 1,
                          ifelse(amt_delay > 0.28 & amt_delay <=  0.35, 2,
                          ifelse(amt_delay > 0.35 & amt_delay <= 0.4, 3, 
                          ifelse(amt_delay > 0.4 & amt_delay <= 0.45, 4, 5))))) %>%
  
  select(Origin, Origin_alter)

# joining back
train <- inner_join(train, airports_origin_model, by = 'Origin')

# Dest
airports_dest_model <- train %>%
  group_by(Dest) %>%
  summarise(
    n_airports = n(),
    no_delayed = sum(delay_marker),
    amt_delay = no_delayed / n_airports) %>%
  
  arrange(amt_delay) %>% 
  
  mutate(
    Dest_alter = ifelse(amt_delay <= 0.28, 1,
                 ifelse(amt_delay > 0.28 & amt_delay <=  0.32, 2,
                 ifelse(amt_delay > 0.32 & amt_delay <= 0.36, 3, 
                 ifelse(amt_delay > 0.36 & amt_delay <= 0.40, 4, 5))))) %>%
  
  select(Dest, Dest_alter)

# joining back
train <- inner_join(train, airports_dest_model, by = 'Dest')

# Airline
airline_model <- train %>%
  group_by(UniqueCarrier) %>%
  summarise(
    n_airports = n(),
    no_delayed = sum(delay_marker),
    amt_delay = no_delayed / n_airports) %>%
  
  arrange(amt_delay) %>% 
  
  mutate(
    Carrier_alter = ifelse(amt_delay <= 0.3, 1,
                    ifelse(amt_delay > 0.3 & amt_delay <=  0.35, 2,
                    ifelse(amt_delay > 0.35 & amt_delay <= 0.395, 3, 4))))  %>%
  
  select(UniqueCarrier, Carrier_alter)

# joining back
train <- inner_join(train, airline_model, by = 'UniqueCarrier')

# Altering Day and Month variables
train <- train %>% mutate(Month_alter = ifelse(Month == 10 | Month == 9 | Month == 5, 1,
                                        ifelse(Month == 4 | Month == 8 | Month == 11, 2,
                                        ifelse(Month == 3 | Month == 1 | Month == 7, 3, 4))),
                          
                          Day_alter = ifelse(DayOfWeek == 3 | DayOfWeek == 6, 1,
                                      ifelse(DayOfWeek == 4 | DayOfWeek == 1, 2, 3)),
                          
                          cluster_pca = cluster_pca - 1
  
)

# Model formula
logistic_form <- as.formula(as.factor(delay_marker) ~ as.factor(Month_alter)+
                                           as.factor(Day_alter) +
                              
                                           pca1 +
                                           pca2 +
                                           as.factor(Carrier_alter) + 
                                           #cluster_pca +
                                           as.factor(Origin_alter) +
                                           as.factor(Dest_alter),
                                           as.factor(y_pred))

train_control<- trainControl(method="cv", number=10, allowParallel = TRUE)

model <-  train(logistic_form,
                      data      = train,
                      method    = "glm",
                      family    = binomial,
                      trControl = train_control)

# print cv scores
summary(model)
model$results




# creating correlation plot
corr_data <- cor(train %>% select(Month_alter ,
                                Day_alter ,
                                pca1 ,
                                pca2 ,
                                Carrier_alter , 
                              #  cluster_pca ,
                                Origin_alter ,
                                Dest_alter))

corrplot(corr_data, method = 'number')

# predict on train
pred <- predict(model, newdata = train, type = 'prob')
train$pred <- pred[,2]
train$pred_bin <- ifelse(train$pred > 0.3425,1,0)

# confusion matrix
train_confusion <- confusionMatrix(data = train$pred_bin, reference = train$delay_marker)

# predicting on test data
# joining back aggregated cat columns
test <- inner_join(test, airports_origin_model, by = 'Origin')
test <- inner_join(test, airports_dest_model, by = 'Dest')
test <- inner_join(test, airline_model, by = 'UniqueCarrier')

# Altering Day and Month variables
test <- test %>% mutate(Month_alter = ifelse(Month == 10 | Month == 9 | Month == 5, 1,
                                               ifelse(Month == 4 | Month == 8 | Month == 11, 2,
                                                      ifelse(Month == 3 | Month == 1 | Month == 7, 3, 4))),
                          
                          Day_alter = ifelse(DayOfWeek == 3 | DayOfWeek == 6, 1,
                                             ifelse(DayOfWeek == 4 | DayOfWeek == 1, 2, 3)),
                          
                          cluster_pca = cluster_pca - 1
                          
)

# Predicting on test data
pred_test <- predict(model, newdata = test, type = 'prob')
test$pred <- pred_test[,2]

# setting cutoff
test$pred_bin <- ifelse(test$pred > 0.3425,1,0)

# confustin matrix for test
test_confusion <- confusionMatrix(data = test$pred_bin, reference = test$delay_marker)

test_confusion$byClass

# sampling test
set.seed(999)
index_test <- sample(test$index, 10000)
test_sample <- test %>% filter(index %in% index_test)

# ROC curve
auc <- auc(test_sample$delay_marker, test_sample$pred)
plot(roc(test_sample$delay_marker, test_sample$pred))



