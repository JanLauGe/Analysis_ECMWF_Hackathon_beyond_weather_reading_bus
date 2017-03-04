
library(tidyverse)
library(magrittr)
library(lubridate)
library(forcats)
library(caret)
library(stringr)

setwd('C:/Users/Laurens/Dropbox/projects/ecmwf/code/ReadingBus/Data/')
claims <- read_csv('busClaims.csv')
apiKey <- readline('Enter API key:')

# Data Cleaning --------------------------------------------------------

# Get useful route names
routes <- c('2','3','4','5','6','7','9','11','12','13','14','15','16','17',
  '19a','19b','19c','21','22','23','24','25','26','27','28','33','500') %>%
  factor

claims %<>%
  mutate(
    route = Route %>%
      # Take part before slash
      str_split('/') %>%
      lapply(FUN = function(x) {x[[1]]}) %>%
      unlist,
    # Remove trailing spaces
    route = route %>%
      str_split(boundary('word')) %>%
      lapply(FUN = function(x) {x[[1]]}) %>%
      unlist,
    # Remove leading zeros
    route = route %>%
      str_split('^[0]{1,9}') %>%
      lapply(FUN = function(x) {x[[length(x)]]}) %>%
      unlist %>% factor,
    route = route %>% 
      fct_recode(
        `2` = '2A',
        `4` = 'X4',
        `6` = '6A',
        `27` = '29')) %>%
  # Create proper date field
  mutate(dates = `Accident Date` %>% as.Date(format = '%d/%m/%y')) %>%
  # Only claims in 2015 and 2016
  filter(year(dates) > 2014 & year(dates) < 2017) %>%
  # Only valid routes
  filter(route %in% routes) %>%
  # Make bad flag
  mutate(claimed = 1) %>% 
  select(one_of('dates', 'route', 'claimed'))


# Create model data frame ----------------------------------------------
modeldata <- data_frame(dates = seq(ymd('2015-01-01'), ymd('2016-12-31'), by = 'days')) %>%
  # Add routes
  cbind(setNames(lapply(routes, function(x) x=NA), routes)) %>%
  # One row per route per day
  gather(key = route, value = claim, -dates) %>%
  # Set claims to zero
  mutate(claim = 0) %>%
  # Merge claims data
  full_join(claims, by = c('dates', 'route')) %>%
  # Get number of claims per day per route
  group_by(dates, route) %>%
  summarise(claims = sum(claim, na.rm = TRUE) + sum(claimed, na.rm = TRUE)) %>%
  mutate(claims = ifelse(claims > 0, 'yes', 'no') %>% as.factor())

# Plot bad flag over time
modeldata %>%
  filter(claims == 'yes') %>%
  ggplot(aes(x = dates)) +
  geom_histogram(binwidth = 1)


# Run the model ---------------------------------------------------------
set.seed(1717)
kfolds <- createFolds(modeldata$claims, k = 3)

data_train <- modeldata[-kfolds[[3]],]
data_test <- modeldata[kfolds[[3]],]
  

mod_eval <- trainControl(summaryFunction = madSummary)
mod_tune <- expand.grid()



train(
  x = modeldata,
  )


# Train glmnet model
mod_tune <- expand.grid(
  alpha = c(0.1, 0.5, 1), #c(0.1, 0.2, 0.3, 0.5, 0.7, 1), 
  lambda = c(0.0001, 0.001, 0.01)) #c(0.00003, 0.0001, 0.0003, 0.001, 0.003, 0.01, 0.03, 0.1))
mod_eval <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)
mod_train <- train(
  claims ~ 1 + .,
  data = modeldata,
  method = "glmnet",
  family = "binomial",
  metric = "ROC",
  tuneGrid = mod_tune,
  trControl = mod_eval)







