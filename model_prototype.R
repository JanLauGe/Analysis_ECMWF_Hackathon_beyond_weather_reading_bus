
library(tidyverse)
library(magrittr)
library(lubridate)
library(forcats)
library(caret)
library(stringr)

claims <- read_csv('busClaims.csv')

# Data Cleaning --------------------------------------------------------

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
  mutate(dates = `Accident Date` %>% as.Date(format = '%d/%m/%y'))

# Get useful route names
routes <- c('2','3','4','5','6','7','9','11','12','13','14','15','16','17',
  '19a','19b','19c','21','22','23','24','25','26','27','28','33','500') %>%
  factor

claims %<>% 
  # Only claims in 2015 and 2016
  filter(`Year of Account` > 2014) %>%
  # Only valid routes
  filter(route %in% routes) %>%
  # Make bad flag
  mutate(claimed = 1)

# Create model data frame ----------------------------------------------
modeldata <- data_frame(dates = seq(ymd('2015-01-01'), ymd('2016-12-31'), by = 'days')) %>%
  # Add routes
  cbind(setNames(lapply(routes, function(x) x=NA), routes)) %>%
  # One row per route per day
  gather(key = route, value = claim, -dates) %>%
  # Set claims to zero
  mutate(claim = 0) %>%
  # Merge claims data
  full_join(claims %>% select(dates, route, claimed), by = c('dates', 'route')) %>%
  # Get number of claims per day per route
  group_by(dates, route) %>%
  summarise(claims = sum(claim, na.rm = TRUE) + sum(claimed, na.rm = TRUE)) %>%
  mutate(claims)




