library(tidyverse)
library(jsonlite)

setwd('C:/Users/Laurens/Dropbox/projects/ecmwf/code/ReadingBus/')

# Getting locations from claims data
claims <- read_csv('busClaims.csv')

claimsloc <- claims %>%
  {.$`Place of Event`}

# Frequency of accidents by location
claimsloc %>%
  table %>%
  sort(decreasing = TRUE)

# Function to find location via google maps api
geocodeAdddress <- function(location) {
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, location, "&sensor=false", sep = ""))
  x <- fromJSON(url)
  if (x$status == "OK") {
    out <- c(x$results[1,]$geometry$location$lng,
             x$results[1,]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

geocodeAdddress("Time Square, New York City")
geocodeAdddress(paste0(claimsloc[1], ', Reading'))
