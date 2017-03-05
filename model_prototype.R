
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
  # Make factor binary numeric
  mutate(claims = ifelse(claims > 0, 1, 0)) %>% 
  ungroup

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

#library(Information)
# Get WoE table
infotab <- create_infotables(data_train, y = 'claims')
# Plot WoE
plot_infotables(infotab, 'route')
# Modify factor levels
traindata <- data_train %>%
  mutate(
    claims = ifelse(claims > 0, 'yes', 'no') %>% as.factor,
    val = claims) %>%
  spread(key = route, value = val) %>%
  mutate(
    `2` = infotab$Tables$route %>% {.[.$route == '2','WOE']},
    `3` = infotab$Tables$route %>% {.[.$route == '3','WOE']},
    `4` = infotab$Tables$route %>% {.[.$route == '4','WOE']},
    `5` = infotab$Tables$route %>% {.[.$route == '5','WOE']},
    `6` = infotab$Tables$route %>% {.[.$route == '6','WOE']},
    `7` = infotab$Tables$route %>% {.[.$route == '7','WOE']},
    `9` = infotab$Tables$route %>% {.[.$route == '9','WOE']},
    `11` = infotab$Tables$route %>% {.[.$route == '11','WOE']},
    `12` = infotab$Tables$route %>% {.[.$route == '12','WOE']},
    `13` = infotab$Tables$route %>% {.[.$route == '13','WOE']},
    `14` = infotab$Tables$route %>% {.[.$route == '14','WOE']},
    `15` = infotab$Tables$route %>% {.[.$route == '15','WOE']},
    `16` = infotab$Tables$route %>% {.[.$route == '16','WOE']},
    `17` = infotab$Tables$route %>% {.[.$route == '17','WOE']},
    `19a` = infotab$Tables$route %>% {.[.$route == '19a','WOE']},
    `19b` = infotab$Tables$route %>% {.[.$route == '19b','WOE']},
    `19c` = infotab$Tables$route %>% {.[.$route == '19c','WOE']},
    `21` = infotab$Tables$route %>% {.[.$route == '21','WOE']},
    `22` = infotab$Tables$route %>% {.[.$route == '22','WOE']},
    `23` = infotab$Tables$route %>% {.[.$route == '23','WOE']},
    `24` = infotab$Tables$route %>% {.[.$route == '24','WOE']},
    `25` = infotab$Tables$route %>% {.[.$route == '25','WOE']},
    `26` = infotab$Tables$route %>% {.[.$route == '26','WOE']},
    `27` = infotab$Tables$route %>% {.[.$route == '27','WOE']},
    `28` = infotab$Tables$route %>% {.[.$route == '28','WOE']},
    `33` = infotab$Tables$route %>% {.[.$route == '33','WOE']},
    `500` = infotab$Tables$route %>% {.[.$route == '500','WOE']}) %>%
  gather(key = route, value = routewoe, -dates, -claims) %>%
  select(dates, routewoe, claims)

#detach("package:Information", unload=TRUE)

# Train glmnet model
mod_tune <- expand.grid(
  alpha = c(0, 0.1, 0.5, 1), #c(0.1, 0.2, 0.3, 0.5, 0.7, 1), 
  lambda = c(0.0001, 0.001, 0.01, 0.1, 1)) #c(0.00003, 0.0001, 0.0003, 0.001, 0.003, 0.01, 0.03, 0.1))
mod_eval <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)
mod_train <- caret::train(
  claims ~ 1 + .,
  data = traindata,
  method = "glmnet",
  family = "binomial",
  metric = "ROC",
  tuneGrid = mod_tune,
  trControl = mod_eval)






create_infotables <- function (data = NULL, valid = NULL, y = NULL, bins = 10, trt = NULL, 
          ncore = NULL, parallel = TRUE) 
{
  combine <- function(x, ...) {
    lapply(seq_along(x), function(i) c(x[[i]], lapply(list(...), 
                                                      function(y) y[[i]])))
  }
  if (is.null(data)) {
    stop("Error: No dataset provided")
  }
  crossval <- TRUE
  if (is.null(valid)) {
    crossval <- FALSE
  }
  c <- CheckInputs(data, valid, trt, y)
  data <- c[[1]]
  if (crossval == TRUE) {
    valid <- c[[2]]
  }
  variables <- names(data)[!(names(data) %in% c(trt, y))]
  d_netlift <- 0
  if (is.null(trt) == FALSE) {
    d_netlift <- 1
  }
  i <- NULL
  if (length(variables) == 0) {
    stop("ERROR: no variables left after screening")
  }
  if (parallel == TRUE) {
    if (is.null(ncore)) {
      ncore <- detectCores() - 1
    }
    if (ncore < 1) 
      ncore <- 1
    registerDoParallel(ncore)
    loopResult <- foreach(i = 1:length(variables), .combine = "combine", 
                          .multicombine = TRUE, .init = list(list(), list())) %dopar% 
                          {
                            data$var <- data[[variables[i]]]
                            cuts <- NULL
                            if (crossval == TRUE) {
                              valid$var <- valid[[variables[i]]]
                              if (is.character(data[[variables[i]]]) != is.character(valid[[variables[i]]])) {
                                stop(paste0("ERROR: variable type mismatch for ", 
                                            variables[i], " across validation and training (not a character in both dataframes)"))
                              }
                              if (is.factor(data[[variables[i]]]) != is.factor(valid[[variables[i]]])) {
                                stop(paste0("ERROR: variable type mismatchfor ", 
                                            variables[i], " across validation and training dataframes (not a factor in both dataframes)"))
                              }
                            }
                            if (is.character(data[[variables[i]]]) == FALSE & 
                                is.factor(data[[variables[i]]]) == FALSE) {
                              q <- quantile(data[[variables[i]]], probs = c(1:(bins - 
                                                                                 1)/bins), na.rm = TRUE, type = 3)
                              cuts <- unique(q)
                            }
                            summary_train <- Aggregate(data, variables[i], 
                                                       y, cuts, trt)
                            if (crossval == TRUE) {
                              summary_valid <- Aggregate(valid, variables[i], 
                                                         y, cuts, trt)
                            }
                            if (d_netlift == 0) {
                              woe_train <- WOE(summary_train, variables[i])
                              if (crossval == TRUE) {
                                woe_valid <- WOE(summary_valid, variables[i])
                              }
                              if (crossval == TRUE) {
                                woe_train <- penalty(woe_train, woe_valid, 
                                                     0)
                              }
                              woe_train[, c("IV_weight")] <- NULL
                              strength <- data.frame(variables[i], woe_train[nrow(woe_train), 
                                                                             "IV"])
                              names(strength) <- c("Variable", "IV")
                              if (crossval == TRUE) {
                                strength$PENALTY <- woe_train[nrow(woe_train), 
                                                              "PENALTY"]
                                strength$AdjIV <- strength$IV - strength$PENALTY
                              }
                            }
                            else {
                              nwoe_train <- NWOE(summary_train, variables[i])
                              if (crossval == TRUE) {
                                nwoe_valid <- NWOE(summary_valid, variables[i])
                              }
                              if (crossval == TRUE) {
                                nwoe_train <- penalty(nwoe_train, nwoe_valid, 
                                                      1)
                              }
                              nwoe_train[, c("NIV_weight")] <- NULL
                              strength <- data.frame(variables[i], nwoe_train[nrow(nwoe_train), 
                                                                              "NIV"])
                              names(strength) <- c("Variable", "NIV")
                              if (crossval == TRUE) {
                                strength$PENALTY <- nwoe_train[nrow(nwoe_train), 
                                                               "PENALTY"]
                                strength$AdjNIV <- strength$NIV - strength$PENALTY
                              }
                            }
                            if (d_netlift == 1) {
                              nwoe_train$key <- NULL
                              list(nwoe_train, strength)
                            }
                            else {
                              woe_train$key <- NULL
                              list(woe_train, strength)
                            }
                          }
    tables <- loopResult[[1]]
    stats <- data.frame(rbindlist(loopResult[[2]]))
    stopImplicitCluster()
    rm(loopResult)
    gc()
  }
  else {
    statslist <- list(length = length(variables))
    tables <- list(length = length(variables))
    for (i in 1:length(variables)) {
      data$var <- data[[variables[i]]]
      cuts <- NULL
      if (crossval == TRUE) {
        valid$var <- valid[[variables[i]]]
        if (is.character(data[[variables[i]]]) != is.character(valid[[variables[i]]])) {
          stop(paste0("ERROR: variable type mismatch for ", 
                      variables[i], " across validation and training (not a character in both dataframes)"))
        }
        if (is.factor(data[[variables[i]]]) != is.factor(valid[[variables[i]]])) {
          stop(paste0("ERROR: variable type mismatchfor ", 
                      variables[i], " across validation and training dataframes (not a factor in both dataframes)"))
        }
      }
      if (is.character(data[[variables[i]]]) == FALSE & 
          is.factor(data[[variables[i]]]) == FALSE) {
        q <- quantile(data[[variables[i]]], probs = c(1:(bins - 
                                                           1)/bins), na.rm = TRUE, type = 3)
        cuts <- unique(q)
      }
      summary_train <- Aggregate(data, variables[i], y, 
                                 cuts, trt)
      if (crossval == TRUE) {
        summary_valid <- Aggregate(valid, variables[i], 
                                   y, cuts, trt)
      }
      if (d_netlift == 0) {
        woe_train <- WOE(summary_train, variables[i])
        if (crossval == TRUE) {
          woe_valid <- WOE(summary_valid, variables[i])
        }
        if (crossval == TRUE) {
          woe_train <- penalty(woe_train, woe_valid, 
                               0)
        }
        woe_train[, c("IV_weight")] <- NULL
        strength <- data.frame(variables[i], woe_train[nrow(woe_train), 
                                                       "IV"])
        names(strength) <- c("Variable", "IV")
        if (crossval == TRUE) {
          strength$PENALTY <- woe_train[nrow(woe_train), 
                                        "PENALTY"]
          strength$AdjIV <- strength$IV - strength$PENALTY
        }
      }
      else {
        nwoe_train <- NWOE(summary_train, variables[i])
        if (crossval == TRUE) {
          nwoe_valid <- NWOE(summary_valid, variables[i])
        }
        if (crossval == TRUE) {
          nwoe_train <- penalty(nwoe_train, nwoe_valid, 
                                1)
        }
        nwoe_train[, c("NIV_weight")] <- NULL
        strength <- data.frame(variables[i], nwoe_train[nrow(nwoe_train), 
                                                        "NIV"])
        names(strength) <- c("Variable", "NIV")
        if (crossval == TRUE) {
          strength$PENALTY <- nwoe_train[nrow(nwoe_train), 
                                         "PENALTY"]
          strength$AdjNIV <- strength$NIV - strength$PENALTY
        }
      }
      statslist[[i]] <- strength
      if (d_netlift == 1) {
        nwoe_train$key <- NULL
        tables[[i]] <- nwoe_train
      }
      else {
        woe_train$key <- NULL
        tables[[i]] <- woe_train
      }
    }
    stats <- data.frame(rbindlist(statslist))
  }
  if (crossval == TRUE) {
    if (d_netlift == 0) {
      stats <- stats[order(-stats$AdjIV), ]
    }
    else {
      stats <- stats[order(-stats$AdjNIV), ]
    }
  }
  else {
    if (d_netlift == 0) {
      stats <- stats[order(-stats$IV), ]
    }
    else {
      stats <- stats[order(-stats$NIV), ]
    }
  }
  stats$Variable <- as.character(stats$Variable)
  names(tables) <- variables
  object <- list(Tables = tables, Summary = stats)
  class(object) <- "Information"
  return(object)
}
