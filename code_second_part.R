library(RPostgreSQL)
library(readxl)       # excel files reader
library(foreign)      # necessary to import datasets in formats like .dta (STATA) etc.
library(dplyr)        # ! many useful functions for quick data manipulation
library(tidyr)        # designed specifically for data tidying
library(ggplot2)      # (probably) most common package to produce graphs
library(stargazer)    # best way to export tables in LaTex format
library(haven)        # good alternative to "foreign", to read STATA/Sas/Spss data
library(dummies)      # it helps create dummy variables quickly
library(Hmisc)        # some useful functions for e.g. renaming variables
library(lmtest)       # great to test linear regression models
library(sandwich)     # produces robust standard errors
library(doBy)         # good to produce statistics by group
library(multiwayvcov) # estimators for clustered standard errors
library(car)          # nice estimation and post-estimation commands
library(stringr)      # ! great for character variables manipulation
library(datasets)     # package containing many datasets
library(purrr)        # enhanced features to deal with vectors and functions
library(vtable)       # nice package for producing tables
library(data.table)
library(lubridate)
library(plyr)

source("/Users/monicamirelabutnariuc/Downloads/UZH/FS_20/Master TS/R/first_part/conf.R")

recipes_create <- data.table(dbReadTable(db, "recipes_create"))
recipes_rate <- data.table(dbReadTable(db, "recipes_rate"))
recipes <- data.table(dbReadTable(db, "recipes"))
users <- data.table(dbReadTable(db, "users"))
recipe_comments <- data.table(dbReadTable(db, "recipe_comments"))
recipe_ingredients_mapping <- data.table(dbReadTable(db, "recipe_ingredients_mapping"))
ingredients_emissions <- data.table(dbReadTable(db, "ingredients_emissions"))

# preparation for population
temporary_table_users <- data.table(created_at = users$created_at)
temporary_table_users <- cbind(my_user = 1, temporary_table_users)
temporary_table_users$new_date <- as.Date(temporary_table_users$created_at, format="%Y-%m-%d")

tmp <- list()
tmp$y <- format(temporary_table_users$new_date, format="%Y")
tmp$w <- format(temporary_table_users$new_date, format="%W")
tmp$y[tmp$w=="00"] <- as.character(as.numeric(tmp$y[tmp$w=="00"]) - 1)
tmp$w[tmp$w=="00"] <- "52"
temporary_table_users$new_date <- paste(tmp$y, tmp$w, sep = "-")

temporary_table_users2 <- ddply(temporary_table_users, .(new_date), summarize, pop=sum(my_user))
temporary_table_users2$cumsum_users <- cumsum(temporary_table_users2$pop)
temporary_table_users2$period <- seq(from = 1, to = nrow(temporary_table_users2))
### WORKS BECAUSE the number of periods is the same as for the oldest recipe == 2606
### WORKS BECAUSE the final population is equal to the nr of users == 329209


# select only the recipes we need and add column of 1s (reference for adopters, rate and comments)
recipes_create <- recipes_create[recipes_create$recipe_id %in% recipes$recipe_id,]
recipes_create <- cbind(starting_week = 1, recipes_create)
recipes_create <- cbind(end = max(recipes_rate$timestamp), recipes_create)
recipes_create$timestamp <- as.Date(recipes_create$timestamp)
recipes_create$end <- as.Date(recipes_create$end)


# add calendar week of creation for every recipe
add_calendar_week <- function(table_for_calendar_week) {
  Calendar_week <- as.numeric(strftime(table_for_calendar_week$timestamp, format = "%W"))
  result <- cbind(table_for_calendar_week, Calendar_week)
  return(result)
}
recipes_create <- add_calendar_week(recipes_create)


# add weeks of life for every recipe
add_life_time_to_table <- function(table_to_add) {
  life_in_weeks <- ceiling(difftime(max(recipes_rate$timestamp), table_to_add$timestamp, units = "weeks"))
  result <- cbind(table_to_add, life_in_weeks)
  return(result)
}
recipes_create <- add_life_time_to_table(recipes_create)

### TABLE CONSTRUCTION ###
create_table_for_recipe <- function(row_to_calculate) {
  n_weeks <- row_to_calculate$life_in_weeks 
  id_col <- replicate(n_weeks, row_to_calculate$recipe_id) 
  data.table(
    "RECIPE ID" = id_col,
    period_in_weeks = seq(from = 1, to = n_weeks),
    calendar_week = format(seq(row_to_calculate$timestamp, row_to_calculate$end, by ="1 week"), "%U"),
    year = format(seq(row_to_calculate$timestamp, row_to_calculate$end, by ="1 week"), "%Y"),
    total_adopters = calculate_adopters(row_to_calculate$recipe_id, n_weeks, 
                                        row_to_calculate$starting_week, row_to_calculate$timestamp),
    mean_rate = calculate_mean(row_to_calculate$recipe_id, n_weeks, 
                               row_to_calculate$starting_week, row_to_calculate$timestamp),
    nr_comments = calculate_comments(row_to_calculate$recipe_id, n_weeks, 
                                     row_to_calculate$starting_week, row_to_calculate$timestamp),
    population_size = as.numeric(tail(temporary_table_users2$cumsum_users, as.numeric(n_weeks)))
  )
}


final_table <- data.table(
  "RECIPE ID" = numeric(),
  period_in_weeks = numeric(),
  calendar_week = numeric(),
  year = numeric(),
  total_adopters = numeric(),
  mean_rate = numeric(),
  nr_comments = numeric(),
  population_size = numeric()
)


# --> calculation of the nr. of adopters
calculate_adopters <- function(single_recipe, n_weeks, start_week, creation_timestamp) { 
  adopters_of_rec <- recipes_rate[recipes_rate$recipe_id == single_recipe,] 
  week_adoption <- ceiling(difftime(adopters_of_rec$timestamp, creation_timestamp, units = "weeks")) 
  adopters_of_rec <- cbind(adopters_of_rec, week_adoption) 
  adoption_by_week <- 0 
  count_adoption <- 0 
  end_week <- start_week + n_weeks 
  for (week in start_week:end_week) { 
    partial_count <- nrow(filter(adopters_of_rec, adopters_of_rec$week_adoption == week)) 
    count_adoption <- count_adoption + partial_count 
    adoption_by_week[week - start_week] <- count_adoption  
  }
  return(adoption_by_week)
}


# --> calculation of the mean rate
calculate_mean <- function(single_recipe, n_weeks, start_week, creation_timestamp) {
  rates_of_rec <- recipes_rate[recipes_rate$recipe_id == single_recipe,] 
  week_rate_created <- ceiling(difftime(rates_of_rec$timestamp, creation_timestamp, units = "weeks")) 
  rates_of_rec <- cbind(rates_of_rec, week_rate_created) 
  rate_by_week <- 0
  end_week <- start_week + n_weeks 
  n_rate <- 0
  total_sum_rate <- 0
  avg <- 0
  for (week in start_week:end_week) {
    rate_of_week <- rates_of_rec[rates_of_rec$week_rate_created == week,]
    if (nrow(rate_of_week) > 0) {
      n_rate <- n_rate + nrow(rate_of_week)
      total_sum_rate <- total_sum_rate + sum(rate_of_week$rating)
      avg <- total_sum_rate / n_rate
    }
    rate_by_week[week - start_week] <- avg
  }
  return(rate_by_week)
}


# --> calculation of nr. of comments
calculate_comments <- function(single_recipe, n_weeks, start_week, creation_timestamp) {
  comments_of_rec <- recipe_comments[recipe_comments$recipe_id == single_recipe,]
  week_comment_created <- ceiling(difftime(comments_of_rec$created_at, creation_timestamp, units = "weeks"))
  comments_of_rec <- cbind(comments_of_rec, week_comment_created)
  comments_by_week <- 0
  count_comment <- 0
  end_week <- start_week + n_weeks
  for (week in start_week:end_week) {
    partial_count <- nrow(filter(comments_of_rec, comments_of_rec$week_comment_created == week))
    count_comment <- count_comment + partial_count
    comments_by_week[week - start_week] <- count_comment
  }
  return(comments_by_week)
}


# ACTUAL CREATION OF FINAL TABLE indicating the nr. of recipes to include 
for (r in 1:2) { # to print the whole table write "r in 1:nrow(recipes_create)"
    new_table <- create_table_for_recipe( 
      recipes_create[r,]
    )
    final_table <- rbind(final_table, new_table)
}
