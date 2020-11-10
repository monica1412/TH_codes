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
library(foreach)
library(doMC)

# Source relevant files
source("conf.R")
sapply(list.files("R", full.names=TRUE), source)

# Read in data
recipes_create <- data.table(dbReadTable(db, "recipes_create"))
recipes_rate <- data.table(dbReadTable(db, "recipes_rate"))
recipes <- data.table(dbReadTable(db, "recipes"))
users <- data.table(dbReadTable(db, "users"))
recipe_comments <- data.table(dbReadTable(db, "recipe_comments"))
recipe_ingredients_mapping <- data.table(dbReadTable(db, "recipe_ingredients_mapping"))
# ingredients_emissions <- data.table(dbReadTable(db, "ingredients_emissions"))

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
recipes_create <- add_calendar_week(recipes_create)
recipes_create <- add_life_time_to_table(recipes_create)

### TABLE CONSTRUCTION ###
# final_table <- data.table(
#  "RECIPE ID" = numeric(),
#  period_in_weeks = numeric(),
#  calendar_week = numeric(),
#  year = numeric(),
#  total_adopters = numeric(),
#  mean_rate = numeric(),
#  nr_comments = numeric(),
#  population_size = numeric()
# )

# ACTUAL CREATION OF FINAL TABLE indicating the nr. of recipes to include
registerDoMC(4) # How many cores to use
final_table <- foreach(r = 1:nrow(recipes_create[1:4]))  %dopar% { # to print the whole table write "r in 1:nrow(recipes_create)"
    # Print status
    if(r %% 100 == 0)
        print (r/nrow(recipes_create))

    new_table <- create_table_for_recipe(
      recipes_create[r,]
    )
    new_table
}
final_table <- rbindlist(final_table)

dbWriteTable(db, "radu_aggregated_diffusion",  final_table, row.names=FALSE)
