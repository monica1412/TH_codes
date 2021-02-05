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
library(diffusion)


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
recipe_emission <- data.table(dbReadTable(db, "monica_recipe_emission"))
recipe_type <- data.table(dbReadTable(db, "monica_recipe_type"))

# select only the recipes we need
recipes <- recipes[recipes$category == "Main dishes - meat" | recipes$category == "Main dishes - fish"
                   | recipes$category == "Main dishes - others" | recipes$category == "Main dishes - vegetarian" 
                   | recipes$category == "Pasta & rice dishes" | recipes$category == "Starters"]

recipes <- recipes[recipes$recipe_id %in% recipe_type$recipe_id,]
recipes_create <- recipes_create[recipes_create$recipe_id %in% recipes$recipe_id,]
recipe_comments <- recipe_comments[recipe_comments$recipe_id %in% recipes$recipe_id,]
recipe_ingredients_mapping <- recipe_ingredients_mapping[recipe_ingredients_mapping$recipe_id %in% recipes$recipe_id,]
recipes_rate <- recipes_rate[recipes_rate$recipe_id %in% recipes$recipe_id,]

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


# add column of 1s (reference for adopters, rate and comments)
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




### DIFFUSION ###

final_table <- final_table[final_table$total_adopters != 0, ] # necessary, otherwise recipes with 0 final adopters will mess with the code
final_table <- as.data.frame(final_table) # necessary, because diffusion works only with data frames
my_recipe_id <- data.table(recipe_id = unique(final_table$`RECIPE ID`), nr = 1:length(unique(final_table$`RECIPE ID`)))

# --> fitbass
final_diff <- data.table(
  V1 = numeric(),
  V2 = numeric(),
  V3 = numeric()
)

for (i in unique(final_table$`RECIPE ID`)) { 
  fitbass <- diffusion(final_table[final_table$`RECIPE ID`== i, 5], type = "bass")
  new_fitbass <- data.table(matrix(fitbass$w, nrow = 1))
  final_diff <- rbind(final_diff, new_fitbass, fill=TRUE)
}

final_diff$nr <- 1:length(unique(final_table$`RECIPE ID`))
final_diff <- merge(final_diff, my_recipe_id, by="nr")
final_diff <- merge(final_diff, recipe_type[, c("recipe_id", "type")], by="recipe_id", all.x = TRUE)
final_diff <- merge(final_diff, recipe_emission[, c("recipe_id", "co2emissions")], by="recipe_id", all.x = TRUE)

names(final_diff)[3] <- "fitbass_p"
names(final_diff)[4] <- "fitbass_q"
names(final_diff)[5] <- "fitbass_m"

final_diff <- final_diff[, c(2, 1, 6, 7, 3, 4, 5)]

# --> fitgomp
final_fitgomp <- data.table(
  V1 = numeric(),
  V2 = numeric(),
  V3 = numeric()
)

for (i in unique(final_table$`RECIPE ID`)) { 
  fitgomp <- diffusion(final_table[final_table$`RECIPE ID`== i, 5], type = "gompertz")
  new_fitgomp <- data.table(matrix(fitgomp$w, nrow = 1))
  final_fitgomp <- rbind(final_fitgomp, new_fitgomp, fill=TRUE)
}

final_fitgomp$nr <- 1:length(unique(final_table$`RECIPE ID`))
names(final_fitgomp)[1] <- "fitgomp_a"
names(final_fitgomp)[2] <- "fitgomp_b"
names(final_fitgomp)[3] <- "fitgomp_m"

final_diff <- merge(final_diff, final_fitgomp, by="nr")


# --> fitgsg
final_fitgsg <- data.table(
  V1 = numeric(),
  V2 = numeric(),
  V3 = numeric(),
  V4 = numeric()
)

for (i in unique(final_table$`RECIPE ID`)) { 
  fitgsg <- diffusion(final_table[final_table$`RECIPE ID`== i, 5], type = "gsgompertz")
  new_fitgsg <- data.table(matrix(fitgsg$w, nrow = 1))
  final_fitgsg <- rbind(final_fitgsg, new_fitgsg, fill=TRUE)
}

final_fitgsg$nr <- 1:length(unique(final_table$`RECIPE ID`))
names(final_fitgsg)[1] <- "fitgsg_a"
names(final_fitgsg)[2] <- "fitgsg_b"
names(final_fitgsg)[3] <- "fitgsg_c"
names(final_fitgsg)[4] <- "fitgsg_m"

final_diff <- merge(final_diff, final_fitgsg, by="nr")
final_diff$nr <- NULL
final_diff$recipe_id <- NULL

aggregated_diffusion <- aggregate(final_diff[, 2:12], list(final_diff$type), mean)

write.csv(aggregated_diffusion, "aggregated_diffusion.csv")

# dbWriteTable(db, "monica_diffusion",  final_diff, row.names=FALSE)












