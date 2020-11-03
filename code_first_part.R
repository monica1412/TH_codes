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


# upload tables of interest
users <- data.table(read.csv(paste("/Users/monicamirelabutnariuc/Desktop/tables/users.cvs")))
recipes_create <- data.table(read.csv(paste("/Users/monicamirelabutnariuc/Desktop/tables/recipes_create.cvs")))
recipe_comments <- data.table(read.csv(paste("/Users/monicamirelabutnariuc/Desktop/tables/recipe_comments.cvs")))
recipes_rate <- data.table(read.csv(paste("/Users/monicamirelabutnariuc/Desktop/tables/recipes_rate.cvs")))
recipes <- data.table(read.csv(paste("/Users/monicamirelabutnariuc/Desktop/tables/recipes.cvs")))
ingredients_emissions <- data.table(read.csv(paste("/Users/monicamirelabutnariuc/Desktop/tables/ingredients_emissions.cvs")))
recipe_ingredients_mapping <- data.table(read.csv(paste("/Users/monicamirelabutnariuc/Desktop/tables/recipe_ingredients_mapping.cvs")))


### MONICA'S TABLE CREATION ###

# --> emission calculation
ingredients_emissions <- rename(ingredients_emissions, raw = name)
recipe_ingredients_mapping <- left_join(recipe_ingredients_mapping, ingredients_emissions, by="raw")
recipe_ingredients_mapping$co2emissions <- as.numeric(recipe_ingredients_mapping$quantity*recipe_ingredients_mapping$emissions)

h_2 <- names (recipe_ingredients_mapping)[9]  # co2emissions
total_co2_emission <- unique(recipe_ingredients_mapping[, lapply(.SD, sum, na.rm=TRUE), by=recipe_id, .SDcols=h_2])


# --> mean rating calculation
h.1 <- names(recipes_rate)[4] # rating
mean_rating <- unique(recipes_rate[, lapply(.SD, mean, na.rm=TRUE), by=recipe_id, .SDcols=h.1])
mean_rating <- rename(mean_rating, mean_rating = rating)


# --> recipe age in weeks calculation
recipes_create <- recipes_create[, age_in_weeks :=ceiling(difftime(max(recipes_rate$timestamp), recipes_create$timestamp, units = "weeks"))]


# --> nr comments calculation
recipe_comments <- recipe_comments[, nr_comments := as.numeric(.N), by = recipe_id] # count elements 
recipe_comments <- recipe_comments[!duplicated(recipe_id),]


# --> nr adopters calculation
recipes_rate <- recipes_rate[, nr_adopters := as.numeric(.N), by = recipe_id] # count elements 
recipes_rate <- recipes_rate[!duplicated(recipe_id),]


# --> creator characteristics 
age_of_creator_when_creating <- users[, age_of_creator := year(users$created_at) - year(users$birthday) ]
recipes_create <- merge(x=recipes_create, y=users[, c("user_id", "cooking_skills")], by="user_id", all.x = TRUE)
recipes_create <- merge(x=recipes_create, y=users[, c("user_id", "age_of_creator")], by="user_id", all.x = TRUE)


### table first part creation ###
table_first_part <- data.table(recipe_id=recipes$recipe_id,  
                           type=recipes$type)

table_first_part <- merge(x=table_first_part, y=recipes_create[, c("recipe_id", "cooking_skills")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipes_create[, c("recipe_id", "age_of_creator")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=total_co2_emission[, c("recipe_id", "co2emissions")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipes_rate[, c("recipe_id", "nr_adopters")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=mean_rating[, c("recipe_id", "mean_rating")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipe_comments[, c("recipe_id", "nr_comments")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipes_create[, c("recipe_id", "age_in_weeks")], by="recipe_id", all.x = TRUE)


### DOWNLOAD TABLE AS CVS FILE ###
# write.csv(table_first_part, "table_first_part.cvs")










##### INGREDIENTS AND CO2 SCORES #####
## What is the most used ingredient and the respective emission?
## Which ingrediets have the lowest/ highest emissions?
my_ingredient_emission <- data.table(ingredient=recipe_ingredients_mapping$raw, emissions=recipe_ingredients_mapping$emissions, type=recipe_ingredients_mapping$type) 
my_ingredient_emission <- unique(my_ingredient_emission)  
my_ingredient_emission <- my_ingredient_emission[complete.cases(my_ingredient_emission[ , "emissions"]),] # emissions or 2
freq_ingredients <- as.data.frame(table(recipe_ingredients_mapping$raw))
freq_ingredients <- rename(freq_ingredients, ingredient = Var1)
my_ingredient_emission <- merge(x=my_ingredient_emission, y=freq_ingredients[, c("ingredient", "Freq")], by="ingredient", all.x = TRUE)

order.emissions <- order(my_ingredient_emission$emissions)
my_ingredient_emission <- my_ingredient_emission[order.emissions,]
my_ingredient_emission$rank <- rank(my_ingredient_emission$emissions)

my_ingredient_emission[which.max(my_ingredient_emission$Freq)]


## How does the age of the creator influence emission? 
sub_1 <- data.table(emissions=table_first_part$co2emissions, age_of_creator=table_first_part$age_of_creator) 
sub_1 <- sub_1[complete.cases(sub_1[ , "emissions"]),] # emissions or 2

lm_sub_1 <- lm(emissions ~ age_of_creator, data = sub_1) 
stargazer(lm_sub_1, type = "text")

## How does emissions influence nr. of adopters? 
sub_2 <- data.table(emissions=table_first_part$co2emissions, nr_adopters=table_first_part$nr_adopters) 
sub_2 <- sub_2[complete.cases(sub_2[ , "emissions"]),] # emissions or 2

lm_sub_2 <- lm(nr_adopters ~ emissions, data = sub_2) 
stargazer(lm_sub_2, type = "text")

## How does emissions influence nr. of comments?
sub_3 <- data.table(emissions=table_first_part$co2emissions, nr_comments=table_first_part$nr_comments) 
sub_3 <- sub_3[complete.cases(sub_3[ , "emissions"]),] # emissions or 2

lm_sub_3 <- lm(nr_comments ~ emissions, data = sub_3) 
stargazer(lm_sub_3, type = "text")

## How does emissions influence nr. of comments?
sub_4 <- data.table(emissions=table_first_part$co2emissions, mean_rating=table_first_part$mean_rating) 
sub_4 <- sub_4[complete.cases(sub_4[ , "emissions"]),] # emissions or 2

lm_sub_4 <- lm(mean_rating ~ emissions, data = sub_4) 
stargazer(lm_sub_4, type = "text")


stargazer(lm_sub_1, lm_sub_2, lm_sub_3, lm_sub_4, type = "text")


## Does type (vegan - meat) influence emissions? 
sub_6 <- data.table(emissions=table_first_part$co2emissions, type=table_first_part$type) 
sub_6 <- sub_6[sub_6$type %in% c("vegan", "meat"), ]
sub_6 <- sub_6[complete.cases(sub_6[ , "emissions"]),] 
sub_6$type <- as.numeric(sub_6$type=="vegan") # 1 if vegan

lm_sub_6 <- lm(emissions ~ type, data = sub_6) 
stargazer(lm_sub_6, type = "text")


## Does cooking skills (beginner - chefcook) influence emissions? 
sub_7 <- data.table(emissions=table_first_part$co2emissions, coooking_skills=table_first_part$cooking_skills) 
sub_7 <- sub_7[sub_7$coooking_skills %in% c("beginner", "chefcook"), ]
sub_7 <- sub_7[complete.cases(sub_7[ , "emissions"]),] 
sub_7$coooking_skills <- as.numeric(sub_7$coooking_skills=="beginner") # 1 if beginner

lm_sub_7 <- lm(emissions ~ coooking_skills, data = sub_7) 
stargazer(lm_sub_7, type = "text")

stargazer(lm_sub_7, lm_sub_6, type = "text")

##### DYNAMICS OVER TIME #####

## Are recipes becoming more/ less sustainable in time? # look at older recipes (higher age in weeks)
table_dynamics <- data.table(type=table_first_part$type, age_in_weeks=table_first_part$age_in_weeks) 
table_dynamics <- table_dynamics[complete.cases(table_dynamics[ , "type"]),]

ago_0_10 <- subset(table_dynamics, age_in_weeks > 0 & age_in_weeks < 522)  # 1-521
freq_ago_0_10 <- as.data.frame(table(ago_0_10$type))

ago_10_20 <- subset(table_dynamics, age_in_weeks > 521 & age_in_weeks < 1043) # 522-1042
freq_ago_10_20 <- as.data.frame(table(ago_10_20$type))

ago_20_30 <- subset(table_dynamics, age_in_weeks > 1042 & age_in_weeks < 1564) # 1043-1563
freq_ago_20_30 <- as.data.frame(table(ago_20_30$type))

ago_30_40 <- subset(table_dynamics, age_in_weeks > 1563 & age_in_weeks < 2085) # 1564-2084
freq_ago_30_40 <- as.data.frame(table(ago_30_40$type))

ago_40_50 <- subset(table_dynamics, age_in_weeks > 2084 & age_in_weeks < 2607) # 2085-2606
freq_ago_40_50 <- as.data.frame(table(ago_40_50$type))













