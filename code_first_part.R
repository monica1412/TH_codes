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

# Define connection parameters
source("conf.R")

# upload tables of interest
recipes_create <- data.table(dbReadTable(db, "recipes_create"))
recipes_rate <- data.table(dbReadTable(db, "recipes_rate"))
recipes <- data.table(dbReadTable(db, "recipes"))
users <- data.table(dbReadTable(db, "users"))
recipe_comments <- data.table(dbReadTable(db, "recipe_comments"))
recipe_ingredients_mapping <- data.table(dbReadTable(db, "recipe_ingredients_mapping"))
ingredients_emissions <- data.table(dbReadTable(db, "ingredients_emissions"))


### MONICA'S TABLE CREATION ###

# --> emission calculation
ingredients_emissions <- rename(ingredients_emissions, raw = name)
recipe_ingredients_mapping <- left_join(recipe_ingredients_mapping, ingredients_emissions, by="raw")
recipe_ingredients_mapping$co2emissions <- as.numeric(recipe_ingredients_mapping$quantity*recipe_ingredients_mapping$emissions)

h_2 <- names (recipe_ingredients_mapping)[7]  # co2emissions
total_co2_emission <- unique(recipe_ingredients_mapping[, lapply(.SD, sum, na.rm=TRUE), by=recipe_id, .SDcols=h_2])


# --> mean rating calculation
h.1 <- names(recipes_rate)[3] # rating
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
recipes_create <- merge(x=recipes_create, y=users[, c("user_id", "country")], by="user_id", all.x = TRUE)

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
table_first_part <- merge(x=table_first_part, y=recipes_create[, c("recipe_id", "country")], by="recipe_id", all.x = TRUE)


#####
##### INGREDIENTS AND CO2 SCORES #####
#####

## 1) Table of summary statistics

sumary_statistics <- table_first_part %>%
  subset(select=c("co2emissions", "age_in_weeks", "nr_adopters", "mean_rating",
                  "nr_comments", "age_of_creator"))
stargazer(as.data.frame(sumary_statistics), type = "text")

# cooking skills
table_first_part$cooking_level <- sample(1:nrow(table_first_part))

table_first_part <- table_first_part %>%
  mutate(cooking_level = ifelse(cooking_skills == "beginner", 1, cooking_level)) %>%
  mutate(cooking_level = ifelse(cooking_skills == "amateurchef", 2, cooking_level)) %>%
  mutate(cooking_level = ifelse(cooking_skills == "advanced", 3, cooking_level)) %>%
  mutate(cooking_level = ifelse(cooking_skills == "chefcook", 4, cooking_level))

table_first_part_2 <- table_first_part[complete.cases(table_first_part[ , "cooking_skills"]),]

pdf("plotcook.pdf")
ggplot(table_first_part_2)+
  geom_histogram(aes(x=cooking_level, fill=cooking_skills), bins = 8) + labs(title="Distribution of cooking skills",
                                                                             x="Level of cooking skills", y = "Count") + theme_bw()
dev.off()


# type
table_first_part$overall_type <- sample(1:nrow(table_first_part))

table_first_part <- table_first_part %>%
  mutate(overall_type = ifelse(type == "vegan", 1, overall_type)) %>%
  mutate(overall_type = ifelse(type == "vegetarian", 2, overall_type)) %>%
  mutate(overall_type = ifelse(type == "fish", 3, overall_type)) %>%
  mutate(overall_type = ifelse(type == "meat", 4, overall_type))

table_first_part_3 <- table_first_part[complete.cases(table_first_part[ , "type"]),]

pdf("plottype.pdf")
ggplot(table_first_part_3)+
  geom_histogram(aes(x=overall_type, fill=type), bins = 8) + labs(title="Distribution of recipe types",
                                                                  x="Recipe types", y = "Count") + theme_bw()
dev.off()


## 2) Scatterplots and Boxplots
# Scatterplots
pdf("plot_age.pdf")
ggplot(table_first_part, aes(x = age_of_creator,
                      y = co2emissions)) + geom_point(size=2, shape=23, color="palegreen4", alpha = 0.3)
dev.off()

pdf("plot_adopters.pdf")
ggplot(table_first_part, aes(x = nr_adopters,
                             y = co2emissions)) + geom_point(size=2, shape=23, color="sienna2", alpha = 0.3)
dev.off()

pdf("plot_rate.pdf")
ggplot(table_first_part, aes(x = mean_rating,
                             y = co2emissions)) + geom_point(size=2, shape=23, color="pink", alpha = 0.3)
dev.off()

pdf("plot_comments.pdf")
ggplot(table_first_part, aes(x = nr_comments,
                             y = co2emissions)) + geom_point(size=2, shape=23, color="thistle3", alpha = 0.3)
dev.off()

pdf("plot_life.pdf")
ggplot(table_first_part, aes(x = age_in_weeks,
                             y = co2emissions)) + geom_point(size=2, shape=23, color="royalblue1", alpha = 0.3)
dev.off()


# Boxplots
pdf("plot_boxcookingskills.pdf")
p = ggplot(table_first_part[table_first_part$cooking_skills=="beginner"
                            | table_first_part$cooking_skills=="amateurchef"
                            | table_first_part$cooking_skills=="advanced"
                            | table_first_part$cooking_skills=="chefcook",],
           aes(x=cooking_level, y=co2emissions, fill=cooking_skills))
p + geom_boxplot()
dev.off()


pdf("plot_boxtype.pdf")
pp = ggplot(table_first_part[table_first_part$type=="vegan"
                            | table_first_part$type=="vegetarian"
                            | table_first_part$type=="fish"
                            | table_first_part$type=="meat",],
           aes(x=overall_type, y=co2emissions, fill=type))
pp + geom_boxplot()
dev.off()


## 3) Linear regressions
## What is the most used ingredient and the respective emission?
## Which ingredients have the lowest/ highest emissions?
my_ingredient_emission <- data.table(ingredient=recipe_ingredients_mapping$raw, emissions=recipe_ingredients_mapping$emissions, type=recipe_ingredients_mapping$type)
my_ingredient_emission <- unique(my_ingredient_emission)
my_ingredient_emission <- my_ingredient_emission[complete.cases(my_ingredient_emission[ , "emissions"]),] # emissions or 2
freq_ingredients <- as.data.frame(table(recipe_ingredients_mapping$raw))
freq_ingredients <- rename(freq_ingredients, ingredient = Var1)
my_ingredient_emission <- merge(x=my_ingredient_emission, y=freq_ingredients[, c("ingredient", "Freq")], by="ingredient", all.x = TRUE)

order.emissions <- order(my_ingredient_emission$emissions)
my_ingredient_emission <- my_ingredient_emission[order.emissions,]
my_ingredient_emission$rank <- rank(my_ingredient_emission$emissions)

write.csv(my_ingredient_emission, "my_ingredient_emission.csv")

## How does emissions influence nr. of adopters?
sub_2 <- data.table(emissions=table_first_part$co2emissions, nr_adopters=table_first_part$nr_adopters)
sub_2 <- sub_2[complete.cases(sub_2[ , "emissions"]),] # emissions or 2
lm_sub_2 <- lm(nr_adopters ~ emissions, data = sub_2)

## How does emissions influence nr. of comments?
sub_3 <- data.table(emissions=table_first_part$co2emissions, nr_comments=table_first_part$nr_comments)
sub_3 <- sub_3[complete.cases(sub_3[ , "emissions"]),] # emissions or 2
lm_sub_3 <- lm(nr_comments ~ emissions, data = sub_3)

## How does emissions influence mean_rating?
sub_4 <- data.table(emissions=table_first_part$co2emissions, mean_rating=table_first_part$mean_rating)
sub_4 <- sub_4[complete.cases(sub_4[ , "emissions"]),] # emissions or 2
lm_sub_4 <- lm(mean_rating ~ emissions, data = sub_4)
stargazer(lm_sub_2, lm_sub_3, lm_sub_4, type = "text")

## Does age of the creator, type (vegan - meat), and cooking skills (beginner - chefcook) influence emissions?
sub_one <- data.table(recipe_id = table_first_part$recipe_id,
                      age_of_creator = table_first_part$age_of_creator,
                      type = table_first_part$type,
                      coooking_skills = table_first_part$cooking_skills,
                      country = table_first_part$country,
                      emissions = table_first_part$co2emissions)
sub_one <- sub_one[complete.cases(sub_one),]

lm_tot <- lm(emissions ~ age_of_creator + type + coooking_skills + country, data = sub_one)

# lm_tot <- lm(emissions ~ age_of_creator + type + coooking_skills, data = sub_one)

stargazer(lm_tot, type = "text")


##### DYNAMICS OVER TIME #####

## Are recipes becoming more/ less sustainable in time?
table_dynamics <- data.table(type=table_first_part$type, age_in_weeks=table_first_part$age_in_weeks)
table_dynamics <- table_dynamics[complete.cases(table_dynamics[ , "type"]),]

ago_0_10 <- subset(table_dynamics, age_in_weeks > 0 & age_in_weeks < 522)  # 1-521
freq_ago_0_10 <- as.data.frame(table(ago_0_10$type))
freq_ago_0_10 <- cbind(freq_ago_0_10, year = 2020)

ago_10_20 <- subset(table_dynamics, age_in_weeks > 521 & age_in_weeks < 1043) # 522-1042
freq_ago_10_20 <- as.data.frame(table(ago_10_20$type))
freq_ago_10_20 <- cbind(freq_ago_10_20, year = 2010)

ago_20_30 <- subset(table_dynamics, age_in_weeks > 1042 & age_in_weeks < 1564) # 1043-1563
freq_ago_20_30 <- as.data.frame(table(ago_20_30$type))
freq_ago_20_30 <- cbind(freq_ago_20_30, year = 2000)

ago_30_40 <- subset(table_dynamics, age_in_weeks > 1563 & age_in_weeks < 2085) # 1564-2084
freq_ago_30_40 <- as.data.frame(table(ago_30_40$type))
freq_ago_30_40 <- cbind(freq_ago_30_40, year = 1990)

ago_40_50 <- subset(table_dynamics, age_in_weeks > 2084 & age_in_weeks < 2607) # 2085-2606
freq_ago_40_50 <- as.data.frame(table(ago_40_50$type))
freq_ago_40_50 <- cbind(freq_ago_40_50, year = 1980)

dynamics <- rbind(freq_ago_0_10, freq_ago_10_20, freq_ago_20_30, freq_ago_30_40, freq_ago_40_50)
dynamics <- rename(dynamics, Types = Var1)

pdf("plot_dymanics.pdf")
ggplot(dynamics, aes(x = year, y = Freq)) +
  geom_line(aes(color = Types), size = 1) +
  ylim(1000, 1400) +
  theme_light()
dev.off()










