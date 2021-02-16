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
library(forcats)

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

# select only the recipes we need
recipes <- recipes[recipes$category == "Main dishes - meat" | recipes$category == "Main dishes - fish"
                   | recipes$category == "Main dishes - others" | recipes$category == "Main dishes - vegetarian" 
                   | recipes$category == "Pasta & rice dishes" | recipes$category == "Starters"]

recipes_create <- recipes_create[recipes_create$recipe_id %in% recipes$recipe_id,]
recipe_comments <- recipe_comments[recipe_comments$recipe_id %in% recipes$recipe_id,]
recipe_ingredients_mapping <- recipe_ingredients_mapping[recipe_ingredients_mapping$recipe_id %in% recipes$recipe_id,]
recipes_rate <- recipes_rate[recipes_rate$recipe_id %in% recipes$recipe_id,]


# --> emission calculation
# ingredients_emissions <- rename(ingredients_emissions, clean = name)
recipe_ingredients_mapping <- merge(recipe_ingredients_mapping, ingredients_emissions, 
                                    by.x="clean", by.y="name", all.x=TRUE)
recipe_ingredients_mapping$co2emissions <- as.numeric(
  recipe_ingredients_mapping$quantity*recipe_ingredients_mapping$emissions)

h_2 <- "co2emissions"
total_co2_emission <- unique(recipe_ingredients_mapping[, lapply(.SD, sum, na.rm=TRUE), by=recipe_id, .SDcols=h_2])


# --> mean rating calculation
h.1 <- "rating"
mean_rating <- unique(recipes_rate[, lapply(.SD, mean, na.rm=TRUE), by=recipe_id, .SDcols=h.1])
names(mean_rating)[2] <- "mean_rating"


# --> recipe age in weeks calculation
recipes_create <- recipes_create[, age_in_weeks :=ceiling(difftime(as.Date("31-12-2019", "%d-%m-%Y"), 
                                                                   recipes_create$timestamp, units = "weeks"))]


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


# --> type extraction
table_type <- recipe_ingredients_mapping %>% subset(type == "vegan" | type == "vegetarian" | type == "meat")

table_type$raw_type <- sample(1:nrow(table_type))

table_type <- table_type %>%
  mutate(raw_type = ifelse(type == "meat", 3, raw_type)) %>%
  mutate(raw_type = ifelse(type == "vegetarian", 2, raw_type)) %>%
  mutate(raw_type = ifelse(type == "vegan", 1, raw_type))

h_type <- "raw_type"

recipe_type <- unique(table_type[, lapply(.SD, max, na.rm=TRUE), by=recipe_id, .SDcols=h_type])

recipe_type$type <- sample(1:nrow(recipe_type))
recipe_type <- recipe_type %>%
  mutate(type = ifelse(raw_type ==  3,"meat", type)) %>%
  mutate(type = ifelse(raw_type == 2, "vegetarian", type)) %>%
  mutate(type = ifelse(raw_type ==  1,"vegan", type))


### table first part creation ###
table_first_part <- data.table(recipe_id=recipes$recipe_id)
table_first_part <- merge(x=table_first_part, y=recipe_type[, c("recipe_id", "type")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipes_create[, c("recipe_id", "cooking_skills")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipes_create[, c("recipe_id", "age_of_creator")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=total_co2_emission[, c("recipe_id", "co2emissions")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipes_rate[, c("recipe_id", "nr_adopters")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=mean_rating[, c("recipe_id", "mean_rating")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipe_comments[, c("recipe_id", "nr_comments")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipes_create[, c("recipe_id", "age_in_weeks")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipes_create[, c("recipe_id", "country")], by="recipe_id", all.x = TRUE)
table_first_part <- merge(x=table_first_part, y=recipes_create[, c("recipe_id", "timestamp")], by="recipe_id", all.x = TRUE)
table_first_part <- subset(table_first_part, age_of_creator > 7 & age_of_creator < 81)
table_first_part <- transform(table_first_part, age_in_weeks = as.numeric(age_in_weeks))

#####
##### INGREDIENTS AND CO2 SCORES #####
#####

table_first_part_hist <- table_first_part[complete.cases(type), ]

pdf("hist_emission.pdf")
ggplot(table_first_part_hist, aes(x=co2emissions, fill=type)) +
  geom_histogram(color = "white") + 
  labs(title="Frequency of emission by recipe type", x= bquote("g CO"[2]~.(paste0("-eq"))), y="Count") + 
  theme_minimal()
dev.off()

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

pdf("plot_cook.pdf")
ggplot(table_first_part_2, aes(x=cooking_level, fill=cooking_skills)) +
  geom_histogram(color = "darkolivegreen", bins = 8) + 
  scale_fill_manual(values = c("darkolivegreen4", "darkolivegreen3", "darkolivegreen1", "darkolivegreen")) +
  labs(title="Distribution of cooking skills", x="Level of cooking skills", y = "Count") + 
  theme_minimal()
dev.off()


# type
table_first_part$overall_type <- sample(1:nrow(table_first_part))

table_first_part <- table_first_part %>%
  mutate(overall_type = ifelse(type == "vegan", 1, overall_type)) %>%
  mutate(overall_type = ifelse(type == "vegetarian", 2, overall_type)) %>%
  mutate(overall_type = ifelse(type == "meat", 3, overall_type))

table_first_part_3 <- table_first_part[complete.cases(table_first_part[ , "type"]),]

pdf("plot_type.pdf")
ggplot(table_first_part_3, aes(x=overall_type, fill=type)) +
  geom_histogram(color = "white", bins = 8) + 
  scale_fill_manual(values = c("deepskyblue4", "deepskyblue3", "deepskyblue1")) +
  labs(title="Distribution of recipe types", x="Recipe types", y = "Count") + 
  scale_x_continuous(breaks=c(1, 2, 3)) + theme_minimal()
dev.off()


## 2) Scatterplots and Boxplots
# prep

sub_age <- data.table(emissions=table_first_part$co2emissions, age_of_creator=table_first_part$age_of_creator)
sub_age <- sub_age[complete.cases(sub_age), ]
sub_age <- sub_age[sub_age$emissions != 0, ]
sub_age$ln_emissions <- log(sub_age$emissions, base = exp(1))
sub_age <- sub_age[sub_age$age_of_creator > 0]

sub_adopters <- data.table(emissions=table_first_part$co2emissions, nr_adopters=table_first_part$nr_adopters)
sub_adopters <- sub_adopters[complete.cases(sub_adopters), ]
sub_adopters <- sub_adopters[sub_adopters$emissions != 0, ]
sub_adopters$ln_emissions <- log(sub_adopters$emissions, base = exp(1))
sub_adopters$ln_adopters <- log(sub_adopters$nr_adopters, base = exp(1))

sub_rate <- data.table(emissions=table_first_part$co2emissions, mean_rating=table_first_part$mean_rating)
sub_rate <- sub_rate[complete.cases(sub_rate), ]
sub_rate <- sub_rate[sub_rate$emissions != 0, ]
sub_rate$ln_emissions <- log(sub_rate$emissions, base = exp(1))

sub_comments <- data.table(emissions=table_first_part$co2emissions, nr_comments=table_first_part$nr_comments)
sub_comments <- sub_comments[complete.cases(sub_comments), ]
sub_comments <- sub_comments[sub_comments$emissions != 0, ]
sub_comments$ln_emissions <- log(sub_comments$emissions, base = exp(1))
sub_comments$ln_comments <- log(sub_comments$nr_comments, base = exp(1))

sub_life <- data.table(emissions=table_first_part$co2emissions, age_in_weeks=table_first_part$age_in_weeks)
sub_life <- sub_life[complete.cases(sub_life), ]
sub_life <- sub_life[sub_life$emissions != 0, ]
sub_life$ln_emissions <- log(sub_life$emissions, base = exp(1))

# Scatterplots
pdf("plot_age.pdf")
ggplot(sub_age, aes(x = age_of_creator, y = ln_emissions)) + 
  geom_point(size=2, shape=23, color="palegreen4", alpha = 0.8) + 
  labs(title="Relationship between emission and age of creator", 
       x="Age of the creator of the recipe", y = bquote("g CO"[2]~.(paste0("-eq (ln)")))) + 
  theme_minimal()
dev.off()

pdf("plot_adopters.pdf")
ggplot(sub_adopters, aes(x = ln_adopters, y = ln_emissions)) + 
  geom_point(size=2, shape=23, color="sienna2", alpha = 0.8) + 
  labs(title="Relationship between emission and number of adopters", 
       x="Number of adopters (ln)", y = bquote("g CO"[2]~.(paste0("-eq")))) + 
  theme_minimal() 
dev.off()

pdf("plot_rate.pdf")
ggplot(sub_rate, aes(x = mean_rating, y = ln_emissions)) + 
  geom_point(size=2, shape=23, color="coral2", alpha = 0.8) + 
  labs(title="Relationship between emission and recipe rate", 
       x="Mean rating", y = bquote("g CO"[2]~.(paste0("-eq")))) + 
  theme_minimal() 
dev.off()

pdf("plot_comments.pdf")
ggplot(sub_comments, aes(x = ln_comments, y = ln_emissions)) + 
  geom_point(size=2, shape=23, color="thistle3", alpha = 0.8) + 
  labs(title="Relationship between emission and number of comments", 
       x="Number of comments (ln)", y = bquote("g CO"[2]~.(paste0("-eq")))) + 
  theme_minimal()
dev.off()

pdf("plot_life.pdf")
ggplot(sub_life, aes(x = age_in_weeks, y = ln_emissions)) + 
  geom_point(size=2, shape=23, color="royalblue1", alpha = 0.8) +
  labs(title="Relationship between emission and age of the recipe", 
       x="Age (in weeks)", y = bquote("g CO"[2]~.(paste0("-eq")))) + 
  theme_minimal()
dev.off()


# Boxplots
pdf("boxplot_cooking_skills.pdf")
p = ggplot(table_first_part[table_first_part$cooking_skills=="beginner"
                            | table_first_part$cooking_skills=="amateurchef"
                            | table_first_part$cooking_skills=="advanced"
                            | table_first_part$cooking_skills=="chefcook",],
           aes(x=cooking_level, y=co2emissions, color=cooking_skills)) + 
  labs(title="Distribution of emission according to cooking skills", 
       x="Cooking skills", y = bquote("g CO"[2]~.(paste0("-eq"))))
p + geom_boxplot() + theme_minimal()
dev.off()


pdf("boxplot_type.pdf")
pp = ggplot(table_first_part[table_first_part$type=="vegan"
                             | table_first_part$type=="vegetarian"
                             | table_first_part$type=="meat",],
            aes(x=overall_type, y=co2emissions, color=type)) + 
  labs(title="Distribution of emission according to recipe type", 
  x="Recipe type", y = bquote("g CO"[2]~.(paste0("-eq"))))
pp + geom_boxplot() + theme_minimal()
dev.off()


## 3) Linear regressions
## What is the most used ingredient and the respective emission?
## Which ingredients have the lowest/ highest emissions?
my_ingredient_emission <- data.table(ingredient=recipe_ingredients_mapping$clean, 
                                     emissions=recipe_ingredients_mapping$emissions, 
                                     type=recipe_ingredients_mapping$type)
my_ingredient_emission <- unique(my_ingredient_emission)
my_ingredient_emission <- my_ingredient_emission[complete.cases(my_ingredient_emission[ , "emissions"]),] 
freq_ingredients <- as.data.frame(table(recipe_ingredients_mapping$clean))
freq_ingredients <- rename(freq_ingredients, ingredient = Var1)
my_ingredient_emission <- merge(x=my_ingredient_emission, y=freq_ingredients[, c("ingredient", "Freq")], 
                                by="ingredient", all.x = TRUE)
my_ingredient_emission$rank <- rank(my_ingredient_emission$emissions)

order.emissions <- order(my_ingredient_emission$emissions)
my_ingredient_emission <- my_ingredient_emission[order.emissions,]
highest_emission <- tail(my_ingredient_emission, 5)
write.csv(highest_emission, "highest_emission.csv")
lowest_emission <- head(my_ingredient_emission, 5)
write.csv(lowest_emission, "lowest_emission.csv")

order.freq <- order(my_ingredient_emission$Freq)
my_ingredient_emission <- my_ingredient_emission[order.freq,]
highest_use <- tail(my_ingredient_emission, 5)
write.csv(highest_use, "highest_use.csv")
lowest_use <- head(my_ingredient_emission, 5)
write.csv(lowest_use, "lowest_use.csv")


## How do emissions influence nr. of adopters?
lm_sub_adopters <- lm(ln_adopters ~ ln_emissions, data = sub_adopters)

## How do emissions influence nr. of comments? 
lm_sub_comments <- lm(ln_comments ~ ln_emissions, data = sub_comments)

## How do emissions influence mean_rating?
lm_sub_rate <- lm(mean_rating ~ ln_emissions, data = sub_rate)
stargazer(lm_sub_adopters, lm_sub_comments, lm_sub_rate, type = "text")

## Do age of the creator, life of recipe, type, cooking skills, (and country) influence emissions?
sub_one <- data.table(age_of_creator = table_first_part$age_of_creator,
                      age_of_recipe = table_first_part$age_in_weeks,
                      type = table_first_part$type,
                      coooking_skills = table_first_part$cooking_skills,
                      country = table_first_part$country,
                      emissions = table_first_part$co2emissions)
sub_one <- sub_one[complete.cases(sub_one),]
sub_one$ln_emissions <- log(sub_one$emissions, base = exp(1))

lm_tot <- lm(ln_emissions ~ age_of_creator + age_of_recipe + type + coooking_skills, data = sub_one)

stargazer(lm_tot, type = "text")


##### DYNAMICS OVER TIME #####

## Are recipes becoming more/ less sustainable in time?
table_dynamics <- data.table(type = table_first_part$type, year_publication = year(table_first_part$timestamp)) 
table_dynamics <- table_dynamics[complete.cases(table_dynamics[ , "type"]),]
freq_dynamics <- data.frame(table(table_dynamics$year_publication, table_dynamics$type))
names(freq_dynamics)[1] <- "Year"
names(freq_dynamics)[2] <- "Type"
freq_dynamics$Year = as.numeric(levels(freq_dynamics$Year))[freq_dynamics$Year]

pdf("plot_dymanics.pdf")
ggplot(freq_dynamics, aes(x = Year, y = Freq)) +
  geom_line(aes(color = Type), size = 1) +
  scale_x_continuous(breaks=c(2009, 2012, 2015, 2018, 2020)) + 
  labs(title="Number of recipes posted by year", 
       x="Year", y = "Count") + theme_minimal()
dev.off()


dbWriteTable(db, "monica_recipe_emission",  total_co2_emission, row.names=FALSE)
dbWriteTable(db, "monica_recipe_type",  recipe_type, row.names=FALSE)









