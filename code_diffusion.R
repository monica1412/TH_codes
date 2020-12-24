library(RPostgreSQL)
library(dplyr)        # many useful functions for quick data manipulation
library(tidyr)        # designed specifically for data tidying
library(data.table)
library(diffusion)

# Define connection parameters
source("conf.R")

# upload tables of interest
recipes_rate <- data.table(dbReadTable(db, "recipes_rate")) 
recipe_ingredients_mapping <- data.table(dbReadTable(db, "recipe_ingredients_mapping")) 

# --> type extraction
table_type <- recipe_ingredients_mapping %>% subset(type == "vegan" | type == "vegetarian" | 
                                                      type == "fish" | type == "meat")

table_type$raw_type <- sample(1:nrow(table_type))

table_type <- table_type %>%
  mutate(raw_type = ifelse(type == "meat", 4, raw_type)) %>%
  mutate(raw_type = ifelse(type == "fish", 3, raw_type)) %>%
  mutate(raw_type = ifelse(type == "vegetarian", 2, raw_type)) %>%
  mutate(raw_type = ifelse(type == "vegan", 1, raw_type))

h_type <- names(table_type)[6] # raw_type

recipe_type <- unique(table_type[, lapply(.SD, max, na.rm=TRUE), by=recipe_id, .SDcols=h_type])

recipe_type$type <- sample(1:nrow(recipe_type))
recipe_type <- recipe_type %>%
  mutate(type = ifelse(raw_type ==  4,"meat", type)) %>%
  mutate(type = ifelse(raw_type ==  3 , "fish",  type)) %>%
  mutate(type = ifelse(raw_type == 2, "vegetarian", type)) %>%
  mutate(type = ifelse(raw_type ==  1,"vegan", type))

recipes_rate <- merge(x=recipes_rate, y=recipe_type[, c("recipe_id", "type")], by="recipe_id", all.x = TRUE)
recipes_rate <- recipes_rate[complete.cases(recipes_rate[ , "type"]),]
recipes_rate$timestamp <- substr(recipes_rate$timestamp, 0,7)
recipes_rate <- cbind(nr = 1, recipes_rate)


### DIFFUSION ### 
diff_final <- data.table(type = recipes_rate$type, year_month_adoption = recipes_rate$timestamp) 
diff_final <- diff_final[complete.cases(diff_final[ , "type"]),]
diff_freq <- data.frame(table(diff_final$year_month_adoption, diff_final$type))
names(diff_freq)[1] <- "Year_Month"
names(diff_freq)[2] <- "Type"

select_vegan <- subset(diff_freq, diff_freq$Type == "vegan")
select_vegan[, 3] <- cumsum(select_vegan[, 3])

select_vegetarian <- subset(diff_freq, diff_freq$Type == "vegetarian")
select_vegetarian[, 3] <- cumsum(select_vegetarian[, 3])

select_pescatarian <- subset(diff_freq, diff_freq$Type == "fish")
select_pescatarian[, 3] <- cumsum(select_pescatarian[, 3])

select_omnivorous <- subset(diff_freq, diff_freq$Type == "meat")
select_omnivorous[, 3] <- cumsum(select_omnivorous[, 3])

fitbass <- diffusion(select_vegan[, 3], type = "bass")
fitbass
pdf("fitbass.pdf")
plot(fitbass)
dev.off()

fitgomp <- diffusion(select_vegan[, 3], type="gompertz")
fitgomp
pdf("fitgomp.pdf")
plot(fitgomp)
dev.off()

fitgsg <- diffusion(select_vegan[, 3], type = "gsgompertz")
fitgsg
pdf("fitgsg.pdf")
plot(fitgsg)
dev.off()


