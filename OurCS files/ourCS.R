# setwd(...) # Set working directory to source file location

library(readr) # Install if needed

# Read data into R
dream_table = read_csv("dream_table_final_merge.csv")
View(dream_table)

# Check how many rows (projects) there are
nrow(dream_table)

# Check names of columns
names(dream_table)

# Sanity check: number of unique slugs should match number of rows
length(unique(dream_table$slug))

# Summary statistics for the response variable
summary(dream_table$female_ratio)
hist(dream_table$female_ratio)
# .. only for projects with at least one female contributor
hist(dream_table[dream_table$female_ratio > 0,]$female_ratio)

# Check how many projects have at least one female contributor
table(dream_table$female_ratio > 0)

# Make a boolean variable to encode at least one female contributor
dream_table$has_female = dream_table$female_ratio > 0
table(dream_table$has_female)

# Start exploring features

# Stars
summary(dream_table$stars)
hist(dream_table$stars)
dream_table$log_stars = log(dream_table$stars+1)
hist(dream_table$log_stars)

# Compare stars between female vs no-female projects
boxplot(dream_table$log_stars)

boxplot(list(with_female = dream_table[dream_table$has_female == TRUE,]$log_stars,
             without_female = dream_table[dream_table$has_female == FALSE,]$log_stars))

# Number of contributors
names(dream_table)
summary(dream_table$total_num_contributors)
hist(dream_table$total_num_contributors)
dream_table$log_contributors = log(dream_table$total_num_contributors)
hist(dream_table$log_contributors)

boxplot(list(with_female = dream_table[dream_table$has_female == TRUE,]$log_contributors,
             without_female = dream_table[dream_table$has_female == FALSE,]$log_contributors))

# Test correlation between popularity and gender

m1 = glm(has_female ~ log_stars,
         family = "binomial",
         data = dream_table)

summary(m1)


m2 = glm(has_female ~ log_stars + log_contributors,
         family = "binomial",
         data = dream_table)

summary(m2)

# ... skip to the end

names(dream_table)

table(dream_table$coc)
str(dream_table$coc)
dream_table$has_coc = dream_table$coc == 1

table(dream_table$contrib)
str(dream_table$contrib)
dream_table$has_contrib = dream_table$contrib == 1

summary(dream_table$compound)
hist(dream_table$compound)
dream_table$sentiment = dream_table$compound

summary(dream_table$readability)
hist(dream_table$readability)
hist(dream_table$readMeLength)
hist(dream_table$readability / dream_table$readMeLength)
summary(dream_table$readability / dream_table$readMeLength)

dream_table$normalized_readability = dream_table$readability / dream_table$readMeLength

names(dream_table)
hist(dream_table$header_count)

hist(dream_table$total_num_commits)
summary(dream_table$total_num_commits)
hist(log(dream_table$total_num_commits))
dream_table$log_commits = log(dream_table$total_num_commits)

dream_table_subset = subset(dream_table, readability / readMeLength > -600)
hist(dream_table_subset$readability / dream_table_subset$readMeLength)
nrow(dream_table_subset)

mn = glm(has_female ~ 
            log_contributors 
          + log_stars
          + has_coc
          + has_contrib
          + sentiment
          + header_count
          + normalized_readability
          , family = "binomial"
          , data = dream_table_subset)

# No colinearity
library(car)
vif(mn)

# Sad :( No correlation
summary(mn)
anova(mn)
