library(dplyr)
df <- data.frame(Theoph)
names(df)

# 3 select columns
sub <- select(df, "Subject":"Dose")
sub <- select(df, "Wt","Dose")

# 4 filter
df_4 <- filter(df, Dose>5, Time > mean(Time, na.rm = TRUE))

#5 sorting
df <- arrange(df, desc(Wt))
df <- arrange(df, Wt) # ascending is default
df <- arrange(df, Wt, desc(Time)) # sorted by weight, then subsorted by time

#6 creating new cols (mutating)
df <- mutate(df, trend = Time - mean(Time, na.rm = TRUE))

df <- mutate(df, weight_cat = ifelse(Wt < 66.68, "Welterweight", 
                              ifelse(Wt < 69.85, "Light-Middleweight", 
                              ifelse(Wt < 72.57, "Middleweight",
                                 "Super-Middleweight"))))

# 7 group by
df_weight_group <- group_by(df, weight_cat)
sumry <- summarize(df_weight_group, mean(Time, na.rm=TRUE), sum(Dose))

###############################################################################
###############################################################################
####### More Smooth Data Exploration ##########
############

library(AER)
data("Fertility")
glimpse(Fertility)

# selecting rows
Fertility %>% select("age","work") %>% slice(35:50)

# select last row
slice_tail(Fertility)

# how many women proceeded to have a third child
Fertility %>% filter(morekids == "yes") %>% count()

# most common gender combination
Fertility %>% group_by(gender1,gender2) %>% summarize(n())

#By racial composition, the proportion of woman working four weeks or less in 1979
Fertility %>% group_by(afam,hispanic,other) %>% summarize(mean(work<4))

# proportion of women between the age 22 and 24 who had a boy as their firstborn
Fertility %>% filter(age >= 22, age <= 24) %>% summarize(mean(gender1 == 'male'))

# new column, age squared
Fertility <- Fertility %>% mutate(age_squared = age**2)

# racial composition in the dataset which had the 
# lowest proportion of boys for their firstborn
# number of observation in each category as well
Fertility %>% 
  group_by(afam,hispanic,other) %>% summarize(prop_boys = mean(gender1 == 'male'), n()) %>% 
  arrange(prop_boys)

# proportion of women who have a third child by gender combination of the first two children
Fertility %>% group_by(gender1,gender2) %>% 
  summarize(prop_third = mean(morekids == "yes")) %>% arrange(prop_third)


###############################################################################
###############################################################################
####### dplyr 17 ##########
############


