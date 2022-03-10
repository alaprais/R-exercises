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


