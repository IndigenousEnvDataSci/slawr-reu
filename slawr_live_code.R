# COMPASS workshop April 8 2025 

# Slides: https://docs.google.com/presentation/d/1xsj7_2NHbnBNsA34Jp61f0PypGFSeM7Kl8kJFhDnR54/edit?usp=sharing

# introduction to programming in R 

# Goal 1: Become familiar with Rstudio Env. 

# first step when starting R is to load packages 
# install package if not already there 
install.packages("tidyverse")
# load your package 
library(tidyverse)

# --
# Goal 2: create an object 
# -- 

# define variables 
weight = 150 # lbs 

days_per_week = 2 # days per week eating fish 

# example of using print function 
print(weight)

# example of checking the type of data
typeof(days_per_week)

# --
# Goal 3: do calculations 
# -- 

# use our variables to calculate the mean daily consumption rate (cr)
# FDA reference amounts customarily consumed (RACC) per eating occasion for fish and shellfish without sauce is 110 grams for uncooked fish.
cr = 110*days_per_week/7 # daily consumption rate 

# calculate the safe limit screening value 
# create another variable 
RfD = 0.1 # ug mercurry/kg body weight/day 

safe_limit = RfD*weight*0.45/cr 

# --
# Goal 3: Create a vector
# -- 

# make list 
my_list <- list(100, "rachel", TRUE, 3.4)
# lists can contain mix of different data types 

# make another list - vectors 
weight_vector <- c(100, 175, 200, 220)
servings_per_week <- c(1, 7, 3, 4)
# vectors have the same type of data 

# -- 
# Goal 3: Create a data frame 
# -- 

# data frame 
sv_df <- data.frame(weight_vector, servings_per_week)

# reference your vectors in data frame $ 
sv_df$weight_vector

sv_df <- sv_df %>% # pipe can be made with ctrl+shift+M 
  mutate(screen_val = RfD*weight_vector*0.45/(110*servings_per_week/7)) # mutate function is used to add new column to data frame 

# check data frame 
head(sv_df)

#  use different functions on our data 
mean(sv_df)

# get help with function 
?mean

mean(sv_df$screen_val)


# read in a longer data frame 

sv_df_long <- read.csv("screening_values_per_week.csv")


# explore data frame 
str(sv_df_long) # preview the structure of data frame

# -- 
# Goal 5: Get descriptive statistics 
# --
# summarize the data 
summary(sv_df_long$screen_val)

summary(sv_df_long)

# what if we want to know only for servings 1 per week? 
# or below a specific weight 
sv_df_long %>% 
  filter(serving_per_week==1)

sv_df_long %>% 
  filter(weight < 160)

# --
# Goal 7: plot data frame
# -- 

# plot our data 
ggplot(sv_df_long, aes(x=serving_per_week, y=screen_val, col=weight)) + 
  geom_point(size=2) + 
  geom_hline(aes(yintercept = 0.46), linetype='dashed')  # 0.46 is max to avoid 

ggplot(sv_df_long, aes(x=serving_per_week, y=screen_val, group=serving_per_week)) +
  geom_boxplot()

# take away of plots: no matter how much you weigh, if you eat fish frequently you should avoid mercury levels above 0.1ug/g

# note - we did not have time to talk about Goal 6: Unit conversions, although they were occurring in the calculations. This is something you could try in your own time with the next steps! 

# next steps: look back at figure in slide 5 (https://docs.google.com/presentation/d/1xsj7_2NHbnBNsA34Jp61f0PypGFSeM7Kl8kJFhDnR54/edit?usp=sharing)

# select a fish - given these screening values of mercury in our data frame, how many of the fish/shellfish would each person be able to eat? 
# hint: 1ug=1PPM 

# how many salmon can you eat? 

sv_df_long$salmon = sv_df_long$screen_val/0.022

ggplot(sv_df_long) + geom_point(aes(y=salmon, x=serving_per_week))

