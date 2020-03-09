library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)



m <- read.csv('/Users/miana/Downloads/master.csv',header = T)
view(m)

pp <- m %>%
  select(pp_2014,pp_2015,pp_2016)

pp_results <- apply(pp,1,function(x) names(which.max(table(x))))
view(pp_results)
new_m$State<- m$State
new_m <- data.frame(age = rowMeans(subset(m, select = (c(mean_age_2014,mean_age_2015,mean_age_2016)), na.rm = TRUE)))
new_m$pp = pp_results 
new_m$prop_male = rowMeans(subset(m, select = c(prop_male_2014, prop_male_2015 , prop_male_2016)),na.rm = TRUE)

new_m$ps = rowMeans(subset(m, select = c(ps_2014 , ps_2015 , ps_2016)), na.rm = T)
new_m$pop_prop_ps = rowMeans(subset(m,select = c(pop_prop_ps_2014 , pop_prop_ps_2015 , pop_prop_ps_2016)),na.rm = T)
new_m$beverage = rowMeans(subset(m, select = c(X2014_beverages , X2015_beverages , X2016_beverages)), na.rm = T)
new_m$rev_ratio = rowMeans(subset(m, select = c(X2014_rev_ratio , X2015_rev_ratio , X2016_rev_ratio)), na.rm = T)

                                  
                                  
new_m$public_welfare = rowMeans(subset(m, select = c(X2014_Public_Welfare , X2015_Public_Welfare , X2016_Public_Welfare)),na.rm = T)
new_m$hospitals_expense = rowMeans(subset(m, select = c(X2014_Hospitals_Expense , X2015_Hospitals_Expense , 
                                                        X2016_Hospitals_Expense)), na.rm = T)
new_m$health_expense = rowMeans(subset(m, select = c(X2014_Health_Expense , X2015_Health_Expense , 
                                                     X2016_Health_Expense)), na.rm = T)
                                       
new_m$police_expense = rowMeans(subset(m, select = c(X2014_Police_Expense , X2015_Police_Expense , 
                                                     X2016_Police_Expense)), na.rm = T)
new_m$correction_expense = rowMeans(subset(m, select = c(X2014_Correction_Expense , X2015_Correction_Expense ,
                                     X2016_Correction_Expense)), na.rm = T)
new_m$population_density = rowMeans(subset(m, select = c(X2014_Population_Density , X2015_Population_Density , 
                                                        X2016_Population_Density)), na.rm = T)


colnames(new_m)
colnames(m)

new_m$legalization = ifelse(new_m$State == 'California' | new_m$State =='Massachusetts' | 
  new_m$State == 'Neveda' | new_m$State =='Maine' | new_m$State =='Colorado'|
    new_m$State =='Washington' |new_m$State =='Alaska'|new_m$State =='Oregon', 1, 0)

view(new_m)
new_m <- new_m %>%
  select(State,age,pp:legalization)
write.csv(new_m,'/Users/miana/Downloads/clean_m.csv')
new_m <- read.csv("/Users/miana/Downloads/clean_m.csv",header = T)

install.packages('caret')
library(caret)
normalized_m
colnames(new_m)
normalized_m<- scale(new_m[,c(3,5:17)])
normalized_m <-cbind(new_m[,c(1,2,4)],normalized_m)
write.csv(normalized_m,'/Users/miana/Downloads/normalized_m.csv')


install.packages("corrplot")
library(corrplot)
colnames(normalized_m)
mc_data <- normalized_m[,c(4:15)]
corrplot(round(cor(mc_data),2))


library(MatchIt)
colnames(new_m)

normalized_m <- read.csv('/Users/miana/Downloads/normalized_m.csv', header = T)
normalized_m$legalization = ifelse(normalized_m$State == 'California' | normalized_m$State =='Massachusetts' | 
                                     normalized_m$State == 'Neveda' | normalized_m$State =='Maine' |
                                     normalized_m$State == 'Colorado' |normalized_m$State == 'Washington' |normalized_m$State == 'Alaska'|
                                     normalized_m$State == 'Oregon', 1, 0)

# drop states
#normalized_m <- normalized_m[normalized_m$State != 'Colorado' &normalized_m$State != 'Washington'&normalized_m$State != 'Alaska'&
#                               normalized_m$State != 'Oregon',]


summary(glm(legalization ~ age+prop_male + as.factor(pp)+rev_ratio+
              beverage + population_density, data = normalized_m, family = "binomial"))

PScore = glm(legalization ~ age+prop_male + as.factor(pp)+rev_ratio+
               beverage + population_density, data = normalized_m, family = "binomial")$fitted.values
new_m$PScore = PScore
ggplot(new_m, aes(x = PScore, color = factor(legalization))) +
  geom_density() 

match_output <- matchit(legalization ~ age+prop_male + as.factor(pp)+rev_ratio+
                          beverage+ population_density, data = normalized_m, method = 'nearest', distance = "logit", caliper = 0.05, replace = F, ratio = 1)
summary(match_output)
data_match = match.data(match_output)

ggplot(data_match, aes(x = PScore, color = factor(legalization))) +
  geom_density()
