# Clearing environment
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(readxl)
library(pROC)
library(MASS)

#reading data
data <- read.csv("df_final_mortality.csv")
head(data)

#dataset info
dim(data)
summary(data)

#no missing values
colSums(is.na(data))

# Replace NANs in types of coffee with zeros, if the person didn't drink any coffee at all
data$TotalCoffeeIntake <- ifelse(is.na(data$TotalCoffeeIntake) & data$Coffee == 0, 0, data$TotalCoffeeIntake)
data$CaffeinatedStatus <- ifelse(is.na(data$CaffeinatedStatus) & data$Coffee == 0, "did not drink any coffee", data$CaffeinatedStatus)
#data$SugaryStatus <- ifelse(is.na(data$SugaryStatus) & data$Coffee == 0, 0, data$SugaryStatus)
#data$FattyStatus <- ifelse(is.na(data$FattyStatus) & data$Coffee == 0, 0, data$FattyStatus)
#data$MilkContainingStatus <- ifelse(is.na(data$MilkContainingStatus) & data$Coffee == 0, 0, data$MilkContainingStatus)



# Preprocess data
data <- data%>% 
  rename(Age_num = RIDAGEYR,
         Sex = RIAGENDR,
         Education = DMDEDUC,
         Race = RIDRETH1,
         MaritalStatus = DMDMARTL,
         PovertyIncome = INDFMPIR) %>%
  mutate(Sex = as.factor(Sex),
         Education = as.factor(Education),
         Race = as.factor(Race),
         MaritalStatus = as.factor(MaritalStatus),
         PAD = as.factor(PAD),
         Coffee = as.factor(Coffee),
         CaffeinatedStatus = as.factor(CaffeinatedStatus),
         SugaryStatus = as.factor(SugaryStatus),
         FattyStatus = as.factor(FattyStatus),
         MilkContainingStatus = as.factor(MilkContainingStatus),
         diabetes = as.factor(diabetes),
         hyperten = as.factor(hyperten)) %>%
  mutate(Sex = plyr::revalue(Sex, c("1" = "Male","2" = "Female")),
         Education = case_when(Education == "1" ~ "Less Than High School",
                               Education == "2" ~ "High School",
                               Education == "3" ~ "Some college or above",
                               Education == "7" | Education == "9" ~ "Incomplete"), #patients REFUSED to give into about the education or they DON'T KNOW
         Education = as.factor(Education),
         MaritalStatus = case_when(MaritalStatus == "1" ~ "Married",
                                   MaritalStatus == "5" ~ "Never Married",
                                   MaritalStatus =="2" | MaritalStatus == "3" | MaritalStatus == "4" | MaritalStatus == "6" ~ "Unmarried but have or had a partner"),
         MaritalStatus = as.factor(MaritalStatus),
         Race = case_when(Race == "1" ~ "Mexican American",
                          Race == "4" ~ "Non-Hispanic Black",
                          Race == "3" ~ "Non-Hispanic White",
                          Race == "2" | Race == "5" ~ "Other"),
         Race = as.factor(Race),
         Age_fct = case_when(Age_num < 65 ~ "Age < 65 years",
                             Age_num >= 65 ~ "Age >= 65 years"),
         Age_fct = as.factor(Age_fct),
         CaffeinatedStatus = case_when(CaffeinatedStatus == "0" ~ "Without Caffeine",
                                       CaffeinatedStatus == "1" ~ "With Caffeine",
                                       CaffeinatedStatus == "did not drink any coffee" ~ "Did not consume coffee"),
         CaffeinatedStatus = as.factor(CaffeinatedStatus),
         Disease = case_when(diabetes == "0" & hyperten == "0" ~ "0",
                             mortstat == "0" ~ "alive",
                             diabetes == "1" & hyperten == "0" ~ "1",
                             diabetes == "0" & hyperten == "1" ~ "1",
                             diabetes == "1" & hyperten == "1" ~ "2"),
         Disease = as.factor(Disease))
summary(data)

data_clean <- data[complete.cases(data$Education, data$MaritalStatus, data$PovertyIncome), ]

#define model with all predictors 
all_coffee <- glm(mortstat ~ Coffee + TotalCoffeeIntake+ Sex + Age_fct + Race + Education + MaritalStatus  + PovertyIncome, family=binomial(link='logit'), data=data_clean)
summary(all_coffee)

#perform backward and forward stepwise regression
both <- step(all_coffee, direction='both')

#view results of backward stepwise regression
both$anova

#view final model
exp(both$coefficients)
summary(both)

#model with caffeine
all_caffeine <- glm(mortstat ~ CaffeinatedStatus + TotalCoffeeIntake+ Sex + Age_fct + Race + Education  + PovertyIncome, family=binomial(link='logit'), data=data_clean)
summary(all_caffeine)

#perform backward and forward stepwise regression
both <- step(all_caffeine, direction='both')
summary(both)
