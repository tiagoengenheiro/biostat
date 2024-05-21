# Clearing environment
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(readxl)
library(pROC)
library(MASS)

# Read data
data <- read.csv("df_final_mortality.csv")

# Preprocess data
data <- data %>% 
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
         MilkContainingStatus = as.factor(MilkContainingStatus)) %>%
  mutate(Sex = plyr::revalue(Sex, c("1" = "Male","2" = "Female")),
         Education = case_when(Education == "1" ~ "Less Than High School",
                               Education == "2" ~ "High School",
                               Education == "3" ~ "Some college or above",
                               Education == "7" | Education == "9" ~ "Incomplete"), 
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
         Age_fct = as.factor(Age_fct)) %>%
  filter(LEXLABPI < 1.40 & LEXRABPI < 1.40)

data_clean <- data[complete.cases(data$MaritalStatus, data$PovertyIncome), ]

#define intercept-only model
intercept_only <- glm(mortstat ~ 1, family=binomial, data=data_clean)

#define model with all predictors 
all <- glm(mortstat ~ Coffee + Sex + Age_fct + Race + Education + MaritalStatus + PovertyIncome, family=binomial(link='logit'), data=data_clean)
summary(all)
#perform forward stepwise regression
forward <- step(intercept_only, direction='forward')

#view results of forward stepwise regression
forward$anova

#view final model
forward$coefficients
summary(forward)

#########################

#perform backward stepwise regression
backward <- step(all, direction='backward')

#view results of backward stepwise regression
backward$anova

#view final model
backward$coefficients
summary(backward)

#########################

#perform backward and forward stepwise regression
both <- step(all, direction='both')

#view results of backward stepwise regression
both$anova

#view final model
both$coefficients
summary(both)


