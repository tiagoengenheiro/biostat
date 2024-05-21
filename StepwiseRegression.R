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
  rename(Age_num = RIDAGEYR,  # Renaming columns for readability
         Sex = RIAGENDR,
         Education = DMDEDUC,
         Race = RIDRETH1,
         MaritalStatus = DMDMARTL,
         PovertyIncome = INDFMPIR) %>%
  mutate(Sex = as.factor(Sex),  # Converting columns to factors where appropriate
         Education = as.factor(Education),
         Race = as.factor(Race),
         MaritalStatus = as.factor(MaritalStatus),
         PAD = as.factor(PAD),
         Coffee = as.factor(Coffee),
         CaffeinatedStatus = as.factor(CaffeinatedStatus),
         SugaryStatus = as.factor(SugaryStatus),
         FattyStatus = as.factor(FattyStatus),
         MilkContainingStatus = as.factor(MilkContainingStatus)) %>%
  mutate(Sex = plyr::revalue(Sex, c("1" = "Male","2" = "Female")),  # Renaming factor levels for readability
         Education = case_when(Education == "1" ~ "Less Than High School",
                               Education == "2" ~ "High School",
                               Education == "3" ~ "Some college or above",
                               Education == "7" | Education == "9" ~ "Incomplete"),  # Handling special cases in education
         Education = as.factor(Education),  # Ensuring the column is a factor
         MaritalStatus = case_when(MaritalStatus == "1" ~ "Married",
                                   MaritalStatus == "5" ~ "Never Married",
                                   MaritalStatus =="2" | MaritalStatus == "3" | MaritalStatus == "4" | MaritalStatus == "6" ~ "Unmarried but have or had a partner"),  # Handling special cases in marital status
         MaritalStatus = as.factor(MaritalStatus),  # Ensuring the column is a factor
         Race = case_when(Race == "1" ~ "Mexican American",
                          Race == "4" ~ "Non-Hispanic Black",
                          Race == "3" ~ "Non-Hispanic White",
                          Race == "2" | Race == "5" ~ "Other"),  # Handling special cases in race
         Race = as.factor(Race),  # Ensuring the column is a factor
         Age_fct = case_when(Age_num < 65 ~ "Age < 65 years",
                             Age_num >= 65 ~ "Age >= 65 years"),  # Creating a categorical variable for age
         Age_fct = as.factor(Age_fct)) %>%
  filter(LEXLABPI < 1.40 & LEXRABPI < 1.40)  # Filtering out certain rows based on conditions

# Remove rows with missing values in MaritalStatus or PovertyIncome
data_clean <- data[complete.cases(data$MaritalStatus, data$PovertyIncome), ]

# Define intercept-only model
intercept_only <- lm(mortstat ~ 1, data=data_clean)  # Model with only the intercept

# Define model with all predictors
all <- lm(mortstat ~ Coffee + Sex + Age_num + Race + Education + MaritalStatus + PovertyIncome + permth_int, data=data_clean)  # Full model with all predictors

# Perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=1)  # Forward selection starting from intercept-only model

# View results of forward stepwise regression
forward$anova  # ANOVA table of the stepwise selection process

# View final model
forward$coefficients  # Coefficients of the final model
summary(forward)  # Summary of the final model

#########################

# Perform backward stepwise regression
backward <- step(all, direction='backward', scope=formula(all), trace=1)  # Backward selection starting from the full model

# View results of backward stepwise regression
backward$anova  # ANOVA table of the stepwise selection process

# View final model
backward$coefficients  # Coefficients of the final model
summary(backward)  # Summary of the final model
