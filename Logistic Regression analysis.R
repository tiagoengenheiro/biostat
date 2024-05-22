# Logistic regression analysis


#libraries
#install.packages(c("tidyverse","ggplot2","psych","rstatix","readxl"))
#install.packages("ResourceSelection")
library(pROC)
library(tidyverse)
library(ggplot2)
#install.packages("kernlab")
library(kernlab)
#install.packages("ggcorrplot")
library(ggcorrplot)
library(psych)
#install.packages("ltm")
library(ltm)
library(rstatix)
library(readxl)
library(dplyr)
library(car)
library(ResourceSelection)
#install.packages("generalhoslem")
library(generalhoslem)
#install.packages('dplyr')


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


#creating the categorical variables according to the TABLE 1 in Journal of periodontology
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
         CaffeinatedStatus = as.factor(CaffeinatedStatus))

summary(data)

#----------------- Filtering for missing values

data <- data %>%
  filter(!is.na(PovertyIncome))

summary(data)

#--------------------------------------- Logistic regression

############# CoffeeConsumption

# Model 0 (Unadjustified): Only Coffee


model_1_0 <- glm(mortstat ~ Coffee, family = binomial(), data = data)
summary(model_1_0)
bic_value <- BIC(model_1_0)
bic_value

# ---Model 1: Model 0 with Age (Categorical), Sex and Race

model_1_1 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race, family = binomial(), data = data)
summary(model_1_1)
bic_value <- BIC(model_1_1)
print(paste("BIC:", bic_value))


# ------Model 2:  Model1 + poverty_income

model_1_2 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race + PovertyIncome, family = binomial(), data = data)
summary(model_1_2)
bic_value <- BIC(model_1_2)
print(paste("BIC:", bic_value))


############# Caffeinated Coffee

# Model 2_0:
model_2_0 <- glm(mortstat ~ CaffeinatedStatus, family = binomial(), data = data)
summary(model_2_0)
bic_value <- BIC(model_2_0)
print(paste("BIC:", bic_value))


# ---Model 2_1: Model 0 with Age_fct, Sex and Race
model_2_1 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race, family = binomial(), data = data)
summary(model_2_1)
bic_value <- BIC(model_2_4)
print(paste("BIC:", bic_value))



# ------Model 2_2:  Model1 + poverty_income
model_2_2 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race + PovertyIncome, family = binomial(), data = data)
summary(model_2_2)
bic_value <- BIC(model_2_5)
print(paste("BIC:", bic_value))
#--------------------------
