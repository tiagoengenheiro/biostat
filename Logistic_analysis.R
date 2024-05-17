# Logistic regression

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
                                       CaffeinatedStatus == "did not drink any coffee" ~ "Did not consume coffee")) 

summary(data)

#----------------- Filtering for missing values

data <- data %>%
  filter(!is.na(PovertyIncome))

summary(data)

# Model 1: Only Coffee
# ---Model 2: Model 1 with Age, Sex and Race
# ------Model 3:  Model2 + poverty_income
#---------Model 4: Model 2 but Age_fct
#------------Model 5: Model 3 but Agefct



# Variable one: Coffee: Patient consumed coffee (1) or not (0)
# Variable two: CaffeinatedStatus: Caffeinated Coffee(1) | Decaffeited Coffee(0)
# Variable 3: Integration of TotalCoffeeIntake and Coffee

#--------1----------Coffee:


# Model 1_1: Only Coffee
model_1_1 <- glm(mortstat ~ Coffee, family = binomial(), data = data)
summary(model_1_1)
######-------Testing model goodness

#-------------------
# ROC/AUC
# Generate predicted probabilities
model_1_1pp <- predict(model_1_1, type = "response")
model_1_1roc <- roc(data$mortstat, model_1_1pp)
plot(model_1_1roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_1_1auc <- auc(model_1_1roc)
print(paste("AUC:", model_1_1auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_1_1))
logitgof(data$mortstat, fitted(model_1_1), g = 5)
# Cant perform hoslem.test
# Residuals
residuals <- residuals(model_1_1, type = "deviance")
plot(residuals)
#--------------------------
##----------------------
#AIC: 504.24
#AUC: 0.573912483912484
########################################
# ---Model 1_2: Model 1 with Age, Sex and Race
model_1_2 <- glm(mortstat ~ Coffee + Age_num + Sex + Race, family = binomial(), data = data)
summary(model_1_2)
#-------------------
# ROC/AUC
# Generate predicted probabilities
model_1_2pp <- predict(model_1_2, type = "response")
model_1_2roc <- roc(data$mortstat, model_1_2pp)
plot(model_1_2roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_1_2auc <- auc(model_1_2roc)
print(paste("AUC:", model_1_2auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_1_2))
# Residuals
residuals <- residuals(model_1_2, type = "deviance")
plot(residuals)
#--------------------------
##----------------------
#AIC: 359.71
#AUC: 0.865598455598456
#Hosmer:
#X-squared = 5.1791, df = 8, p-value = 0.7383

# ------Model 1_3:  Model2 + poverty_income
model_1_3 <- glm(mortstat ~ Coffee + Age_num + Sex + Race + PovertyIncome, family = binomial(), data = data)
summary(model_1_3)
# ROC/AUC
# Generate predicted probabilities
model_1_3pp <- predict(model_1_3, type = "response")
model_1_3roc <- roc(data$mortstat, model_1_3pp)
plot(model_1_3roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_1_3auc <- auc(model_1_3roc)
print(paste("AUC:", model_1_3auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_1_3))
# Residuals
residuals <- residuals(model_1_3, type = "deviance")
plot(residuals)
#--------------------------
##----------------------------------
#AIC: 348.68
#AUC: 0.875444015444015
#Hosmer: X-squared = 6.8582, df = 8, p-value = 0.552
#
#############################################################
# ---Model 1_4: Model 1 with Age_fct, Sex and Race
model_1_4 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race, family = binomial(), data = data)
summary(model_1_4)
#############################################
model_1_4pp <- predict(model_1_4, type = "response")
model_1_4roc <- roc(data$mortstat, model_1_4pp)
plot(model_1_4roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_1_4auc <- auc(model_1_4roc)
print(paste("AUC:", model_1_4auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_1_4))
# Residuals
residuals <- residuals(model_1_4, type = "deviance")
plot(residuals)
#--------------------------
##----------------------------------
#AIC: 398.81
#AUC: 0.818944658944659"
#Hosmer:data:  data$mortstat, fitted(model_1_4)
#X-squared = 6.286, df = 6, p-value = 0.3919
#
# ------Model 1_5:  Model2 + poverty_income
model_1_5 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race + PovertyIncome, family = binomial(), data = data)
summary(model_1_5)
############################################### PABAIGTI SITA
model_1_5pp <- predict(model_1_5, type = "response")
model_1_5roc <- roc(data$mortstat, model_1_5pp)
plot(model_1_5roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_1_5auc <- auc(model_1_5roc)
print(paste("AUC:", model_1_5auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_1_5))
# Residuals
residuals <- residuals(model_1_5, type = "deviance")
plot(residuals)
#--------------------------
##----------------------------------
#AIC: 388.01
#AUC: 0.832445302445302
#Hosmer:data:  X-squared = 4.3374, df = 8, p-value = 0.8255 
#

model_1_6 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race + PovertyIncome + hyperten + diabetes, family = binomial(), data = data)
summary(model_1_6)

#--------2----------CaffeinatedStatus


# Model 2_1: Only Coffee
model_2_1 <- glm(mortstat ~ CaffeinatedStatus, family = binomial(), data = data)
summary(model_2_1)
######################################
model_2_1pp <- predict(model_2_1, type = "response")
model_2_1roc <- roc(data$mortstat, model_2_1pp)
plot(model_2_1roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_2_1auc <- auc(model_2_1roc)
print(paste("AUC:", model_2_1auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_2_1))
logitgof(data$mortstat, fitted(model_2_1))
# Residuals
residuals <- residuals(model_2_1, type = "deviance")
plot(residuals)
#--------------------------
##----------------------------------
#AIC: 501.97
#"AUC: 0.598301669545193"
#Hosmer:data:  
#
#data:  data$mortstat, fitted(model_2_1)
#X-squared = 5.9789e-18, df = 0, p-value < 2.2e-16
###################################################################


# ---Model 2_2: Model 1 with Age, Sex and Race
model_2_2 <- glm(mortstat ~ CaffeinatedStatus + Age_num + Sex + Race, family = binomial(), data = data)
summary(model_2_2 )
##################################################
model_2_2pp <- predict(model_2_2, type = "response")
model_2_2roc <- roc(data$mortstat, model_2_2pp)
plot(model_2_2roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_2_2auc <- auc(model_2_2roc)
print(paste("AUC:", model_2_2auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_2_2))
# Residuals
residuals <- residuals(model_2_2, type = "deviance")
plot(residuals)
#--------------------------
##----------------------------------
#AIC: 358.99
#"AUC: 0.868352638352638
#Hosmer:data:  
#data:  data$mortstat, fitted(model_2_2)
#X-squared = 5.4845, df = 8, p-value = 0.70484


# ------Model 2_3:  Model2 + poverty_income
model_2_3 <- glm(mortstat ~ CaffeinatedStatus + Age_num + Sex + Race + PovertyIncome, family = binomial(), data = data)
summary(model_2_3)

model_2_3pp <- predict(model_2_3, type = "response")
model_2_3roc <- roc(data$mortstat, model_2_3pp)
plot(model_2_3roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_2_3auc <- auc(model_2_3roc)
print(paste("AUC:", model_2_3auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_2_3))
# Residuals
residuals <- residuals(model_2_3, type = "deviance")
plot(residuals)
#--------------------------
##----------------------------------
#AIC: 346.83
#"AUC: 0.877425997425997
#Hosmer:X-squared = 5.8364, df = 8, p-value = 0.6656



# ---Model 2_4: Model 1 with Age_fct, Sex and Race
model_2_4 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race, family = binomial(), data = data)
summary(model_2_4)

model_2_4pp <- predict(model_2_4, type = "response")
model_2_4roc <- roc(data$mortstat, model_2_4pp)
plot(model_2_4roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_2_4auc <- auc(model_2_4roc)
print(paste("AUC:", model_2_4auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_2_4))
# Residuals
residuals <- residuals(model_2_4, type = "deviance")
plot(residuals)
#--------------------------
##----------------------------------
#AIC: 396.99
#"AUC: 0.825302445302445
#Hosmer:X-squared = 5.0243, df = 7, p-value = 0.657



# ------Model 2_5:  Model4 + poverty_income
model_2_5 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race + PovertyIncome, family = binomial(), data = data)
summary(model_2_5)


model_2_5pp <- predict(model_2_5, type = "response")
model_2_5roc <- roc(data$mortstat, model_2_5pp)
plot(model_2_5roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_2_5auc <- auc(model_2_5roc)
print(paste("AUC:", model_2_5auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_2_5))
# Residuals
residuals <- residuals(model_2_5, type = "deviance")
plot(residuals)
#--------------------------
##----------------------------------
#AIC: 385.28
#"AUC: 0.837644787644788
#X-squared = 12.365, df = 8, p-value = 0.1356



# ------Model 2_6:  Model4 + diseases
model_2_6 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race + PovertyIncome + hyperten + diabetes, family = binomial(), data = data)
summary(model_2_6)


model_2_6pp <- predict(model_2_6, type = "response")
model_2_6roc <- roc(data$mortstat, model_2_6pp)
plot(model_2_6roc, main = "ROC Curve", col = "blue", lwd = 2)
# Calculate AUC
model_2_5auc <- auc(model_2_5roc)
print(paste("AUC:", model_2_5auc))
# Hosmer_lemeshow
hoslem.test(data$mortstat, fitted(model_2_5))
# Residuals
residuals <- residuals(model_2_5, type = "deviance")
plot(residuals)
#--------------------------
##----------------------------------
#AIC: 385.28
#"AUC: 0.837644787644788
#X-squared = 12.365, df = 8, p-value = 0.1356


#----------------------------------Removed variable
#######################################################
#--------3----------Coffee+TotalCoffeeIntake

# Model 1_1: Only Coffee
model_3_1 <- glm(mortstat ~ Coffee + TotalCoffeeIntake, family = binomial(), data = data)
summary(model_3_1)
# ---Model 1_2: Model 1 with Age, Sex and Race
model_3_2 <- glm(mortstat ~ Coffee + TotalCoffeeIntake + Age_num + Sex + Race, family = binomial(), data = data)
summary(model_3_2 )
# ------Model 1_3:  Model2 + poverty_income
model_3_3 <- glm(mortstat ~ Coffee + TotalCoffeeIntake + Age_num + Sex + Race + PovertyIncome, family = binomial(), data = data)
summary(model_3_3)
# ---Model 1_4: Model 1 with Age, Sex and Race
model_2_4 <- glm(mortstat ~ Coffee + TotalCoffeeIntake + Age_fct + Sex + Race, family = binomial(), data = data)
summary(model_2_4)
# ------Model 1_5:  Model2 + poverty_income
model_2_5 <- glm(mortstat ~ Coffee + TotalCoffeeIntake + Age_fct + Sex + Race + PovertyIncome, family = binomial(), data = data)
summary(model_2_5)




