## New analysis with new data
#clearing environment
rm(list = ls())

#libraries
#install.packages(c("tidyverse","ggplot2","psych","rstatix","readxl"))
library("tidyverse")
library("ggplot2")
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

# Install readr explicitly
install.packages("readr")

# You might also consider updating the entire tidyverse to ensure compatibility
install.packages("tidyverse")

install.packages("timechange")



#reading data
data <- read.csv("df_final_mortality_covariates.csv")
head(data)

#dataset info
dim(data)
summary(data)

#no missing values
colSums(is.na(data))

# This is the covariates I got from him:
# - Hiperlipidmia
# - Smoking
# - Diabetes (use glicemia as proxy)
# - High BP


# SmokingStatus: Never Smoked (0), Former Smoker (1), Current Smoker(2)
# Hypertension: Yes (1) No (0)
# Diabetes: Yes (1) No (0)
# Hyperlipidemia: Yes (1) No (0)
# DIABETES
# 1 if:
#   Doctor told that patient has diabetes (Self-reported)
# Patient is taking insulin now (Self-reported)
# Patient takes pills to lower blood sugar (Self-reported)
# Fasting Glucose values >=126 mg/dL
# Non-Fasting Glucose values >= 200 mg/dL
# diabetes listed has multiple cause of death
# 0 otherwise

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
         Hyperlipidemia = as.factor(Hyperlipidemia),
         SmokingStatus = as.factor(SmokingStatus),
         MilkContainingStatus = as.factor(MilkContainingStatus)) %>%
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
         SmokingStatus = case_when(SmokingStatus == "0" ~ "Never Smoked",
                          SmokingStatus == "1" ~ "Former Smoker",
                          SmokingStatus == "2" ~ "Current Smoker"),
         SmokingStatus = as.factor(SmokingStatus),
         Hypertension = case_when(Hypertension == "0" ~ "No",
                                  Hypertension == "1" ~ "Yes"),
         Hyperlipidemia = case_when(Hyperlipidemia == "0" ~ "No",
                                    Hyperlipidemia == "1" ~ "Yes"),
         Diabetes = case_when(Diabetes == "0" ~ "No",
                              Diabetes == "1" ~ "Yes"),
         Age_fct = case_when(Age_num < 65 ~ "Age < 65 years",
                             Age_num >= 65 ~ "Age >= 65 years"),
         Age_fct = as.factor(Age_fct)) %>%
  filter(LEXLABPI < 1.40 & LEXRABPI <1.40) #filtering according to the first paragraph in the Results section in the "Secondary prevention..." article

head(data)




# logistic_regression <- function(dep_var, ind_var, cov1 = NULL, cov2 = NULL, cov3 = NULL, cov4 = NULL, cov5 = NULL, cov6 = NULL, cov7 = NULL, cov8 = NULL) {
#   glm(dep_var ~ ind_var + cov1 + cov2 + cov3 + cov4 + cov5 + cov6 + cov7 + cov8, family = binomial(), data = data)
# }

#### model 0:
model0 <- glm(mortstat ~ Coffee, family = binomial(), data = data)
summary(model0)

#### model 1:

model1 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race,, family = binomial(), data = data)
summary(model1)

#### model 2:

model2 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race + PovertyIncome + Education, family = binomial(), data = data)
summary(model2)

### exploring covariates:
# - Hiperlipidmia
model_hyperlipidemia <- glm(mortstat ~ Coffee + Hyperlipidemia, family = binomial(), data = data)
summary(model_hyperlipidemia)

# - Smoking
model_smoking <- glm(mortstat ~ Coffee + SmokingStatus, family = binomial(), data = data)
summary(model_smoking)

# - Diabetes (use glicemia as proxy)

model_diabetes <- glm(mortstat ~ Coffee + Diabetes, family = binomial(), data = data)
summary(model_diabetes)

# - High BP

model_hypertension <- glm(mortstat ~ Coffee + Hypertension, family = binomial(), data = data)
summary(model_hypertension)



################## Stepwise regression
############################### performing model 3 with all covariates:


model00 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race + 
                 PovertyIncome + Education + SmokingStatus + Hypertension
               + Hyperlipidemia + Diabetes, family = binomial(), data = data)
summary(model00)

# as we can see, education level is not significant at all

model01 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race + 
                 PovertyIncome + SmokingStatus + Hypertension
               + Hyperlipidemia + Diabetes, family = binomial(), data = data)
summary(model01)

# Race as well unless its white. hyperlipidemia as well:

model02 <- glm(mortstat ~ Coffee + Age_fct + Sex + Race + 
                 PovertyIncome + SmokingStatus + Hypertension
                + Diabetes, family = binomial(), data = data)
summary(model02)


# doing the same with caffeinated status

#### model 0:
model0 <- glm(mortstat ~ CaffeinatedStatus, family = binomial(), data = data)
summary(model0)

#### model 1:

model1 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race,, family = binomial(), data = data)
summary(model1)

#### model 2:

model2 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race + PovertyIncome + Education, family = binomial(), data = data)
summary(model2)

### exploring covariates:
# - Hiperlipidmia
model_hyperlipidemia <- glm(mortstat ~ CaffeinatedStatus + Hyperlipidemia, family = binomial(), data = data)
summary(model_hyperlipidemia)

# - Smoking
model_smoking <- glm(mortstat ~ CaffeinatedStatus + SmokingStatus, family = binomial(), data = data)
summary(model_smoking)

# - Diabetes (use glicemia as proxy)

model_diabetes <- glm(mortstat ~ CaffeinatedStatus + Diabetes, family = binomial(), data = data)
summary(model_diabetes)

# - High BP

model_hypertension <- glm(mortstat ~ CaffeinatedStatus + Hypertension, family = binomial(), data = data)
summary(model_hypertension)



################## Stepwise regression
############################### performing model 3 with all covariates:


model00 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race + 
                 PovertyIncome + Education + SmokingStatus + Hypertension
               + Hyperlipidemia + Diabetes, family = binomial(), data = data)
summary(model00)

# as we can see, education level is not significant at all

model01 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race + 
                 PovertyIncome + SmokingStatus + Hypertension
               + Hyperlipidemia + Diabetes, family = binomial(), data = data)
summary(model01)

# Race as well unless its white (this one is close to). hyperlipidemia as well:

model02 <- glm(mortstat ~ CaffeinatedStatus + Age_fct + Sex + Race + 
                 PovertyIncome + SmokingStatus + Hyperlipidemia
               + Diabetes, family = binomial(), data = data)
summary(model02)