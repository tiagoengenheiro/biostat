rm(list = ls())

#install.packages("survival")
#install.packages("survminer")
#install.packages("gtsummary")
#remove.packages("dplyr")
#install.packages("dplyr")
library("tidyverse")
library("ggplot2")
library("readxl")
library(survival)
library(survminer)
library(gtsummary)
#install.packages("ggcorrplot")
library(ggcorrplot)
library(psych)
#install.packages("ltm")
library("ltm")
library(rstatix)

##################################################################################################
#-----1-------Preliminary analysis
##################################################################################################

#reading data
data <- read.csv("df_final_mortality.csv")
head(data)

#dataset info
dim(data)
summary(data)

#no missing values
colSums(is.na(data))

# Replace NANs in types of coffee with zeros, if the person didn't drink any coffee at all
#data$TotalCoffeeIntake <- ifelse(is.na(data$TotalCoffeeIntake) & data$Coffee == 0, 0, data$TotalCoffeeIntake)
data$CaffeinatedStatus <- ifelse(is.na(data$CaffeinatedStatus) & data$Coffee == 0, "did not drink any", data$CaffeinatedStatus)
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
         Age_fct = as.factor(Age_fct)) 

summary(data)

#creating censored and non-censored data

data_cenz <- data %>%
  filter(mortstat == 0)
data_dead <- data %>%
  filter(mortstat == 1)
summary(data_cenz)
summary(data_dead)


data %>%
  ggplot(aes(x=Coffee, y=permth_exm)) + 
  geom_boxplot(size = 0.6, notch = F) + 
  xlab("Coffee status") +
  ylab("Survival time") +
  theme_bw() +  
  geom_jitter(aes(Coffee,permth_exm),#adding dottes
              position=position_jitter(width=0.3,height=0),
              alpha=0.5,
              size=1,
              show.legend=T) +
  scale_colour_gradient(low = "hotpink", high = "#56B1F7") +
  stat_summary(fun=mean, geom="point", shape=4, size=4,color = "blue") 
data %>% anova_test(permth_exm ~ Coffee)

##-------------------------------- Kaplan-Meier model
  #Kaplan-Meier model is good only for predictions (NOT the HR!!!)
# Fit survival data using the Kaplan-Meier method
model0 <- survfit(Surv(time = permth_exm, event = mortstat) ~ Coffee, data = data, type = "kaplan-meier")

ggsurvplot(model0, data = data, pval = TRUE, conf.int=TRUE,xlim = c(0,250), surv.median.line = "hv")

#testing if the two survival functions are significantly different: LOG-RANK TEST
  #H0: survival in the two groups is the same
  #Ha: survival is not the same

survdiff(Surv(time = permth_exm, event = mortstat) ~ Coffee, data = data)

##-------------------------------- Cox's PH regression model
  #Cox is good only for the HR (not for predictions!!!)



model1 <- coxph(Surv(permth_exm, mortstat) ~ Coffee, data = data) #the same as model0, but extracting the HR
summary(model1)


model2 <- coxph(Surv(permth_exm, mortstat) ~ Coffee+Sex+Race+Age_fct, data = data[complete.cases(data[, c("PovertyIncome")]), ])
summary(model2)

model3 <- coxph(Surv(permth_exm, mortstat) ~ Coffee+Sex+Race+Age_fct+PovertyIncome, data = data)
summary(model3)

#Test the Proportional Hazards Assumption of a Cox Regression
  #H0... HR are proportional
  #Ha... HR are not
cox.zph(model1) 
cox.zph(model2) 
cox.zph(model3) 


#checking the linearity 
plot(predict(model2), residuals(model2,type = "martingale"),xlab = "fitted values",ylab = "Residuals",las = 1)
abline(h=0)
lines(smooth.spline(predict(model2), residuals(model3,type = "martingale")),col = "red")

plot(predict(model2), residuals(model2,type = "deviance"),xlab = "fitted values",ylab = "Residuals",las = 1)
abline(h=0)
lines(smooth.spline(predict(model2), residuals(model3,type = "deviance")),col = "red")

#checking the linearity 
plot(predict(model3), residuals(model3,type = "martingale"),xlab = "fitted values",ylab = "Residuals",las = 1)
abline(h=0)
lines(smooth.spline(predict(model3), residuals(model3,type = "martingale")),col = "red")

plot(predict(model3), residuals(model3,type = "deviance"),xlab = "fitted values",ylab = "Residuals",las = 1)
abline(h=0)
lines(smooth.spline(predict(model3), residuals(model3,type = "deviance")),col = "red")

#----comparing nested models:
anova(model2,model3,test = "LRT")
AIC(model2,model3)
BIC(model2,model3)
#the higher the better
summary(model2)$concordance[1]
summary(model3)$concordance[1]

#if I use Coffee and Caffeinated status we would get NA values, because the variables are correlated
model4 <- coxph(Surv(permth_exm, mortstat) ~ Sex+Race+PovertyIncome+Age_fct+CaffeinatedStatus, data = data)
summary(model4)

#----comparing non-nested models:
AIC(model3,model4)
BIC(model3,model4)
#the higher the better
summary(model3)$concordance[1]
summary(model4)$concordance[1] 

#checking the linearity 
plot(predict(model4), residuals(model4,type = "martingale"),xlab = "fitted values",ylab = "Residuals",las = 1)
abline(h=0)
lines(smooth.spline(predict(model4), residuals(model3,type = "martingale")),col = "red")



##-------------------------------- SURVIVAL REGRESSION (PARAMETRIC MODELS)

# Fit different survival models
exp_model <- survreg(Surv(permth_int, mortstat) ~ Sex+Race+PovertyIncome+Age_fct+CaffeinatedStatus, data = data, dist = "exponential")
weibull_model <- survreg(Surv(permth_int, mortstat) ~ Sex+Race+PovertyIncome+Age_fct+CaffeinatedStatus, data = data, dist = "weibull")
lognormal_model <- survreg(Surv(permth_int, mortstat) ~ Sex+Race+PovertyIncome+Age_fct+CaffeinatedStatus, data = data, dist = "lognormal")
loglogistic_model <- survreg(Surv(permth_int, mortstat) ~ Sex+Race+PovertyIncome+Age_fct+CaffeinatedStatus, data = data, dist = "loglogistic")
gauss_model <- survreg(Surv(permth_int, mortstat) ~ Sex+Race+PovertyIncome+Age_fct+CaffeinatedStatus, data = data, dist = "gaussian")
loggauss_model <- survreg(Surv(permth_int, mortstat) ~ Sex+Race+PovertyIncome+Age_fct+CaffeinatedStatus, data = data, dist = "loggaussian")
rayleigh_model <- survreg(Surv(permth_int, mortstat) ~ Sex+Race+PovertyIncome+Age_fct+CaffeinatedStatus, data = data, dist = "rayleigh")
# Compare models using AIC
AIC(exp_model)
AIC(weibull_model)
AIC(lognormal_model)
AIC(loglogistic_model)
AIC(gauss_model)
AIC(loggauss_model)
AIC(rayleigh_model)
