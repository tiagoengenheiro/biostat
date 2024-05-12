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
data$TotalCoffeeIntake <- ifelse(is.na(data$TotalCoffeeIntake) & data$Coffee == 0, 0, data$TotalCoffeeIntake)
#data$CaffeinatedStatus <- ifelse(is.na(data$CaffeinatedStatus) & data$Coffee == 0, 0, data$CaffeinatedStatus)
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

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = data$permth_exm, event = data$mortstat)
fit1 <- survfit(surv_object ~ Coffee, data = data)

ggsurvplot(fit1, data = data, pval = TRUE, conf.int=TRUE,xlim = c(0,250), surv.median.line = "hv")


#Cox's regression model

model1 <- coxph(Surv(permth_int, mortstat) ~ Coffee, data = data)
summary(model1)
model2 <- coxph(Surv(permth_int, mortstat) ~ Coffee+Sex+Race+Age_fct, data = data)
summary(model2)
model3 <- coxph(Surv(permth_int, mortstat) ~ Coffee+Sex+Race+PovertyIncome+Age_fct, data = data)
summary(model3)

# Obtain Likelihood Ratio Test Statistic
# -2 * (logLik(model1) - logLik(model2))
lrt_statistic <- 2 * (logLik(model2) - logLik(model3))

# Perform Likelihood Ratio Test
# pchisq(likelihood_ratio_test_statistic, df = difference_in_parameters, lower.tail = FALSE)
p_value <- pchisq(lrt_statistic, df = (length(coef(model3)) - length(coef(model2))), lower.tail = FALSE)

# Assess Significance
if (p_value < 0.05) {
  print("Reject null hypothesis: More complex model is significantly better.")
} else {
  print("Fail to reject null hypothesis: Simpler model is sufficient.")
}
