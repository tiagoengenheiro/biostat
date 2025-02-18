#clearing environment
rm(list = ls())


#libraries
#install.packages(c("tidyverse","ggplot2","psych","rstatix","readxl"))
library("tidyverse")
library("ggplot2")
#install.packages("kernlab")
library("kernlab")
#install.packages("ggcorrplot")
library(ggcorrplot)
library(psych)
#install.packages("ltm")
library(ltm)
library(rstatix)
library(readxl)


##################################################################################################
#-----1-------Preliminary analysis
##################################################################################################

#reading data
data <- read.csv("df_final_mortality_covariates.csv")
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
data<- data%>% 
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
         Hypertension = as.factor(Hypertension),
         Hyperlipidemia = case_when(Hyperlipidemia == "0" ~ "No",
                                    Hyperlipidemia == "1" ~ "Yes"),
         Hyperlipidemia = as.factor(Hyperlipidemia),
         Diabetes = case_when(Diabetes == "0" ~ "No",
                              Diabetes == "1" ~ "Yes"),
         Diabetes = as.factor(Diabetes),
         Age_fct = case_when(Age_num < 65 ~ "Age < 65 years",
                             Age_num >= 65 ~ "Age >= 65 years"),
         Age_fct = as.factor(Age_fct)) %>%
  filter(LEXLABPI < 1.40 & LEXRABPI <1.40) #filtering according to the first paragraph in the Results section in the "Secondary prevention..." article


summary(data)

#-------use the code before this---------------------------------------------------------------


#####################-------------------NUMERICALS:

#AGE
cat("Quantiles for AGE, TOTAL: ", quantile(data$Age_num, na.rm = TRUE), "\n")
cat("Quantiles for AGE, mortstat =1: ", quantile(data$Age_num[data$mortstat == "1"], na.rm = TRUE), "\n")
cat("Quantiles for AGE, mortstat =0: ", quantile(data$Age_num[data$mortstat == "0"], na.rm = TRUE), "\n")

#PovertyIncome
cat("Quantiles for PovertyIncome, TOTAL: ", quantile(data$PovertyIncome, na.rm = TRUE), "\n")
cat("Quantiles for PovertyIncome, mortstat =1: ", quantile(data$PovertyIncome[data$mortstat == "1"], na.rm = TRUE), "\n")
cat("Quantiles for PovertyIncome, mortstat =0: ", quantile(data$PovertyIncome[data$mortstat == "0"], na.rm = TRUE), "\n")

#Left ankle-brachial pressure index
#cat("Quantiles for Left ankle-brachial pressure index, mortstat =1: ", quantile(data$LEXLABPI[data$mortstat == "1"], na.rm = TRUE), "\n")
#cat("Quantiles for Left ankle-brachial pressure index, mortstat =0: ", quantile(data$LEXLABPI[data$mortstat == "0"], na.rm = TRUE), "\n")

#Right ankle-brachial pressure index
#cat("Quantiles for Right ankle-brachial pressure index, mortstat =1: ", quantile(data$LEXRABPI[data$mortstat == "1"], na.rm = TRUE), "\n")
#cat("Quantiles for Right ankle-brachial pressure index, mortstat =0: ", quantile(data$LEXRABPI[data$mortstat == "0"], na.rm = TRUE), "\n")

#Total Coffee Intake
cat("Quantiles for Total Coffee Intake, TOTAL: ", quantile(data$TotalCoffeeIntake, na.rm = TRUE), "\n")
cat("Quantiles for Total Coffee Intake, mortstat =1: ", quantile(data$TotalCoffeeIntake[data$mortstat == "1"], na.rm = TRUE), "\n")
cat("Quantiles for Total Coffee Intake, mortstat =0: ", quantile(data$TotalCoffeeIntake[data$mortstat == "0"], na.rm = TRUE), "\n")

#Permth_int
cat("Quantiles for permth_int, TOTAL: ", quantile(data$permth_int, na.rm = TRUE), "\n")
cat("Quantiles for permth_int, mortstat =1: ", quantile(data$permth_int[data$mortstat == "1"], na.rm = TRUE), "\n")
cat("Quantiles for permth_int, mortstat =0: ", quantile(data$permth_int[data$mortstat == "0"], na.rm = TRUE), "\n")

#Permth_exm
cat("Quantiles for permth_exm, TOTAL: ", quantile(data$permth_exm, na.rm = TRUE), "\n")
cat("Quantiles for permth_exm, mortstat =1: ", quantile(data$permth_exm[data$mortstat == "1"], na.rm = TRUE), "\n")
cat("Quantiles for permth_exm, mortstat =0: ", quantile(data$permth_exm[data$mortstat == "0"], na.rm = TRUE), "\n")
################# Mann-Whitney U test


#AGE
mwu_result <- wilcox.test(data$Age_num ~ data$mortstat)
print(mwu_result)
#PovertyIncome
mwu_result <- wilcox.test(data$PovertyIncome ~ data$mortstat)
print(mwu_result)
#Left ankle-brachial pressure index
#mwu_result <- wilcox.test(data$LEXLABPI ~ data$mortstat)
#print(mwu_result)
#Right ankle-brachial pressure index
#mwu_result <- wilcox.test(data$LEXRABPI ~ data$mortstat)
#print(mwu_result)
#Total Coffee Intake
mwu_result <- wilcox.test(data$TotalCoffeeIntake ~ data$mortstat)
print(mwu_result)
#Permth_int
mwu_result <- wilcox.test(data$permth_int ~ data$mortstat)
print(mwu_result)
#Permth_exm
mwu_result <- wilcox.test(data$permth_exm ~ data$mortstat)
print(mwu_result)


#####################-------------------CATEGORICALS:
# pivot tables for catagoricals:

#mortstat - only 10% of the patients have the disease
df_pivot <- data %>% 
  summarise(n = n(),
            mortstat_yes = sum(mortstat=="1"),
            mortstat_no = sum(mortstat=="0"),
            mortstat_yes_percent = sum(mortstat=="1")/n,
            mortstat_no_percent = (sum(mortstat=="0"))/n
  )
df_pivot

#AGE - the distribution of AGE groups for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$Age_fct), ] %>%
  group_by(mortstat,Age_fct) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$Age_fct), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals


df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals

#SEX - the distribution of SEXes for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$Sex), ] %>%
  group_by(mortstat,Sex) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$Sex), ] %>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals

df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals

#RACE - the distribution of races for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$Race), ] %>%
  group_by(mortstat,Race) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$Race), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals

df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals


#MaritalStatus - the distribution of MaritalStatus groups for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$MaritalStatus), ] %>%
  group_by(mortstat,MaritalStatus) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$MaritalStatus), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals

df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals

#Education - the distribution of Education groups for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$Education), ] %>%
  group_by(mortstat,Education) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$Education), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals

df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals

#Coffee - the distribution of Coffee groups for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$Coffee), ] %>%
  group_by(mortstat,Coffee) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$Coffee), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals

df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals

#CaffeinatedStatus - the distribution of CaffeinatedStatus groups for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$CaffeinatedStatus), ] %>%
  group_by(mortstat,CaffeinatedStatus) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$CaffeinatedStatus), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals

df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals

#Diabetes - the distribution of Diabetes groups for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$Diabetes), ] %>%
  group_by(mortstat,Diabetes) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$Diabetes), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals


df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals

#Hypertension - the distribution of Hypertension groups for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$Hypertension), ] %>%
  group_by(mortstat,Hypertension) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$Hypertension), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals


df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals


#Hyperlipidemia - the distribution of Hyperlipidemia groups for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$Hyperlipidemia), ] %>%
  group_by(mortstat,Hyperlipidemia) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$Hyperlipidemia), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals


df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals

#SmokingStatus - the distribution of SmokingStatus groups for people having mortstat = 0, or mortstat = 1
#NAN's excluded
pivot_counts <- data[complete.cases(data$SmokingStatus), ] %>%
  group_by(mortstat,SmokingStatus) %>%
  summarise(count = n()) %>%
  ungroup()
pivot_counts 

pivot_totals <- data[complete.cases(data$SmokingStatus), ]%>%
  group_by(mortstat) %>%
  summarise(total_count = n())
pivot_totals


df_with_totals <- left_join(pivot_counts, pivot_totals, by = "mortstat")%>%
  mutate(percentage = count / total_count * 100)%>%
  pivot_wider(names_from = mortstat, values_from = percentage)
df_with_totals

#### CHI-SQUARED test for categoricals
#Null hypotesis: 
  #the null hypothesis (H0) that there is no association between the two categorical variables against the alternative hypothesis (H1) that there is an association between them.
#assumptions: 
  #Independence of the observations, 
  #Random sampling: The data used in the chi-squared test should ideally be obtained through a random sampling process. This ensures that the sample is representative of the population from which it is drawn.
  #the expected frequencies in each cell of the contingency table should ideally be greater than 5. When expected frequencies are too small, the chi-squared test may produce unreliable results. In cases where expected frequencies are very small, Fisher's exact test may be more appropriate.
  #No empty cells: There should be no empty cells in the contingency table. If any combination of categories has zero counts, the chi-squared test cannot be performed.
  #Large sample size: The chi-squared test becomes more accurate as the sample size increases. However, it is generally recommended to have a sample size of at least 100 or more for reliable results.
  #Categorical data: The chi-squared test is designed for analyzing categorical data. If one or both of the variables are continuous, other statistical tests such as t-tests or ANOVA should be used instead.
  #Validity of categories: The categories used in the analysis should be well-defined and meaningful. It's important to ensure that the categories accurately represent the variables being studied.

#NOTE: missing values are automatically excluded
#Age
chisq.test(table(data$Age_fct,data$mortstat))
#SEX
chisq.test(table(data$Sex,data$mortstat)) #there is an association!!!
#Race
chisq.test(table(data$Race,data$mortstat))
#Marital Status
chisq.test(table(data$MaritalStatus,data$mortstat))$expected
fisher.test(table(data$Education,data$mortstat))

#Education
      #PROBLEM: the expected frequency for incomplete group with mortstat = 1 is lower that 5
chisq.test(table(data$Education,data$mortstat))$expected
      #SOLUTION: therefore we should use the Fisher’s exact test
fisher.test(table(data$Education,data$mortstat))

#Coffee
chisq.test(table(data$Coffee,data$mortstat))

#Caffeinated Status
chisq.test(table(data$CaffeinatedStatus,data$mortstat))

#SmokingStatus
chisq.test(table(data$SmokingStatus,data$mortstat))

#Hypertension
chisq.test(table(data$Hypertension,data$mortstat))

#Hyperlipidemia
chisq.test(table(data$Hyperlipidemia,data$mortstat))

#Diabetes
chisq.test(table(data$Diabetes,data$mortstat))

