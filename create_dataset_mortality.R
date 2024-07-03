#install.packages("packrat") #create an environment with packrat (run once)
#packrat::init(infer.dependencies=FALSE) # (run once, change to ur path)
#install.packages("tidyverse")

library("tidyverse") #already has tidy verse
library("haven")
library("dplyr")

rm(list = ls())
#Load Examination data (1999-2004) for Ankle Brachial Index (ABI) 
df_lexab_99_00=read_xpt("data/1999-00/Examination/LEXABPI.XPT")

df_lexab_01_02=read_xpt("data/2001-02/Examination/LEXAB_B.XPT")

df_lexab_03_04=read_xpt("data/2003-04/Examination/LEXAB_C.XPT")

#Bind Examination data (bind_rows merges all rows and keeps the union of the columns - https://www.statology.org/dplyr-bind_rows-bind_cols/)
df_lexab <- bind_rows(df_lexab_99_00, df_lexab_01_02,df_lexab_03_04)

#Remove rows with missing values on noth LEXLABPI and  LEXRABPI -> 7571
df_lexab <- df_lexab %>% filter(!is.na(LEXLABPI) | !is.na(LEXRABPI))
dim(df_lexab)[1]==7571
#Exclude the ones with LEXLABPI or LEXRABPI > 1.40
df_lexab <- df_lexab %>% filter(LEXLABPI<1.40 & LEXRABPI<1.40)

dim(df_lexab)[1]==6716
#Create PAD variable
df_lexab$PAD <- ifelse(
    !is.na(df_lexab$LEXLABPI) & df_lexab$LEXLABPI<=0.9 | 
    !is.na(df_lexab$LEXRABPI) & df_lexab$LEXRABPI<=0.9,1,0)
#Count how many rows with PAD=1
table(df_lexab$PAD)



#Create a new dataset with only the variables we need
df_lexab <- df_lexab %>% select(SEQN, LEXLABPI,LEXRABPI,PAD)

#Save the dataset
#write.csv(df_lexab, "data/df_lexab.csv", row.names = FALSE)
#Check the dataset size and PAD distribution
dim(df_lexab)
table(df_lexab$PAD)


#Only keep the patients with PAD=1
df_lexab <- df_lexab %>% filter(PAD==1)

#Save df_lexab
write.csv(df_lexab, "data/df_lexab.csv", row.names = FALSE)


#Load Dietary Data (1999-2004)
df_indiv_foods_99_00=read_xpt("data/1999-00/Dietary/DRXIFF.XPT")
df_indiv_foods_01_02=read_xpt("data/2001-02/Dietary/DRXIFF_B.XPT")
df_indiv_foods_03_04_1=read_xpt("data/2003-04/Dietary/DR1IFF_C.XPT") #there are two files for 2003-04 (2 days of dietary data)
df_indiv_foods_03_04_2=read_xpt("data/2003-04/Dietary/DR2IFF_C.XPT")

#For the same patient (SEQN) merge the rows on USDA food code by adding the grams consumed
df_indiv_foods_99_00=df_indiv_foods_99_00 %>% group_by(SEQN,DRDIFDCD) %>% summarise(DRXIGRMS=sum(DRXIGRMS,na.rm=TRUE))
df_indiv_foods_01_02=df_indiv_foods_01_02 %>% group_by(SEQN,DRDIFDCD) %>% summarise(DRXIGRMS=sum(DRXIGRMS,na.rm=TRUE))
df_indiv_foods_03_04_1=df_indiv_foods_03_04_1 %>% group_by(SEQN,DR1IFDCD) %>% summarise(DR1IGRMS=sum(DR1IGRMS,na.rm=TRUE))
df_indiv_foods_03_04_2=df_indiv_foods_03_04_2 %>% group_by(SEQN,DR2IFDCD) %>% summarise(DR2IGRMS=sum(DR2IGRMS,na.rm=TRUE))

#Merge the two 2003-04 files by doing the average of the grams per day
df_indiv_foods_03_04=full_join(df_indiv_foods_03_04_1,df_indiv_foods_03_04_2,by=c("SEQN"="SEQN","DR1IFDCD"="DR2IFDCD"))
df_indiv_foods_03_04$DRXIGRMS <- rowMeans(df_indiv_foods_03_04[,c("DR1IGRMS","DR2IGRMS")], na.rm = TRUE)
df_indiv_foods_03_04 <- df_indiv_foods_03_04 %>% select(SEQN,DRDIFDCD=DR1IFDCD,DRXIGRMS)

#Bind Dietary data
df_indiv_foods <- bind_rows(df_indiv_foods_99_00, df_indiv_foods_01_02,df_indiv_foods_03_04)


#Load the USDA food codes for coffee types
coffee_types <- read.delim("data/coffee_types.tsv", header = TRUE, sep = "\t")

coffee_types <- coffee_types %>% select(-FNDDS.food.description) #remove the description column

coffee_types <- coffee_types[!duplicated(coffee_types$FNDDS.food.code),] #Remove the coffee types with the same code (duplicates)

#Inner join the dietary data with the coffee types data to obtain the patients that consumed coffee
df_indiv_foods_with_coffee_types <- df_indiv_foods %>% inner_join(coffee_types, by = c("DRDIFDCD" = "FNDDS.food.code"))

#for each seqn we choose the DRDIFDCD with the highest grams consumed
df_indiv_foods_with_coffee_types <- df_indiv_foods_with_coffee_types %>% group_by(SEQN) %>% slice(which.max(DRXIGRMS)) #7735 rows


dim(df_indiv_foods_with_coffee_types) #needs to be 7735 which are the rows without duplicates
df_indiv_foods_with_coffee_types <- df_indiv_foods_with_coffee_types %>%
mutate(
        TotalCoffeeIntake=DRXIGRMS,
        CaffeinatedStatus=Caffeinated.status,
        SugaryStatus=Sugary.status,
        FattyStatus=Fatty.status,
        MilkContainingStatus=Milk.containing.status,
    )


df_indiv_foods_with_coffee_types <- df_indiv_foods_with_coffee_types %>% 
    select(SEQN, TotalCoffeeIntake, CaffeinatedStatus, SugaryStatus, FattyStatus, MilkContainingStatus)

#Replace - with NA
df_indiv_foods_with_coffee_types[df_indiv_foods_with_coffee_types == "-"] <- NA

df_indiv_foods_with_coffee_types

df_lexab <- read.csv("data/df_lexab.csv")

#Select only the SEQN that are present in the df_indiv_foods
df_lexab <- df_lexab %>% filter(SEQN %in% df_indiv_foods$SEQN)
dim(df_lexab)[1]==512


#Merge the dietary data with the lexab data (Left join because we want patients that did and did not consume coffee)
df_pad_coffee_types<- left_join(df_lexab, df_indiv_foods_with_coffee_types, by = "SEQN")


#create a new column with 1 for coffee consumers
df_pad_coffee_types$Coffee = ifelse(is.na(df_pad_coffee_types$TotalCoffeeIntake),0,1) 
#Save the dataset
#write.csv(df_pad_coffee_types, "data/df_pad_coffee_types.csv", row.names = FALSE)

#Load Demographics Data (1999-2004)
df_demo_99_00=read_xpt("data/1999-00/Demographics/DEMO.XPT")
df_demo_01_02=read_xpt("data/2001-02/Demographics/DEMO_B.XPT")
df_demo_03_04=read_xpt("data/2003-04/Demographics/DEMO_C.XPT")

#Bind Demographics data
df_demo <- bind_rows(df_demo_99_00, df_demo_01_02,df_demo_03_04)

df_demo <- df_demo %>% select(SEQN,RIAGENDR, RIDAGEYR, RIDRETH1,DMDEDUC,DMDMARTL,INDFMPIR)

#Load the PAD and coffee types data
#df_pad_coffee_types <- read.csv("data/df_pad_coffee_types.csv")

#Merge the demographics data with the PAD and coffee types data (Left join because we only want the patients that have PAD variable)
df_final<- inner_join(df_pad_coffee_types, df_demo, by = "SEQN")

#Save the dataset
write.csv(df_final, "data/df_final.csv", row.names = FALSE)

#Load Mortality Data (1999-2004)

dsn_99_00 <- read_fwf(file="data/1999-00/Mortality/NHANES_1999_2000_MORT_2019_PUBLIC.dat",
                col_types = "iiiiiiii",
                fwf_cols(seqn = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)

dsn_01_02 <- read_fwf(file="data/2001-02/Mortality/NHANES_2001_2002_MORT_2019_PUBLIC.dat",
                col_types = "iiiiiiii",
                fwf_cols(seqn = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)

dsn_03_04 <- read_fwf(file="data/2003-04/Mortality/NHANES_2003_2004_MORT_2019_PUBLIC.dat",
                col_types = "iiiiiiii",
                fwf_cols(seqn = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)

#Bind Mortality data
dsn <- bind_rows(dsn_99_00, dsn_01_02,dsn_03_04)

#Save the dataset
write.csv(dsn, "data/dsn.csv", row.names = FALSE)
table(dsn$hyperten)
#Inner join with df_final
df_final_mortality <- inner_join(df_final, dsn, by = c("SEQN"="seqn"))
table(df_final_mortality$mortstat)

#Save the dataset
write.csv(df_final_mortality, "data/df_final_mortality.csv", row.names = FALSE)

#Do a function of fibonacci for the first 10 numbe

rm(list = ls())
#Load df_final mortality
df <- read.csv("data/df_final_mortality.csv")

#Delete diabetes and hypertension columns
#df <- df %>% select(-diabetes, -hyperten)
dim(df)[1]==512

## ADDITIONAL COVARIATES
# - Smoking status
# - Hypertension
# - Diabetes
# - Hyperlipdmia
# - BMI ?

#Load Questionnaire Data (1999-2004)
df_questionnaire_99_00=read_xpt("data/1999-00/Questionnaire/SMQ.XPT")
df_questionnaire_01_02=read_xpt("data/2001-02/Questionnaire/SMQ_B.XPT")
df_questionnaire_03_04=read_xpt("data/2003-04/Questionnaire/SMQ_C.XPT")

#Bind Questionnaire data
df_questionnaire <- bind_rows(df_questionnaire_99_00, df_questionnaire_01_02,df_questionnaire_03_04)

#Select only the SEQN that are present in the df

#Smoking status
df_smoking <- df_questionnaire %>% select(SEQN,SMQ020,SMQ040,SMD070,SMQ050Q)
## Smoking status variable
#SMQ020: Smoked at least 100 cigarettes in life
#SMQ040: Do u now smoke cigarettes
#SMQ050Q - How long since quit smoking cigarettes
#SMD070 - # cigarettes smoked per day now

df_with_smoking <- inner_join(df, df_smoking, by = "SEQN")
dim(df_with_smoking)
table(df_with_smoking$SMQ020)
table(df_with_smoking$SMQ040)


df_with_smoking[df_with_smoking$SMQ020==2,]$SMQ040
#If didn't smoke at least 100 ciggarestes -> never smoked (0)
#If smoked at least 100 cigarettes and now doesn't smoke -> former smoker (1)
#If smoked at least 100 cigarettes and now smokes -> current smoker (2)
sum(is.na(df_with_smoking$SMQ020))
df_with_smoking[is.na(df_with_smoking$SMQ020),]
df_with_smoking$SmokingStatus <- ifelse(df_with_smoking$SMQ020==2,0,ifelse(df_with_smoking$SMQ040!=3,1,2))
table(df_with_smoking$SmokingStatus)
sum(is.na(df_with_smoking$SmokingStatus))

df <- df_with_smoking %>% select(-SMQ020, -SMQ040, -SMQ050Q, -SMD070)
dim(df)
colnames(df)



# Hypertension

#Load Examination Data (1999-2004)  for Blood Pressure
df_blood_pressure_99_00=read_xpt("data/1999-00/Examination/BPX.XPT")
df_blood_pressure_01_02=read_xpt("data/2001-02/Examination/BPX_B.XPT")
df_blood_pressure_03_04=read_xpt("data/2003-04/Examination/BPX_C.XPT")

#Bind Examination data
df_blood_pressure <- bind_rows(df_blood_pressure_99_00, df_blood_pressure_01_02,df_blood_pressure_03_04)

#Select only the SEQN that are present in the df
df_blood_pressure <- df_blood_pressure %>% select(SEQN,BPXSAR,BPXDAR,BPXSY1,BPXSY2,BPXSY3,BPXSY4,BPXDI1,BPXDI2,BPXDI3,BPXDI4)

#Join with df
df_with_blood_pressure <- inner_join(df, df_blood_pressure, by = "SEQN")
#Among the NAs of BPXSAR, check if there is a non na value in BPXSY1-4
df_with_blood_pressure[is.na(df_with_blood_pressure$BPXSAR),]$BPXSY4
#there is not
df_with_blood_pressure$BPS <- df_with_blood_pressure$BPXSAR

df_with_blood_pressure$BPD <- df_with_blood_pressure$BPXDAR

df_with_blood_pressure <- df_with_blood_pressure %>% select(-BPXSAR, -BPXDAR, -BPXSY1, -BPXSY2, -BPXSY3, -BPXSY4, -BPXDI1, -BPXDI2, -BPXDI3, -BPXDI4)
dim(df_with_blood_pressure)[1]==512

#Load prescription medication data in Questionnaire Data (1999-2004)
df_prescription_99_00=read_xpt("data/1999-00/Questionnaire/RXQ_RX.XPT")
df_prescription_01_02=read_xpt("data/2001-02/Questionnaire/RXQ_RX_B.XPT")
df_prescription_03_04=read_xpt("data/2003-04/Questionnaire/RXQ_RX_C.XPT")

#Bind prescription data
df_prescription <- bind_rows(df_prescription_99_00, df_prescription_01_02,df_prescription_03_04)

#Select only FDACODE1-6 and SEQN
df_prescription <- df_prescription %>% select(SEQN,FDACODE1,FDACODE2,FDACODE3,FDACODE4,FDACODE5,FDACODE6)

#For each SEQN check if FDACODE1 to FDACODE6 equals to 0506 (hypertension medication)
df_prescription$hypertensionmed <- ifelse(
    !is.na(df_prescription$FDACODE1) & df_prescription$FDACODE1=="0506" | 
    !is.na(df_prescription$FDACODE2) & df_prescription$FDACODE2=="0506" | 
    !is.na(df_prescription$FDACODE3) & df_prescription$FDACODE3=="0506" | 
    !is.na(df_prescription$FDACODE4) & df_prescription$FDACODE4=="0506" | 
    !is.na(df_prescription$FDACODE5) & df_prescription$FDACODE5=="0506" | 
    !is.na(df_prescription$FDACODE6) & df_prescription$FDACODE6=="0506",1,0)

#Since each SEQN can have multiple rows, we group by SEQN and check if patient is taking at least one hypertension medication
df_prescription=df_prescription %>% group_by(SEQN) %>% summarise(hypertensionmed=ifelse(sum(hypertensionmed,na.rm=TRUE)>0,1,0))
#Create a new df with only SEQN hypertensionmed=1 if 
dim(df_prescription)
#Join with df_with_blood_pressure
df_hypertension<- inner_join(df_with_blood_pressure, df_prescription, by = "SEQN")
table(df_hypertension$hypertensionmed)

# Hypertension
df_hypertension$Hypertension <- ifelse( 
    !is.na(df_hypertension$BPS) & df_hypertension$BPS>=140 | 
    !is.na(df_hypertension$BPD) & df_hypertension$BPD>=90 | 
    !is.na(df_hypertension$hypertensionmed) & df_hypertension$hypertensionmed==1 |
    !is.na(df_hypertension$hyperten) & df_hypertension$hyperten==1 ,1,0)
table(df_hypertension$Hypertension)
sum(is.na(df_hypertension$Hypertension))
colnames(df_hypertension)

df <- df_hypertension %>% select(-BPS, -BPD, -hypertensionmed, -hyperten)
colnames(df)

#save new df
write.csv(df, "data/df_final_mortality_covariates.csv", row.names = FALSE)

