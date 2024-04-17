#install.packages("packrat") #create an environment with packrat (run once)
#packrat::init(infer.dependencies=FALSE) # (run once, change to ur path)
#install.packages("tidyverse")

library("tidyverse") #already has tidy verse
library("haven")
library("dplyr")

rm(list = ls())
#Load Examination data (1999-2004)
df_lexab_99_00=read_xpt("data/1999-00/Examination/LEXABPI.XPT")

df_lexab_01_02=read_xpt("data/2001-02/Examination/LEXAB_B.XPT")

df_lexab_03_04=read_xpt("data/2003-04/Examination/LEXAB_C.XPT")

#Bind Examination data (bind_rows merges all rows and keeps the union of the columns - https://www.statology.org/dplyr-bind_rows-bind_cols/)
df_lexab <- bind_rows(df_lexab_99_00, df_lexab_01_02,df_lexab_03_04)

#Create PAD variable
df_lexab$PAD <- ifelse(df_lexab$LEXLABPI<=0.9 | df_lexab$LEXRABPI<=0.9,1,0)

#Delete rows (patients) with missing PAD
df_lexab <- df_lexab %>% filter(!is.na(PAD))

#Create a new dataset with only the variables we need
df_lexab <- df_lexab %>% select(SEQN, LEXLABPI,LEXRABPI,PAD)

#Check the dataset size and PAD distribution
dim(df_lexab)
table(df_lexab$PAD)

rm(list = ls())
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
df_indiv_foods

#Load coffee types matching with USDA food codes
coffee_types <- read.delim("data/coffee_types.tsv", header = TRUE, sep = "\t")

coffee_types <- coffee_types %>% select(-FNDDS.food.description) #remove the description column

coffee_types <- coffee_types[!duplicated(coffee_types$FNDDS.food.code),] #check this better later

#Inner join the dietary data with the coffee types data to obtain the patients that consumed coffee
df_indiv_foods_with_coffee_types <- df_indiv_foods %>% inner_join(coffee_types, by = c("DRDIFDCD" = "FNDDS.food.code"))



df_indiv_foods_with_coffee_types<- df_indiv_foods_with_coffee_types[!duplicated(df_indiv_foods_with_coffee_types$SEQN),] #check this better later


