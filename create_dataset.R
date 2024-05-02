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

#Remove rows with missing values on LEXLABPI or on LEXRABPI -> 7571
df_lexab <- df_lexab %>% filter(!is.na(LEXLABPI) | !is.na(LEXRABPI))
dim(df_lexab)[1]==7571
#Exclude the ones with LEXLABPI or LEXRABPI > 1.40
df_lexab <- df_lexab %>% filter(LEXLABPI<1.40 & LEXRABPI<1.40)

dim(df_lexab)[1]==6716
#Create PAD variable
df_lexab$PAD <- ifelse(df_lexab$LEXLABPI<=0.9 | df_lexab$LEXRABPI<=0.9,1,0)
#Count how many rows with PAD=1
table(df_lexab$PAD)



#Create a new dataset with only the variables we need
df_lexab <- df_lexab %>% select(SEQN, LEXLABPI,LEXRABPI,PAD)

#Save the dataset
#write.csv(df_lexab, "data/df_lexab.csv", row.names = FALSE)
#Check the dataset size and PAD distribution
dim(df_lexab)
table(df_lexab$PAD)




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
dim(df_lexab)[1]==6565


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

#Inner join with df_final
df_final_mortality <- inner_join(df_final, dsn, by = c("SEQN"="seqn"))

#Filter only the patients that are deceased
df_final_mortality <- df_final_mortality %>% filter(PAD==1)

#Filter 
#Save the dataset
write.csv(df_final_mortality, "data/df_final_mortality.csv", row.names = FALSE)
