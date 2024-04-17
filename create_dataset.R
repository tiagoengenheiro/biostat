#install.packages("packrat") #create an environment with packrat (run once)
#packrat::init(infer.dependencies=FALSE) # (run once, change to ur path)
#install.packages("tidyverse")

library("tidyverse") #already has tidy verse
library("haven")
library("dplyr")


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

