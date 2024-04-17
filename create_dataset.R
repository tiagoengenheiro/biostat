#install.packages("packrat") #create an environment with packrat (run once)
#packrat::init(infer.dependencies=FALSE) # (run once, change to ur path)
#install.packages("tidyverse")

library("tidyverse") #already has tidy verse
library("haven")
library("dplyr")



#Important variables: LEXLABPI and LEXRABPI 
#df_bpx_99_00=read_xpt("data/BPX.XPT")
df_lexab_99_00=read_xpt("data/LEXABPI.XPT")

#df_bpx_01_02=read_xpt("data/BPX_B.XPT")
df_lexab_01_02=read_xpt("data/LEXAB_B.XPT")

#df_bpx_03_04=read_xpt("data/BPX_C.XPT")
df_lexab_03_04=read_xpt("data/LEXAB_C.XPT")

#df_bpx <- rbind(df_bpx_99_00, df_bpx_01_02,df_bpx_03_04)
df_lexab <- bind_rows(df_lexab_99_00, df_lexab_01_02,df_lexab_03_04)


#df_bpx_lexab <- merge(df_bpx, df_lexab, by = "SEQN", all = TRUE)

#DIETARY DATA - https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=Dietary
#df_dietary = read_xpt("data/DR1IFF_C.XPT") #TODO - see this one better to retrieve coffee types
#Variables of interest -> Caffeine variable 

### PAD estimation
## LEXLPTSM<=0.90 or LEXRPTSM<=0.9 => PAD 

df_lexab$PAD <- ifelse(df_lexab$LEXLABPI<=0.9 | df_lexab$LEXRABPI<=0.9,1,0)
