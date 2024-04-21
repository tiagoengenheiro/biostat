# Variables Description

### Render this document
If you want to see this document rendered choose one of the options:
* (a) Download a markdown extension in VS Code to render the md file (I'm using Markdown Preview Enhanced). After installation an icon (a window with a magnifying glass) will appear in the top right corner 
* (b) [See it directly in GitHub](https://github.com/tiagoengenheiro/biostat/blob/main/data/df_final_variables.md)

## Numeric Variables
- **SEQN**: Sequence Respondent Number (primary key)
- **LEXLABPI**: Left ABPI (Left ankle-brachial pressure index)
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LEXABPI.htm#LEXLABPI)
- **LEXRABPI**: Right ABPI (Right ankle-brachial pressure index)
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LEXABPI.htm#LEXRABPI)
- **Total Coffee Intake**: Coffee intake for each patient (g/day)
    - This variable was obtained from **DRXIGRMS** ([See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DRXIFF.htm#DRXIGRMS))
- **RIDAGEYR**: Age  
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#RIDAGEYR)
- **INDFMPIR**: Poverty income ratio (PIR) - a ratio of family income to poverty threshold
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#INDFMPIR)

## Categorical Variables
- **DMDEDUC**: Education Level
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#DMDEDUC)
- **RIDRETH1**: Race/Ethnicity
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#RIDRETH1)
- **RIAGENDR**: Sex
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#RIAGENDR)
- **DMDMARTL**: Marital Status
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#DMDMARTL)

## Binary Variables
- **PAD** (Disease)
- **Sweetened Coffee** (see [README](../README.md#coffee-types-data) for these variables) 
- **Unsweetened Coffee** (see [README](../README.md#coffee-types-data) for these variables)
- **Caffeineited Coffee** (see [README](../README.md#coffee-types-data) for these variables)
- **Coffee with Fat** (see [README](../README.md#coffee-types-data) for these variables)
- **FatFree coffee** (see [README](../README.md#coffee-types-data) for these variables)
- **Coffee with milk** (see [README](../README.md#coffee-types-data) for these variables)
- **Coffee without milk** (see [README](../README.md#coffee-types-data) for these variables)
- **Coffee** (Patient consumed coffee or not)