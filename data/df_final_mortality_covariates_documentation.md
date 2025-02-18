# Variables Description

### Render this document
If this document is not rendered already and you want to it rendered choose one of the options:
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
- **permth_int**: Number of Person Months of Follow-up from NHANES interview date
    - [See Documentation](/data/mortality_info/public-use-linked-mortality-files-data-dictionary.pdf)
- **permth_exam**:Number of Person Months of Follow-up from NHANES Mobile Examination Center (MEC) date
    - [See Documentation](/data/mortality_info/public-use-linked-mortality-files-data-dictionary.pdf)
    
## Categorical Variables
- **DMDEDUC**: Education Level
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#DMDEDUC)
- **RIDRETH1**: Race/Ethnicity
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#RIDRETH1)
- **RIAGENDR**: Sex
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#RIAGENDR)
- **DMDMARTL**: Marital Status
    - [See Documentation](https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#DMDMARTL)
- **eligstat** - Eligibility Status
    - [See Documentation](/data/mortality_info/public-use-linked-mortality-files-data-dictionary.pdf)
- **ucod_leading** - Underlying Leading Cause of Death
    - [See Documentation](/data/mortality_info/public-use-linked-mortality-files-data-dictionary.pdf)

## Binary Variables
- **PAD**: Disease
- **Coffee**: Patient consumed coffee (1) or not (0)
- **CaffeinatedStatus**: Caffeinated Coffee(1) | Decaffeited Coffee(0)
- **SugaryStatus**: Sweetened Coffee(1) | Unsweetened Coffee(0)
- **FattyStatus**: Coffee with Fat(1) | FatFree Coffee(0)
- **MilkContainingStatus**: Coffee with milk (1) | Coffee without milk(0)
- **mortstat**: Mortality Status 
    - [See Documentation](/data/mortality_info/public-use-linked-mortality-files-data-dictionary.pdf)


## Covariates (New ones)
- **SmokingStatus**: Never Smoked (0), Former Smoker (1), Current Smoker(2)
- **Hypertension**: Yes (1) No (0)
- **Diabetes**: Yes (1) No (0)
- **Hyperlipidemia**: Yes (1) No (0)


##### DIABETES
- **1** if one of the following conditions is true:
    - Doctor told that patient has diabetes (Self-reported) 
    - Patient is taking insulin now (Self-reported) 
    - Patient takes pills to lower blood sugar (Self-reported) 
    - Fasting Glucose values >=126 mg/dL 
    - Non-Fasting Glucose values >= 200 mg/dL 
    - Diabetes listed as multiple cause of death (from mortality data)
- **0** otherwise

##### Hypertension
- **1** if one of the following conditions is true:
    - Systolic Blood Pressure (SBP) >=140 mmHg
    - Diastolic Blood Pressure (DBP)>=90 mmHg
    - Taking prescription medication for hypertension (self-reported)
    - Hypertension listed as multiple cause of death (from mortality data)
- **0** otherwise

##### Hyperlipedemia
- **1** if one of the following conditions is true:
    - Diagnosis of elevated cholesterol by a physician (self-reported)
    - Total Cholesterol Level >= 240 mg/dL
- **0** otherwise

##### Smoking Status
- **0** (Never Smoked): Doesn’t smoke now and smoked <100 cigarettes
- **1** (Former Smoker): Doesn’t smoke now and smoked >=100 cigarettes
- **2**  (Current Smoker): Now Smokes 
Now - At the time of the questionnaire 


