## Report URL
[Overleaf link](https://www.overleaf.com/5239177249dncfbbvytmqw#74d3c6)

## Papers 
(2011) - [Secondary Prevention and Mortality in Peripheral Arterial Disease](/papers/pande-et-al-2011-secondary-prevention-and-mortality-in-peripheral-artery-disease.pdf)

(2023) - [The associations of coffee consumption, coffee types, and caffeine metabolites with periodontitis: Results from NHANES 2009–2014](/papers/Journal%20of%20Periodontology%20-%202023%20-%20Chen%20-%20The%20associations%20of%20coffee%20consumption%20%20coffee%20types%20%20and%20caffeine%20metabolites.pdf)

## General Information
We are going to consider 3 data periods (1999-2000 + 2001-2002 + 2003-2004) which were the periods
considered by the 2011 paper. We could use more periods, but these were the only ones where data was collected to detect our diease (Peripheral Arterial Disease or PAD) 


### NHANES Data (by Year):

- [1999-00](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=1999)

- [2001-02](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2001)

- [2003-04](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2003)

From the links above it's possible to access all types of data:
* **Demographics Data** (here is the data for possible covariates age,sex,race, etc)
* **Dietary Data** (here is the data for the coffee types)
* **Examination Data** (here is the data to identify our disease)
* **Laboratory Data** (here would be the data for the coffee metabolites but this data is not available in our 1999-2004 interval)
* **etc** (not so relevant for us)

#### Demographics Data
From the Demographics Data we retrieved the following Data File:
- **Demographic Variables & Sample Weights**

    Most important variables: <br>
    - **RIAGENDR**: Sex
    - **RIDAGEYR**: Age
    - **RIDRETH1**: Race/Ethnicity
    - **DMDEDUC**: Education Level
    - **DMDMARTL**: Marital Status
    - **INDFMPIR**: Poverty income ratio (PIR) - a ratio of family income to poverty threshold


#### Dietary Data
From the Dietary Data we retrieved the following Data File:
- **Dietary Interview - Individual Foods**

    Most important variables: <br>
    - **DRDIFDCD**: USDA food code (this code is equivalent to FNDDS and allows us to retrieve the coffee types using the supplementary table of the 2023 paper)
    - **DRXIGRMS**: Grams (I believe this variable points us to *Total Coffee Intake* from the 2023 paper)

#### Examination Data
From the Dietary Data we retrieved the following Data File:
- **Lower Extremity Disease** - Ankle Brachial Blood Pressure Index

    Most important variables:
    - **LEXLABPI**: Left ABPI
    - **LEXRABPI**: Right ABPI <br>

    From these variables we can create a variable for our disease (PAD):
    - If one of these variables is lower or equal to 0.9, then the patient has PAD (this is the method used by the 2011 paper)

### Coffee Types Data 
**Source**: This data was extracted from the 2023 paper (Table S1) and contains an association between one USDA/FNDDS food code and one or multiple types of coffee.

To obtain a comparable table with Table S1 in the 2023 paper, the following approach was conducted:
1. Only one coffee register was considered per patient/respondent (SEQN)
2. Several variables were criated to match the ones in the paper:

    - **Total Coffee Intake(g/day)**: Coffee intake for each patient (grams/day)
    - **Sweetened Coffee**: (Binary, see note below)
    - **Unsweetened Coffee**: (Binary, see note below)
    - **Caffeineited Coffee**: (Binary, see note below)
    - **Coffee with Fat**: (Binary, see note below)
    - **Fat-free coffe**: (Binary, see note below)
    - **Coffee with milk**: (Binary, see note below)
    - **Coffee without milk**: (Binary, see note below)

    <br>
    
    >**Note**:
    **1 means TRUE, 0 means FALSE or missing**.
    If SweetenedCoffee is 1 then the coffee was sweet otherwise the coffee wasn't sweet or that information was not available. If it was not sweet it will be 1 in the Unsweetened Coffee, if it's 0 in both varialbes it means the sweet information was missing. The same applies to the other variables


### Dataset

#### Variables
The final Dataset (data/**df_final.csv**) contains these variables and some more, you can find all variables and corresponding descriptions in [data/df_final_variables.md](data/df_final_variables.md)
