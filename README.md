## Report URL
[Overleaf link](https://www.overleaf.com/5239177249dncfbbvytmqw#74d3c6)

## Papers (available in /papers)
(2011) - "Secondary Prevention and Mortality in Peripheral Arterial Disease"

(2023) - "The associations of coffee consumption, coffee types, andcaffeine metabolites with periodontitis: Results from NHANES 2009–2014"

## General Information
We are going to consider 3 data periods (1999-2000 + 2001-2002 + 2003-2004) which were the periods
considered by the 2011 paper. We could use more periods, but these were the only periods that data was collected to detect our diease (Peripheral Arterial Disease or PAD) 

### NHANES Data (by Year):
From the links below it's possible to access all types of data:
* Demographics Data (here is the data for possible covariates age,sex,race, etc)
* Dietary Data (here is the data for the coffee types)
* Examination Data (here is the data to identify our disease)
* Laboratory Data (here would be the data for the coffee metabolites but this data is not available in our 1999-2004 interval)

[1999-00](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=1999)

[2001-02](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2001)

[2003-04](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2003)

#### Examination Data 
From the Dietary Data we retrieved the following Data Files:
- Lower Extremity Disease - Ankle Brachial Blood Pressure Index

    Most important variables are:
    - **LEXLABPI**: Left ABPI
    - **LEXRABPI**: Right ABPI <br>

    From these variables we can create a variable for our disease:
    - If one of these variables is lower or equal to 0.9, then the patient has PAD (this is the method used by the 2011 paper)

#### Dietary Data
From the Dietary Data we retrieved the following Data Files:
- Dietary Interview - Individual Foods 

    Most important variables are: <br>
    - **DRDIFDCD**: USDA food code (this code is equivalent to FNDDS and allows us to retrieve the coffee types using the supplementary table of the coffee types paper)
    - **DRXIGRMS**: Grams (I believe this variable points us to *Total Coffee Intake* from the 2023 paper)

