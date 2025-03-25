# load packages
pacman::p_load(haven,
               dplyr,
               tidyr,
               magrittr)

#Demographic Data Cleaning
adult_clean <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.xpt',
                        col_select = c(SEQN, RIAGENDR, RIDAGEYR , RIDRETH1)) %>% # Reduce 'adult' dataframe to just SEQN, RIDERETH1, RIAGENDER, and RIDAGEYR variables
  filter(RIDAGEYR >= 18, RIDAGEYR <40) %>% # Remove study participants that are not an adult of at most 39 years of age
  mutate_at(vars(SEQN, RIDRETH1, RIAGENDR), .funs = as.character) %>% # Convert SEQN, RIDRETH1 and RIAGENDR column types to <chr>
  mutate(RIAGENDR = if_else( RIAGENDR== '1', 'Male', 'Female'), # Converts gender values into "Male" and "Female"
         RIDRETH1 = recode(RIDRETH1,'1' = 'Hispanic Mexican', '2' = 'Hispanic Other', '3' = 'Non-Hispanic White', # Converts ethnicity value
                           '4' = 'Non-Hispanic Black', '5' = 'Other/Multi-Racial'))

# # A tibble: 2,226 x 4
# SEQN RIAGENDR RIDAGEYR RIDRETH1          
# <chr> <chr>       <dbl> <chr>             
# 1 83741 Male      22 Non-Hispanic Black
# 2 83742 Female      32 Hispanic Mexican  
# 3 83743 Male        18 Other/Multi-Racial
# 4 83752 Female      30 Hispanic Other    
# 5 83759 Female      19 Hispanic Mexican  
# 6 83761 Female      24 Other/Multi-Racial
# 7 83762 Female      27 Non-Hispanic Black
# 8 83781 Female      27 Non-Hispanic Black
# 9 83784 Male        22 Hispanic Other    
# 10 83799 Female     37 Hispanic Other    
# # ... with 2,216 more rows

#Drug Usage Data Cleaning
drug_clean <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DUQ_I.xpt') %>%
  select(SEQN, DUQ370) %>% #Subset drug dataframe to include only SEQN and DUQ370 variables.
  mutate_all(.funs = as.character) %>% # convert DUQ370 column to <chr> type
  mutate(DUQ370 = na_if(DUQ370, '1'), DUQ370 = na_if(DUQ370, '7'), DUQ370 = na_if(DUQ370, '9'), # replace those who may have injected needles with NA
         DUQ370 = recode(DUQ370, '2' = 'No')) %>% # replace '2' with 'No' for needle injection
  na.omit() # removes those who may have injected needles

# # A tibble: 4,161 x 2
# SEQN DUQ370
# <chr> <chr> 
# 1 83732 No    
# 2 83733 No    
# 3 83735 No    
# 4 83736 No    
# 5 83741 No    
# 6 83742 No    
# 7 83743 No    
# 8 83744 No    
# 9 83747 No    
# 10 83750 No    
# # ... with 4,151 more rows

#Diet Data Cleaning
diet_clean <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DBQ_I.xpt',
                       col_select = c(SEQN, DBQ700)) %>% #Reduce to just SEQN and DBQ700 variables
  mutate(DBQ700 = na_if(DBQ700, 9), # replace 9 values with NA
         DBQ700 = if_else(DBQ700 > 2, 'Poor/Fair/Good', 'Very good/Excellent')) %>% 
  mutate_all(.funs = as.character) %>%
  na.omit()

# # A tibble: 6,326 x 2
# SEQN  DBQ700             
# <chr> <chr>              
# 1 83732 Poor/Fair/Good     
# 2 83733 Very good/Excellent
# 3 83734 Poor/Fair/Good     
# 4 83735 Poor/Fair/Good     
# 5 83736 Poor/Fair/Good     
# 6 83737 Poor/Fair/Good     
# 7 83741 Poor/Fair/Good     
# 8 83742 Very good/Excellent
# 9 83743 Very good/Excellent
# 10 83744 Poor/Fair/Good     
# # ... with 6,316 more rows

#Merging Hepatitus B&C laboratory data by respondent sequence number

hepbd <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HEPBD_I.xpt') #Hep B lab results
hepc <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HEPC_I.xpt') #Hep C lab results

hep_combo <- full_join(hepc, hepbd, by = 'SEQN') %>% #combined Hep B and C dataframe
  select(SEQN, LBXHCR, LBXHBC) %>% #subset data to only include SEQN, LBXHCR, and LBXHBC variables
  mutate_at(vars(SEQN), .funs = as.character) %>%
  mutate(LBXHCR = recode(LBXHCR, `3` = 0, `2` = 0),
         LBXHBC = recode(LBXHBC, `2` = 0)) # Set values to '0' to indicate negative status

#Merging HPV laboratory data by respondent sequence number

ohpv <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/ORHPV_I.xpt',
                 col_select = c(SEQN, ORXHPV)) #Oral HPV lab results
phpv <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HPVP_I.xpt',
                 col_select = c('SEQN','LBDRPCR')) #Penile HPV lab results
lhpv <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HPVSWR_I.xpt',
                 col_select = c('SEQN','LBDRPCR')) #Vaginal Low-Risk HPV lab results
  
hpv_combo <- full_join(phpv, lhpv, by = c('SEQN','LBDRPCR'))
hpv_combo <- full_join(hpv_combo, ohpv, by = 'SEQN') %>% #combined HPV oral, penile, and vaginal lab results
  mutate_at(vars(SEQN), .funs = as.character) %>% 
  mutate(ORXHPV = na_if(ORXHPV, 3), LBDRPCR = na_if(LBDRPCR, 3), #Set values equal to 3 to NA to remove inadequate/non-evaluated results
         ORXHPV = recode(ORXHPV, `2` = 0), LBDRPCR = recode(LBDRPCR, `2` = 0)) #Set values equal to 2 to 0 to indicate negative result 

# HSV laboratory data cleaning
hsv <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HSV_I.xpt', #Herpes Simplex Virus lab results
                col_select = c(SEQN, LBXHE2)) %>% #only include SEQN and LBXHE2 variables
  mutate_at(vars(SEQN), .funs = as.character) %>%
  mutate(LBXHE2 = na_if(LBXHE2, 3), LBXHE2 = recode(LBXHE2, `2` = 0)) # set 3 to NA for indeterminate, 2 to 0 for negative 

# Chlamydia laboratory data cleaning
chlm <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/CHLMDA_I.xpt') %>% #Chylamydia lab results
  mutate_at(vars(SEQN), .funs = as.character) %>% 
  mutate(URXUCL = na_if(URXUCL, 3), URXUCL = recode(URXUCL, `2` = 0)) # set 3 to NA for indeterminate, 2 to 0 for negative

#HIV laboratory data cleaning
hiv <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HIV_I.xpt', #HIV lab results
                col_select = c(SEQN, LBXHIVC)) %>%  #only include SEQN and LBXHIVC variables
  mutate_at(vars(SEQN), .funs = as.character) %>% 
  mutate(LBXHIVC = recode(LBXHIVC, `2` = 0)) #Set 2 to 0 for negative results

#Merging all laboratory result dataframes by SEQN
lab_clean <- hep_combo %>%
  full_join(hpv_combo, by = 'SEQN') %>%
  full_join(hsv, by = 'SEQN') %>%
  full_join(chlm, by = 'SEQN') %>%
  full_join(hiv, by = 'SEQN') %>%
  na.omit() %>% #Remove all study participants with missing values
  mutate('Positive.Test' = rowSums(select(., -SEQN)))  #Create new variable "Positive.Case" listing sum of STI laboratory tests w/ positive results

lab_pos <- lab_clean %>% #List of study participant SEQN with at least 1 positive test result
  filter(Positive.Test > 0) %>%
  select(SEQN) 
lab_neg <- lab_clean %>%  #List of study participant SEQN with no positive test result
  filter(Positive.Test == 0) %>% 
  select(SEQN)

#Physical Activity Data Cleaning
phys <- read_xpt('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/PAQ_I.xpt',
                 col_select = c(SEQN, PAQ605, PAQ620, PAQ650, PAQ665)) %>% 
  mutate_at(vars(SEQN), .funs = as.character)

#Adjust 'Don't know' responses to NA and values equaling 2 to 0 for 'No'
phys[, c('PAQ650', 'PAQ665', 'PAQ605', 'PAQ620')][phys[, c('PAQ650', 'PAQ665', 'PAQ605', 'PAQ620')] == 9] = NA
phys[, c('PAQ650', 'PAQ665', 'PAQ605', 'PAQ620')][phys[, c('PAQ650', 'PAQ665', 'PAQ605', 'PAQ620')] == 2] = 0

phys_clean <- na.omit(phys) #complete physical responses with no missing values

#Combined Data Frames for Laboratory Results, Demographics, Diet, and Physical Activity
combo <- left_join(lab_clean, adult_clean, by = 'SEQN') %>%
  inner_join(drug_clean, by='SEQN')
dietcombo <- left_join(combo, diet_clean, by = 'SEQN')
physcombo <- inner_join(combo, phys_clean, by = 'SEQN') %>%
  mutate('Activity' = 'sedentary', #Create new variable "Activity" to record Vigorous/Moderate/Sedentary
         Activity = case_when(
           PAQ605 == 1 | PAQ650 == 1 ~ 'vigorous',
           PAQ620 == 1 | PAQ665 == 1 ~ 'moderate',
         TRUE ~ Activity))

# Confirm no missing values

anti_join(combo, diet_clean, by ='SEQN')
anti_join(combo, phys_clean, by = 'SEQN')

# Create .csv files
# write_csv(dietcombo, path='./diet.csv'); write_csv(physcombo, path='./physical.csv'); write_csv(combo, path='./lab_demo_clean.csv')

