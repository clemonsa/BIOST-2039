#Lab 9 Practice

library(dplyr)
library(SASxport)
library(ggplot2)

#Merging demographics and blood pressure data sets by
#respondent sequence number

demo <- read.xport('DEMO_I.XPT')
bpx <- read.xport('BPX_I.XPT')

combo <- full_join(bpx, demo, by = 'SEQN')

#Subset data into including only the SEQN, BPXSY1, RIAGENDR,
#and RIDAGEYR variables

combo <- combo[,c('SEQN','BPXSY1', 'RIAGENDR','RIDAGEYR')]

#Change Gender variable values to "Male" and "Female"
combo$RIAGENDR[combo$RIAGENDR == 1] = 'Male'
combo$RIAGENDR[combo$RIAGENDR == 2] = 'Female'

#Subset data to include only participants => 18yrs old with no missing data

combo.adults <- combo[combo$RIDAGEYR >= 18,] #adults only
combo.adults <- na.omit(combo.adults) #remove any participants w/ missing data

#Remove labels
#combo.adults[] <- lapply(combo.adults, unclass)

#Create numerical and visual summaries of BPXSY1, RIDAGEYR, and RIAGENDR
summary(combo.adults$BPXSY1)

ggplot(data=combo.adults, aes(x=BPXSY1)) +
  geom_histogram(aes(y=..count..), colour = 'black', fill='royalblue4', bins = 50) +
  xlab('Systolic Blood Pressure: 1st Reading mm Hg')

males <- sum(combo.adults$RIAGENDR == 'Male')
females <- sum(combo.adults$RIAGENDR == 'Female')

cat('There are',males,'male and',females,'female adults with no missing data.')

ggplot(data=combo.adults, aes(x=RIAGENDR)) +
  geom_bar(aes(y=..count..), colour = 'black', fill='palegreen4') +
  xlab('Gender')

summary(combo.adults$RIDAGEYR)

ggplot(data=combo.adults, aes(x=RIDAGEYR)) +
  geom_histogram(aes(y=..count..), colour = 'black', fill='magenta4', bins = 50) +
  xlab('Age By Year (80 and over)')
  
