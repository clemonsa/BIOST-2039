source("Project_DataClen.R")

set.seed(12062019)

pacman::p_load(car)

# combo <- read_csv(file = './lab_demo_clean.csv')
# physcombo <- read_csv(file = './physical.csv')
# dietcombo <- read_csv(file = './diet.csv')

# #Numerical Summary of Positive STI Test Results by Gender
combo.total <- nrow(combo) #Total number of demo and lab result participants w/o missing data

pos.male <- sum(combo$Positive.Test[combo$RIAGENDR == 'Male'] == 1) #of males who tested positive for an STI
pos.female <- sum(combo$Positive.Test[combo$RIAGENDR == 'Female'] == 1) #of females who tested positive for an STI
pos.total <- pos.male + pos.female  #Total number of study participants w/ positive test for at least one STI

male.total <- sum(combo$RIAGENDR == 'Male') #Number of Males in combo dataset
female.total <- sum(combo$RIAGENDR == 'Female') #Number of Females in combo dataset

combo$Positive.Test[combo$Positive.Test == 0] = 'Negative'
combo$Positive.Test[combo$Positive.Test == 1] = 'Positive'

male_stat <- summary(combo$Positive.Test[combo$RIAGENDR == 'Male']) #descriptive statistics
female_stat <- summary(combo$Positive.Test[combo$RIAGENDR == 'Female'])

##Two-Sample Z-Test for # of Positive STI Test Results
z <- prop.test(x=c(pos.male, pos.female), n=c(male.total, female.total), correct = FALSE)
z 
cat('P-value is ',z$p.value,' which suggests enough evidence to REJECT THE NULL and thus we can conclude that the true difference in population proportions are NOT equal to 0
 and thus the true population proportion of males and females with a positive STI case are different and confirms gender as confounding factor.')

##Chi-squared Test for Homogeneity of Positive STI Cases (whether they have 1 or more is treated the same) and Diet
cat('Null Hypothesis: The true healthy diet proportion of subjects is homogenous across STI Status.')
cat('Alternative Hypothesis: The true healthy diet proportion of subjects is NOT homogenous across STI Status.')


dietcombo$Positive.Test[dietcombo$Positive.Test %in% c(2,3,4,5)] = 1
dietcombo$Positive.Test[dietcombo$Positive.Test == 0] = 'Negative'
dietcombo$Positive.Test[dietcombo$Positive.Test == 1] = 'Positive'

# Create Matrix of Males by STI Status and Diet
male.obs <- matrix(nrow=3,ncol=3)
row.names(male.obs)<- c('Negative','Positive','Total')
colnames(male.obs)<- c('Poor/Fair/Good','Very good/Excellent','Total')
male.obs[1,1] <- 401-90
male.obs[1,2] <- 90
male.obs[1,3] <- 401
male.obs[2,1] <- 329-68
male.obs[2,2] <- 68
male.obs[2,3] <- 329
male.obs[3,1] <- (401-90) + (329-68)
male.obs[3,2] <- 90 + 68
male.obs[3,3] <- 401 + 329

#Create matrix of expected counts of subjects
male.exp <- matrix(nrow=2, ncol=2)
rownames(male.exp) <-c('Negative','Positive')
colnames(male.exp) <- c('Poor/Fair/Good','Very good/Excellent')
for (j in 1:2){
  m <- male.obs[3, j]
  
  for (i in 1:2){
    
    n <- (male.obs[i, 3]*m)/male.obs[3,3]
    male.exp[i,j] <- n
  }
}

print(as.data.frame(male.exp))     


# Calculate Test Statistic (Q)
q1 <- sum((male.obs[-3,-3] - male.exp)^2 / male.exp)

# Calculate the Critical Value/Rejection Region with 5% significance level
cv1 <- qchisq(df=(nrow(male.exp)-1)*(ncol(male.exp)-1), p=0.05, lower.tail = FALSE)

# Calculate the p-value with 5% significance level
pval1 <- pchisq(q=q1, df=(nrow(male.exp)-1)*(ncol(male.exp)-1), lower.tail = FALSE)

#Test if Test Statistic is Greater than the Critical Value/Rejection Region
q1 > cv1

# Calculation of the Odds Ratio
OR1 <- (male.obs[1,1]*male.obs[2,2])/(male.obs[2,1]*male.obs[1,2])
cat('For men, the odds of having a Poor/Fair/Good diet for those who tested negative for an STI is ',OR1,'times lower than
for those who test positive for an STI.')

# Calculation of the 95% confidence interval fOR1 the Odds Ratio
se1 <- sqrt((1/male.obs[1,1])+(1/male.obs[2,1])+(1/male.obs[1,2])+(1/male.obs[2,2]))
lower1 <- log(OR1) - 1.96*se1
upper1 <- log(OR1) + 1.96*se1

cat('The Test Statistic ',q1,' which under the Null is within a chi-squared distribution does not reach the Critical Value/Rejection Region of (',cv1,') \n 
    With a significance level of 0.05, the p-value ',pval1,' is greater. As such we have very little evidence to reject the Null Hypothesis.\n
    Thus, we conclude that the true healthy diet proportion of males is homogenous across STI Status.')
cat('We are 95% confident that the outcome level for the negative STI group is between (',((1-exp(lower1))*100),'%) lower \n
    and ',exp(upper1),' times higher than the positive STI group for having a poor/fair/good diet for men.')

# Create Matrix of Females by STI Status and Diet
female.obs <- matrix(nrow=3,ncol=3)
row.names(female.obs)<- c('Negative','Positive','Total')
colnames(female.obs)<- c('Poor/Fair/Good','Very good/Excellent','Total')
female.obs[1,1] <- 374-78
female.obs[1,2] <- 78
female.obs[1,3] <- 374
female.obs[2,1] <- 381-72
female.obs[2,2] <- 72
female.obs[2,3] <- 381
female.obs[3,1] <- (374-78) + (381-72)
female.obs[3,2] <- 78 + 72
female.obs[3,3] <- 374 + 381

#Create matrix of expected counts of subjects
female.exp <- matrix(nrow=2, ncol=2)
rownames(female.exp) <-c('Negative','Positive')
colnames(female.exp) <- c('Poor/Fair/Good','Very good/Excellent')
for (j in 1:2){
  m <- female.obs[3, j]
  
  for (i in 1:2){
    
    n <- (female.obs[i, 3]*m)/female.obs[3,3]
    female.exp[i,j] <- n
  }
}

print(female.exp)      


# Calculate Test Statistic (Q)
q2 <- sum((female.obs[-3,-3] - female.exp)^2 / female.exp)

# Calculate the Critical Value/Rejection Region with 5% significance level
cv2 <- qchisq(df=(nrow(female.exp)-1)*(ncol(female.exp)-1), p=0.05, lower.tail = FALSE)

# Calculate the p-value with 5% significance level
pval2 <- pchisq(q=q2, df=(nrow(female.exp)-1)*(ncol(female.exp)-1), lower.tail = FALSE)

#Test if Test Statistic is Greater than the Critical Value/Rejection Region
q2 > cv2

# Calculation of the Odds Ratio
OR2 <- (female.obs[1,1]*female.obs[2,2])/(female.obs[2,1]*female.obs[1,2])
cat('For women, the odds of having a Poor/Fair/Good diet for those who tested negative is ',OR2,' times lower than
    for those who test positive.')

# Calculation of the 95% confidence interval fOR2 the Odds Ratio
se2 <- sqrt((1/female.obs[1,1])+(1/female.obs[2,1])+(1/female.obs[1,2])+(1/female.obs[2,2]))
lower2 <- log(OR2) - 1.96*se2
upper2 <- log(OR2) + 1.96*se2

cat('The Test Statistic (',q2,') which under the Null is within a chi-squared distribution does not reach the Critical Value/Rejection Region of (',cv2,') \n 
    With a significance level of 0.05, the p-value (',pval2,') is greater. As such we have very little evidence to reject the Null Hypothesis.\n
    Thus, we conclude t hat the true healthy diet proportion of females is homogenous across STI Status.')
cat('We are 95% confident that the outcome level for the negative STI group is between ',((1-exp(lower2))*100),' lower \n
    and ',exp(upper2),' times higher than the positive STI group for having a poor/fair/good diet for women.')

##Two-Sample Z-Test for Vigorous Physical Activity and STI Status
cat('Null: The True Vigorous Physical Activity Proportion of Subjects are equal across STI Status.')
cat('Null: The True Vigorous Physical Activity Proportion of Subjects are NOT equal across STI Status.')


physcombo$Positive.Test[physcombo$Positive.Test %in% c(2,3,4,5)] = 1

#Subsetted physcombo dataframes by 'Activity' for proportion comparison
sedentary <- physcombo[physcombo$Activity == 'sedentary',] # Only includes 'sedentary' participants
moderate <- physcombo[physcombo$Activity == 'moderate',] # Only includes 'vigorous' participants
vigorous <- physcombo[physcombo$Activity == 'vigorous',] # Only includes 'moderate' participants

#Subsetted sedentary, moderate and vigorous dataframes by 'RIAGENDR' for proportion comparison
sed_male <- sedentary[sedentary$RIAGENDR == 'Male',]
sed_female <- sedentary[sedentary$RIAGENDR == 'Female',]
mod_male <- moderate[moderate$RIAGENDR == 'Male',]
mod_female <- moderate[moderate$RIAGENDR == 'Female',]
vig_male <- vigorous[vigorous$RIAGENDR == 'Male',]
vig_female <- vigorous[vigorous$RIAGENDR == 'Female',]

#Numerical Values for Proportion  Test
sed_maletotal <- nrow(sedentary[sedentary$RIAGENDR == 'Male',]) # Total Number of Male participants with Sedentary Activity
sed_femaletotal <- nrow(sedentary[sedentary$RIAGENDR == 'Female',]) # Total Number of Female participants with Sedentary Activity
mod_maletotal <- nrow(moderate[moderate$RIAGENDR == 'Male',]) # Total Number of Male participants with Moderate Activity
mod_femaletotal <- nrow(moderate[moderate$RIAGENDR == 'Female',]) # Total Number of Female participants with Moderate Activity
vig_maletotal <- nrow(vigorous[vigorous$RIAGENDR == 'Male',]) # Total Number of Male participants with Vigorous Activity
vig_femaletotal <- nrow(vigorous[vigorous$RIAGENDR == 'Female',]) # Total Number of Female participants with Vigorous Activity

sed_malepos <- sum(sed_male$Positive.Test[sed_male$Activity == 'sedentary']) # Total Number of STI Positive Male participants w/ Sedentary Activity
sed_femalepos <- sum(sed_female$Positive.Test[sed_female$Activity == 'sedentary']) # Total Number of STI Positive Female participants w/ Sedentary Activity
mod_malepos <- sum(mod_male$Positive.Test[mod_male$Activity == 'moderate']) # Total Number of STI Positive Male participants w/ Moderate Activity
mod_femalepos <- sum(mod_female$Positive.Test[mod_female$Activity == 'moderate']) # Total Number of STI Positive Female participants w/ Moderate Activity
vig_malepos <- sum(vig_male$Positive.Test[vig_male$Activity == 'vigorous']) # Total Number of STI Positive Male participants w/ Vigorous Activity
vig_femalepos <- sum(vig_female$Positive.Test[vig_female$Activity == 'vigorous']) # Total Number of STI Positive Female participants w/ Vigorous Activity

#Two-Sample Z-Tests comparing Moderate or Vigorous Activity to Sedentary Activity, separated by gender
z.modmale <- prop.test(x=c(sed_malepos, mod_malepos), n=c(sed_maletotal, mod_maletotal), alternative = 'greater', correct = FALSE)
z.modfemale <- prop.test(x=c(sed_femalepos, mod_femalepos), n=c(sed_femaletotal, mod_femaletotal), alternative = 'greater',correct = FALSE)
z.vigmale <- prop.test(x=c(sed_malepos, vig_malepos), n=c(sed_maletotal, vig_maletotal), alternative = 'greater',correct = FALSE)
z.vigfemale <- prop.test(x=c(sed_femalepos, vig_femalepos), n=c(sed_femaletotal, vig_femaletotal), alternative = 'greater',correct = FALSE)

tstat1 <- c(z.modmale$statistic, z.modfemale$statistic) # stores Test Statistic under Chi-squared distribution for moderate activity
pvalue1 <- c(z.modmale$p.value, z.modfemale$p.value) # Stores P-values for moderate activity
tstat2 <- c(z.vigmale$statistic, z.vigfemale$statistic) # stores Test Statistic under Chi-squared distribution for vigorous activity
pvalue2 <- c(z.vigmale$p.value, z.vigfemale$p.value) # Stores P-values for vigorous activity

cat('With the Null underneath the chi-squared distribution, the Test Statistics from comparing Moderate Activity for men and women is ',tstat1,'respectively.')
cat('With the Null underneath the chi-squared distribution, the Test Statistics from comparing Vigorous Activity for men and women is ',tstat2,'respectively.')

cat('The p-values from commparing Moderate Activity for men and women is ',pvalue1,'respectively.')
cat('The p-values from commparing Vigorous Activity for men and women is ',pvalue2,'respectively.')

cat('These results suggest that we have little to no evidence against the null for men regardless of physical activity, \n
    however there is suggestive evidence against the null for women with moderate activity and strong evidence against the null for women with vigorous activity.')

cat('As such, we REJECT THE NULL only for the tests comparing women with vigorous activity vs those who are sedentary. \n
    We FAIL TO REJECT for every other test. We conclude that the true proportion of STI Positive Status women are not equal between those who are \n
    sedentary and those participate in vigorous activity.')

cat('We are 95% confident that the true difference in proportion in STI Positive Status between women who are sedentary and women who participate in vigorous activity is ', z.vigfemale$conf.int,'.')
