##Lab 7 Practice

df <- read.csv('./mamexp.csv', as.is = TRUE)


# Does a woman's mammography experience depend on her family history of breast cancer?

## Use a Test of Independence to determine this

cat('Null Hypothesis: A womans mammography experience is independent of her family history for breast cancer.

Alternative Hypothesis: A womans mammography experience is NOT independent on her family history for breast cancer.')

tbl1 <- xtabs(~me+famhist, data=df)
#Read 'mamexp.csv' and create table

tbl2 <- matrix(c(85, 63, 220, 85 + 63 +220, 19, 11, 14, 19 + 11 + 14, 85 + 19, 63 + 11, 220 + 14, 85 + 63 + 220 + 19 + 11 + 14), ncol = 3)
colnames(tbl2) <- c('No','Yes','Total')
rownames(tbl2) <- c('<1yr', '>1yr','Never','Total')
#prop.table(as.table(tbl2[-4,-3]))
#Create table of observation counts

expected1 <- matrix(c(104*(368/412),74*(368/412),234*(368/412)))
expected2 <- matrix(c(104*(44/412),74*(44/412),234*(44/412)))

tbl3 <- cbind(expected1, expected2)
colnames(tbl3) <- c('No','Yes')
rownames(tbl3) <- c('<1yr','>1yr','Never')
#Create table of expected values

q1 <- sum((tbl2[-4,-3] - tbl3)^2 / tbl3)
#Test Statistic calculation

cval <- qchisq(df=((nrow(tbl3)-1)*(ncol(tbl3)-1)),p=0.05, lower.tail=FALSE)
pval <- pchisq(q=q1,df=((nrow(tbl3)-1)*(ncol(tbl3)-1)), lower.tail = FALSE)
#Critical Value/Rejection Region and p-value for Test Statistic
q1 > cval
cat('Our Test Statistic of',q1,' is greater than the Critical Value',cval,' and thus is within the rejection region. Also our p-value is',pval,' and thus we reject the null hypothesis.')

# Is a woman's response to "You do not need a mammogram unless you develop symtoms"
# related to her mammography experience?

cat('Null Hypothesis: A womans response to "You do not need a mammogram unless you develop symptoms" is
unrelated to her mammography experience')

cat('Alternative Hypothesis: A womans response to "You do not need a mammogram unless you develop symptoms" is
NOT unrelated to her mammography experience.')

tbl4 <- xtabs(~me + sympt, data=df)
tbl5 <- cbind(tbl4)
tbl5 <- rbind(tbl5, 5)
tbl5 <- cbind(tbl5,6)
colnames(tbl5) <- c('agree','disagree','strongly agree','strongly disagree', 'Total')
rownames(tbl5) <-c('<1yr' ,'>1yr','never','Total')
for (i in 1:4){
  tbl5[4, i] <- sum(tbl5[1:3,i])
}

for(i in 1:4){
  tbl5[i, 5] <- sum(tbl5[i, 1:4])
}
#Create table for 'me' and 'sympt' variables from df of observations
#, summing up values into Totals by column and row
tbl6 <- matrix(nrow=3, ncol=4)
colnames(tbl6) <- c('agree','disagree','strongly agree','strongly disagree')
rownames(tbl6) <-c('<1yr' ,'>1yr','never')
for (j in 1:4){

 m <- tbl5[4, j]
   
  for (i in 1:3){
    
 n <- (tbl5[i, 5]*m)/tbl5[4,5]
    print(n)
    tbl6[i,j] <- n
  }
}
#Create table of Expected Values

q2 <- sum((tbl5[-4,-3] - tbl6)^2 / tbl6)
#Test Statistic calculation

cval2 <- qchisq(df=((nrow(tbl6)-1)*(ncol(tbl6)-1)),p=0.05, lower.tail=FALSE)
pval2 <- pchisq(q=q2,df=((nrow(tbl6)-1)*(ncol(tbl6)-1)), lower.tail = FALSE)
#Critical Value/Rejection Region and p-value for Test Statistic

cat('Our Test Statistics of',q2,' is greater than the Critical Value',cval2,' and thus is within the rejection region. Also our p-value is',pval2,' and thus we reject the null hypothesis.')

# Does a woman's opinion on mammography efficacy (i.e., how likely it is that a mammogram could find a new case of breast cancer)
# influence her mammography experience?

cat('Null Hypothesis:A womans opinion on mammography efficacy (i.e., how likely it is that a mammogram could find a new case of breast cancer) 
    has no influence her mammography experience')

cat('Alternative Hypothesis: A womans opinion on mammography efficacy (i.e., how likely it is that a mammogram could find a new case of breast cancer) has influence on 
    her mammography experience')

tbl7 <- xtabs(~me + detc, data=df)
tbl8 <- cbind(tbl7)
tbl8 <- rbind(tbl8, 5)
tbl8 <- cbind(tbl8,5)
colnames(tbl8) <- c('not likely','somewhat likely','very likely', 'Total')
rownames(tbl8) <-c('<1yr' ,'>1yr','never','Total')
for (i in 1:3){
  tbl8[4, i] <- sum(tbl8[1:3,i])
}

for(i in 1:4){
  tbl8[i, 4] <- sum(tbl8[i, 1:3])
}
#Create table for 'me' and 'detc' variables from df of observations
#, summing up values into Totals by column and row

tbl9 <- matrix(nrow=3, ncol=3)
colnames(tbl9) <- c('not likely','somewhat likely','very likely')
rownames(tbl9) <-c('<1yr' ,'>1yr','never')
for (j in 1:3){
  
  m <- tbl8[4, j]
  
  for (i in 1:3){
    
    n <- (tbl8[i, 4]*m)/tbl8[4,4]
    print(n)
    tbl9[i,j] <- n
  }
}
#Create table of Expected Values

q3 <- sum((tbl8[-4,-4] - tbl9)^2 / tbl9)
#Test Statistic calculation

cval3 <- qchisq(df=((nrow(tbl9)-1)*(ncol(tbl9)-1)),p=0.05, lower.tail=FALSE)
pval3 <- pchisq(q=q3,df=((nrow(tbl9)-1)*(ncol(tbl9)-1)), lower.tail = FALSE)
#Critical Value/Rejection Region and p-value for Test Statistic

cat('Our Test Statistics of',q3,' is greater than the Critical Value',cval3,' and thus is within the rejection region. Also our p-value is',pval3,' and thus we reject the null hypothesis.')
