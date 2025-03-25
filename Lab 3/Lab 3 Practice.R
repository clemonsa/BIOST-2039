#Lab 3 Practice

#3.4 Practice
#(1) The probability that both offspring will have BB genotype
offspring1 <- sample(x=c('AA','AB','BA','BB'),10000, replace=TRUE)
offspring2 <- sample(x=c('AA','AB','BA','BB'), 10000, replace =TRUE)
Tbl1 <- prop.table(table(offspring1))
Tbl2 <- prop.table(table(offspring2))
bothBB <- Tbl1[4]*Tbl2[4]
print(bothBB)


#(2) The probability that at least one of the two offspring will have a BB genotype
atleastoneBB <- Tbl1[4]+Tbl2[4] - Tbl1[4]*Tbl2[4]
print(atleastoneBB)

#(3) The probability that exactly one of the offspring will have a BB genotype
exactlyoneBB <- Tbl1[4]*sum(Tbl2[1:3]) + sum(Tbl1[1:3])*Tbl2[4]
print(exactlyoneBB)

#(4) The probability that neither offspring will have the BB genotype
noneBB <- sum(Tbl1[1:3])*sum(Tbl2[1:3])
print(noneBB)

#Now suppose that they have a total of 6 offspring. Use the relative frequency
#approach to probability to estimate the following probabilities.

offspring3 <- sample(x=c('AA','AB','BA','BB'),10000, replace=TRUE)
offspring4 <- sample(x=c('AA','AB','BA','BB'), 10000, replace =TRUE)
offspring5 <- sample(x=c('AA','AB','BA','BB'),10000, replace=TRUE)
offspring6 <- sample(x=c('AA','AB','BA','BB'), 10000, replace =TRUE)

Tbl3 <- prop.table(table(offspring3))
Tbl4 <- prop.table(table(offspring4))
Tbl5 <- prop.table(table(offspring5))
Tbl6 <- prop.table(table(offspring6))

#(5) The probability that all offspring will have BB genotype
allBB <- Tbl1[4]*Tbl2[4]*Tbl3[4]*Tbl4[4]*Tbl5[4]*Tbl6[4]
print(allBB)

#(6) The probability that at least one offspring will have a BB genotype
noneof6BB <- sum(Tbl1[1:3])*sum(Tbl2[1:3])*sum(Tbl3[1:3])*sum(Tbl4[1:3])*sum(Tbl5[1:3])*sum(Tbl6[1:3])
print(1-noneof6BB)

#(7) The probability that at least two will have BB genotype


#(8) The probability that no offspring will have the BB genotype
print(noneof6BB)

#5.1 Practice
install.packages('tidyverse')
library(tidyverse)
load('Framingham.Rdata')
scl.complete <- stata_data[complete.cases(stata_data$scl),]
View(scl.complete)

#(1)
n <- nrow(scl.complete)
boots1 <- data.frame(sim.num=1:n, means=rep(NA, n)) # create a data frame to store results in 

nboot1 <- 1000 # number of bootstrap samples
for(i in 1:nboot1){
  boot1.sample <- scl.complete[sample(x=1:n, size=n, replace=TRUE), ] # obtain the bootstrap sample
  boots1$means[i] <- mean(boot1.sample$scl) # record the 90th percentile of the bootstrap sample
}

table(boots1$means > 200) %>% prop.table()

ggplot(boots1, aes(x=means)) + geom_histogram(aes(fill=as.factor(means > 200)))

#The distribution shows that across all sampling, the mean SCL levels have exceeded 200.
#Even further the largest distribution of mean SCL is a little less than 228.5

#(2)
boots2 <- data.frame(sim.num=1:n, medians=rep(NA, n)) # create a data frame to store results in 

nboot2 <- 1000 # number of bootstrap samples
for(i in 1:nboot2){
  boot2.sample <- scl.complete[sample(x=1:n, size=n, replace=TRUE), ] # obtain the bootstrap sample
  boots2$medians[i] <- median(boot2.sample$scl) # record the 90th percentile of the bootstrap sample
}

table(boots2$medians > 200) %>% prop.table()

ggplot(boots2, aes(x=medians)) + geom_histogram(aes(fill=as.factor(medians > 200)))

#The distribution shows that across all sampling, the mediam SCL levels have exceeded 200.
#Even further, the largest distribution event has a median SCL of 225.