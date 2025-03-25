#Lab 5 Practice

#2.6 Practice

##For these practice problems, assume that the population of human body temperatures is normally distributed
## with mean 98.6 degrees Fahrenheit and a standard deviation of 0.62 degrees Fahrenheit.

set.seed(09262019)
library(tidyverse)
library(ggplot2)


##(i) Find P(X < 98.2) through simulation of 1000 individuals
##calculate proportion of them fall within the desired boundaries and create histogram
##highlighting those that fall within the desired boundaries

rndm1 <- rnorm(1000,98.6,0.62)
# random normal distribution with mean 98.6 degrees F and sd of 0.62 degrees
prop1 <- sum(rndm1 < 98.2)/1000
#Stores proportion from rndm1 that are < 98.2

p1 <- ggplot(as.data.frame(rndm1), aes(x=rndm1)) + 
         geom_histogram(aes(fill=as.factor(rndm1 < 98.2 )))
#Create histogram highlighting proportion that is P(X < 98.2)

plot(p1)

##(ii) Calculate P(X < 98.2) and compare to (i)

less982 <- pnorm(98.2, 98.6, 0.62)

cat('The calculated probability of P(X < 98.2) is',less982,'in comparison to the simulated probability of',prop1,'.')

##(iii) Find P(X < 98.2) through simulation of 106 individuals from 1000 random samples and
##for each sample store the mean body temp only. Calculate the proportion of them fall
##within the desired boundaries. Create a histogram, highlighting those that fall within the boundaries
##Based on this, what are the plausible values for the sample mean body temp from a random sample of 106 indv?

rndm2 <- rnorm(1000, 98.6, 0.62/sqrt(106))
#random normal distribution of mean body temp from 106 individuals, with a mean of 98.6 and sd of 0.62

prop2 <- sum(rndm2 < 98.2)/1000
#Stores proportion from rndm that are < 98.2

p2 <- ggplot(as.data.frame(rndm2), aes(x=rndm2)) + 
  geom_histogram(aes(fill=as.factor(rndm2 < 98.2 )))
#Create histogram highlighting proportion that is P(X < 98.2)

plot(p2)

cat('Based on the simulation of 106 individuals, the plausible values for the sample mean body temp are all above 98.2.')


##(iv) Calculate P(Xavg < 98.2) Compare to (iii)

avgless982 <- pnorm(98.2, 98.6, 0.62/sqrt(106))

cat('The calculated probability of P(X < 98.2) is',avgless982,'in comparison to the simulated probability of',prop2,'.')

##(v)A random sample of 106 adults body temperatures were recorded.
##The mean body temp was 98.2, how unlikely would it be to get a sample mean of 98.2 or less if
##the population mean body temperature was actually 98.6?
##Does your answer depend on whether the population body temperaturs are normally distributed?

cat('It is extremely unlikely to get a sample mean of 98.2 from 106 adults, as demonstrated in part (iii) the plausible values for 106 individuals ranges from 98.4 and above')
