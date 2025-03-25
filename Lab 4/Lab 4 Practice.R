#Lab 4 Practice

#2.1 Binomial Distribution

install.packages('gridExtra')

#Example: tossing a coin If we toss a coin 10 times, how many times would we expect to get heads?
x1 <- rbinom(n=5, size=10, prob=0.50)
x2 <- rbinom(n=30, size=10, prob=0.50)
x3 <- rbinom(n=500, size=10, prob=0.50)

mydf1 <- data.frame(x1 = factor(x1, levels=0:10))
mydf2 <- data.frame(x2 = factor(x2, levels=0:10))
mydf3 <- data.frame(x3 = factor(x3, levels=0:10))

p1<-ggplot(data=mydf1, aes(x=x1)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="dodgerblue", alpha=0.5)+
  scale_y_continuous(labels = scales::percent) + 
  ylab("percent") + 
  xlab("X") + scale_x_discrete(drop=FALSE) +
  ggtitle("5 simulations") + 
  theme_minimal()

p2<-ggplot(data=mydf2, aes(x=x2)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="dodgerblue", alpha=0.5)+
  scale_y_continuous(labels = scales::percent) + 
  ylab("percent") + 
  xlab("X") + scale_x_discrete(drop=FALSE) +
  ggtitle("30 simulations") + 
  theme_minimal()

p3<-ggplot(data=mydf3, aes(x=x3)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="dodgerblue", alpha=0.5)+
  scale_y_continuous(labels = scales::percent) + 
  ylab("percent") + 
  xlab("X") + scale_x_discrete(drop=FALSE) +
  ggtitle("500 simulations") + 
  theme_minimal()

grid.arrange(p1, p2, p3, ncol=3)

quantile(x3, c(0.025, 0.975))
#n the set of 500 simulations, 95% of the simulated values were between 2 and 8.
#2.2 Poisson

dpois(x=c(1:10), lambda=2)
ppois(q=c(1:10), lambda=2)

x1 <- dpois(x=c(0:10), lambda=5)
x2 <- ppois(q=c(0:10), lambda=5)
plot(x2 ~ c(0:10), xlab="X", ylab="P(X=x)", type="l", col="red", main="X~POI(5)")
points(x2 ~ c(0:10), pch=19,  col="red")
points(x1 ~ c(0:10), pch=17,  col="dodgerblue")

##Example: Birth rate In a recent year, there were 4,229 births at NYU Langone Medical Center (according to the NYU Langone website). If we assume the number of births each day is about the same, and that the Poisson distribution is an OK model for the number of births,

##What is the mean?
  
##mean=lamba*t is the average number of occurrences of the event in the interval. Our interval of interest is one day so we can calculate the average number of births per day mean=(4229/365)

##What is the probability that on a randomly selected day, there are exactly 10 births?

dpois(x=10, lambda=(4229/365))

##Calculate the probability that on a randomly selected day, there are 5 or fewer births.
ppois(q=5, lambda=(4229/365))

#2.3 Practice

#(i)For each of the following Binomial random variables, calculate E(X), Var(X), P(X<=3), P(X=0), and P(X>9). Which binomial random variables have the largest expected value? variance?

# X~BIN(n=10,p=0.01)
# X~BIN(n=10,p=0.1)
# X~BIN(n=10,p=0.4)
# X~BIN(n=10,p=0.9)

cat('The E(X) would',10*(c(0.01,0.1,0.4,0.9)),' be respectively.
    With X~BIN(n=10, p=0.9) having the largest expected value')

cat('The Var(X) would be',10*(c(0.01*(1-0.01),0.1*(1-0.1),0.4*(1-0.4),0.9*(1-0.9))),'respectively.
With X~BIN(n=10, p=0.4) having the largest Variance.')

p0 <-c(dbinom(0, size=10, p=0.01), 
  dbinom(0, size=10, p=0.1),
dbinom(0, size=10, p=0.4),
dbinom(0, size=10, p=0.9)) ##vector of P(X=0) values

peq3 <- c(pbinom(3, 10, 0.01), 
          pbinom(3, 10, 0.1),
          pbinom(3, 10, 0.4),
          pbinom(3, 10, 0.9)) ##vector of P(X<=3) values

pgr9 <- c(pbinom(9, 10, 0.01, lower.tail = FALSE), 
          pbinom(9, 10, 0.1, lower.tail = FALSE),
          pbinom(9, 10, 0.4, lower.tail = FALSE),
          pbinom(9, 10, 0.9, lower.tail = FALSE)) ##vector of P(X>9) values

cat('The P(X<=3),are', peq3,'in respective order.')
cat('The P(X=0), are', p0,'in respective order.')
cat('The P(X>9) are', pgr9,'in respective order.')


#Now, for each of the four binomial random variables above, simulate 1000 outcomes. Use these simulations to estimate the same quantities: 
# E(X), Var(X), P(X<=3), P(X=0), and P(X>9). How do the results compare to the theoretical values?



rndm <- c(rbinom(1000,10,0.01),
          rbinom(1000,10,0.1),
          rbinom(1000,10,0.4),
          rbinom(1000,10,0.9)) ##1000 simulations of each of the 4 above binomial variables

expval <- c(mean(rndm[1:1000]), mean(rndm[1001:2000]),mean(rndm[2001:3000]), mean(rndm[3001:4000]))
#Expected Values from the 4 binomial random variables, in order.

varval <- c(var(rndm[1:1000]), var(rndm[1001:2000]), var(rndm[2001:3000]), var(rndm[3001:4000]))
#Variance from the 4 binomial random variables, in order.

leq3 <- c(sum(rndm[1:1000] <=3)/1000,sum(rndm[1001:2000] <=3)/1000, sum(rndm[2001:3000]<=3)/1000, sum(rndm[3001:4000]<=3)/1000)
#Probabilities of P(<=3) for the 4 binomial random variables, in order.

eq0 <- c(sum(rndm[1:1000] ==0)/1000,sum(rndm[1001:2000] ==0)/1000, sum(rndm[2001:3000]==0)/1000, sum(rndm[3001:4000]==0)/1000)
#Probabilities of P(=0) for the 4 binomial random variables, in order.

gr9 <- c(sum(rndm[1:1000] >9)/1000,sum(rndm[1001:2000] >9)/1000, sum(rndm[2001:3000]>9)/1000, sum(rndm[3001:4000]>9)/1000)
#Probabilities of P(>9) for the 4 binomial random variables, in order.

cat('The E(X) would',expval,' be respectively.
    With X~BIN(n=10, p=0.9) having the largest expected value')

cat('The Var(X) would be',varval,'respectively.
With X~BIN(n=10, p=0.4) having the largest Variance.')

cat('The P(X<=3),are', leq3,'in respective order.')
cat('The P(X=0), are', eq0,'in respective order.')
cat('The P(X>9) are', gr9,'in respective order.')

cat('Overall, the 1000 simulated outcomes are very similar to the theoretical outcomes.')

#(ii) Suppose there are an average of 2.5 accidents per week on Forbes Avenue
# between Stanwix Street (Downtown) and Murray Avenue (Squirrel Hill).

#(a) Find the probability that at least 5 accidents occur during the next 3 weeks.

#(b) How many accidents do we expect on that stretch of Forbes Avenue in one year?
#    What is the corresponding standard deviation?

#3.1 Normal Distribution

