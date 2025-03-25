# Lab 8 Practice
library(ggplot2)
load('quiz.RData')

#(1) Perform a paired t test to address the research question
View(quiz) #confirm format of data is wide

#Paired t.test between 1st and 2nd quiz results
t.test(quiz$first.quiz, quiz$second.quiz, paired = TRUE)

#(2)Perform a randomization test to address the research question

#Create 'shuffle_pairs' function
shuffle_pairs <- function(dat, col1, col2){
  shuffled <- t(apply(dat[,c(col1, col2)], 1, function(i) sample(i, 2, replace = FALSE)))
  colnames(shuffled) <- paste0(c(col1, col2), ".shuffled")
  new.dat <- as.data.frame(cbind(dat, shuffled))
  return(new.dat)
}

#Shuffle pairs
shuffled <- shuffle_pairs(quiz, 'first.quiz', 'second.quiz')
View(shuffled)

#Perform paired t-test on shuffled results
t.test(shuffled$first.quiz.shuffled, shuffled$second.quiz.shuffled, paired=TRUE)

#Randomization
n.sims <- 10000
t.stats <- rep(NA, n.sims)

for (i in 1:n.sims){
  shuffled <- shuffle_pairs(quiz, 'first.quiz','second.quiz')
  t.stats[i] <- t.test(shuffled$first.quiz.shuffled,shuffled$second.quiz.shuffled, paired = TRUE)$statistic
}

View(t.stats)

#Plot randomization results
ggplot(data=as.data.frame(t.stats), aes(x=t.stats)) + 
  geom_histogram(binwidth = 0.25)
observed <- t.test(quiz$first.quiz - quiz$second.quiz)$statistic
table(t.stats > abs(observed) | t.stats < -1*abs(observed)) %>%  prop.table()

ggplot(data=as.data.frame(t.stats), aes(x=t.stats)) + 
  geom_histogram(aes(fill=as.factor(t.stats > abs(observed) | t.stats < -1*abs(observed)), binwidth = 0.25), colour='black') + 
  scale_fill_discrete(name="Test Statistic", labels=c("Less extreme", "As or more extreme"))

  