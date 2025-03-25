library(tibble)
library(Rcpp)

#Create tibble of data from above with expected juror counts
obs1 <- tribble(~'Age(yrs)',~'County-wide%',~'#jurors.obs','21-40',42,5,'41-50',23,9,'51-60',16,19,'over 60',19,33)
jurors.exp <- c(66*.42, 66*.23, 66*.16,66*.19)
obs1 <- add_column(.data = obs1, jurors.exp)

#Observed Probabilities and Expected Probabilities
prop.obs <- prop.table(obs1$`#jurors.obs`)
prop.exp <- prop.table(obs1$jurors.exp)

#Create matrix storing 1000 jurors at county-wide proportion
sim <- matrix(ncol = 4, nrow = 1000)
colnames(sim)<-c('21-40','41-50','51-60','over 60')
sim[1:(prop.exp[1]*1000),1] <- 1
sim[(prop.exp[1]*1000+1):(prop.exp[2]*1000+420),2] <- 1
sim[(prop.exp[2]*1000+421):(prop.exp[3]*1000+650),3] <- 1
sim[(prop.exp[3]*1000+651):(prop.exp[4]*1000+810),4] <- 1

nboot <- 10000 # number of bootstrap samples

# create a data frame to store juror categorical totals in
boot.total <- data.frame(sim.num=1:nboot, '21-40'=rep(NA,nboot),'41-50'=rep(NA,nboot),'51-60'=rep(NA,nboot),'over 60'=rep(NA,nboot))

# Simulate drawing random samples of 66 jurors from population at 10000 iterations

set.seed(04272000)

for (i in 1:nboot) {
  boot <- sim[sapply(nrow(sim), sample, size=66, replace = T),] #obtain the bootstrap sample
  for (j in 2:5){
    boot.total[i,j] <- sum(boot[,j-1],na.rm=TRUE)
  }
}

# Create a data frame to store Test Statistic from each simulation
boot.test <- data.frame(sim.num=1:nboot, Q=rep(NA,nboot))

# Calculate Test Statistic from each simulation

## for loop method
set.seed(04272000)
Start <- Sys.time()
for (i in 1:nboot){
  boot.test[i,2] <- sum(((boot.total[i,2:5] - jurors.exp)^2)/jurors.exp)
}
End <- Sys.time()
End - Start
## apply method
set.seed(04272000)
Start <- Sys.time()
boot.test[, 2] <- apply(boot.total[, 2:5], 1, function(x){sum((x - jurors.exp)^2/jurors.exp)})
End <- Sys.time()
End - Start