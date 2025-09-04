#### basic t-test simulation #### 
# (for more info see https://www.r-bloggers.com/2020/05/power-analysis-by-data-simulation-in-r-part-ii/)
# This is for two groups with known mean and SD (example uses fixed sample size for now, to demonstrate)
set.seed(1)
n_sims <- 1000 #number of simulations to run
p_vals <- c() #initialize collection of p_vals
for(i in 1:n_sims){
  group1 <- rnorm(30,1,2) # group 1 for 30 subjects, mean of 1 and SD of 2
  group2 <- rnorm(30,0,2) # group 2 for 30 subjects, mean of 1 and SD of 2
  p_vals[i] <- t.test(group1, group2, paired=FALSE,var.equal=TRUE, conf.level = 0.95)$p.value
}
mean(p_vals < .05) # calculate power as the proportion of p-values smaller than .05

#### 2 group t-test for a variety of increasing sample sizes ####
# looping through different sample sizes (slow)
set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
power_at_n <- c(0) # this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
cohens_ds <- c()
cohens_ds_at_n <- c() 
n <- 30 # sample-size 
i <- 2
while(power_at_n[i-1] < .80){ # <-- we want power to be at least 80%
  for(sim in 1:n_sims){
    group1 <- rnorm(n,1,2) # simulate group 1 ... note that sample size is now variable, and gets increased every 1000 (n_sims) number of simulations
    group2 <- rnorm(n,0,2) # simulate group 2
    p_vals[sim] <- t.test(group1, group2, paired = FALSE, var.equal = TRUE, conf.level = 0.95)$p.value # run t-test and extract the p-value
    cohens_ds[sim] <- abs((mean(group1)-mean(group2))/(sqrt((sd(group1)^2+sd(group2)^2)/2))) # we also save the cohens ds that we observed in each simulation
  }
  power_at_n[i] <- mean(p_vals < .05) # check power (i.e. proportion of p-values that are smaller than alpha-level of .05)
  cohens_ds_at_n[i] <- mean(cohens_ds) # calculate means of cohens ds for each sample-size
  n <- n+1 # increase sample-size by 1
  i <- i+1 # increase index of the while-loop by 1 to save power and cohens d to vector
}
power_at_n <- power_at_n[-1] # delete first 0 from the vector
cohens_ds_at_n <- cohens_ds_at_n[-1] # delete first NA from the vector
print(paste0("Sample size for 80% power = ",n-1," with Cohen's d = ",round(cohens_ds_at_n[i-2],2)))

plot(30:(n-1), power_at_n, xlab = "Number of participants per group", ylab = "Power", ylim = c(0,1), axes = TRUE)
abline(h = .80, col = "red")

plot(30:(n-1), cohens_ds_at_n, xlab = "Number of participants per group", ylab = "Cohens D", ylim = c(0.45,0.55), axes = TRUE)
abline(h = .50, col = "red")


#### One-sample repeated measure ####
# (test the T1 and T2 difference as greater than zero, which takes care of correlation/dependency, i.e., no need for correlated variables)
set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
power_at_n <- c(0) # this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
cohens_ds <- c()
cohens_ds_at_n <- c() 
n <- 20 # sample-size (this can be set by user )
i <- 2
while(power_at_n[i-1] < .80){
  for(sim in 1:n_sims){
    difference <- rnorm(n,1,2) # simulate the difference score distribution
    p_vals[sim] <- t.test(difference, mu = 0, conf.level = 0.95)$p.value # run t-test and extract the p-value
    cohens_ds[sim] <- mean(difference)/sd(difference) # we also save the cohens ds that we observed in each simulation 
  }
  power_at_n[i] <- mean(p_vals < .05) # check power (i.e. proportion of p-values that are smaller than alpha-level of .05)
  cohens_ds_at_n[i] <- mean(cohens_ds) # calculate means of cohens ds for each sample-size
  n <- n+1 # increase sample-size by 1 (can be set by user -- may be 5, 10, or whatever to give decent range)
  i <- i+1 # increase index of the while-loop by 1 to save power and cohens d to vector
}
power_at_n <- power_at_n[-1] # delete first 0 from the vector
cohens_ds_at_n <- cohens_ds_at_n[-1] # delete first NA from the vector

plot(2:(n-1), power_at_n, xlab = "Number of participants per group", ylab = "Power", ylim = c(0,1), axes = TRUE)
abline(h = .80, col = "red")

#### Correlated distribution simulation (same as above but now we are simulating the correlation between 2 variables (pre and post)) ####
require(MASS) # load MASS package
## Loading required package: MASS
pre_post_means <- c(pre = 0,post = 1) # define means of pre and post in a vector
pre_sd <- 2 # define sd of pre-measure
post_sd <- 2 # define sd of post-measure
correlation <- 0.5 # define their correlation

sigma <- matrix(c(pre_sd^2, pre_sd*post_sd*correlation, pre_sd*post_sd*correlation, post_sd^2), ncol = 2) # define variance-covariance matrix

set.seed(1)
bivnorm <- data.frame(mvrnorm(10000, pre_post_means, sigma)) # simulate bivariate normal
cor.test(bivnorm$pre,bivnorm$post)

# check out the correlation between the two distributions in 3 dimensions!!
bivnorm_kde <- kde2d(bivnorm[,1], bivnorm[,2], n = 50) # calculate kernel density (i.e. the "height of the cone on the z-axis"; not so important to understand here)
par(mar = c(0, 0, 0, 0)) # tel r not to leave so much space around the plot
persp(bivnorm_kde, phi = 45, theta = 30, xlab = "pre-measure", ylab = "post-measure", zlab = "frequency") # plot the bivariate normal

#Figuring out correlations btw pre and post surveys
names(echo_pre)[c(6:95,97:107)] = paste0('pre_',names(echo_pre)[c(6:95,97:107)])
names(echo_post)[c(6:95,97:107)] = paste0('post_',names(echo_post)[c(6:95,97:107)])
echo = merge(echo_pre[,c(3:4,96:107)],echo_post[,c(3:4,96:107)])
cor(echo$pre_Q41_avg,echo$post_Q41_avg,use="complete.obs")
cor(echo$pre_Q38_avg,echo$post_Q38_avg,use="complete.obs")
cor(echo$pre_q25_internal_count,echo$post_q25_internal_count)
cor(echo$pre_q34_internal_count,echo$post_q34_internal_count)
cor(echo$pre_q48_internal_count,echo$post_q48_internal_count)

# Now simulate bivariate normal-distribution, with 3 different possible correlations
# mu, sd, and cor based on consolidated same participants data
#Q41 
mu_pre_post <- c(pre = 3.29, post = 3.33) #mean of a question
sd_pre <- .528
sd_post <- .529 #sd of a question
correlations <- c(0.5, 0.58, 0.65)

set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
# this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
cohens_ds <- c()

powers_at_cor <- list()
cohens_ds_at_cor <- list()
library(MASS)
for(icor in 1:length(correlations)){ # do a power-simulation for each specified simulation
  n <- 14 # sample-size, I can play around by changing it to see how the power changes 
  i <- 2 # index of the while loop for saving things into the right place in the lists
  power_at_n <- c(0) 
  cohens_ds_at_n <- c() 
  sigma <- matrix(c(sd_pre^2, sd_pre*sd_post*correlations[icor], sd_pre*sd_post*correlations[icor], sd_post^2), ncol = 2) #var-covar matrix
  while(power_at_n[i-1] < 0.9){
    for(sim in 1:n_sims){
      bivnorm <- data.frame(mvrnorm(n, mu_pre_post, sigma)) # simulate the bivariate normal
      p_vals[sim] <- t.test(bivnorm$pre, bivnorm$post, paired = TRUE, var.equal = TRUE, conf.level = 0.95)$p.value # run t-test and extract the p-value
      cohens_ds[sim] <- abs((mean(bivnorm$pre)-mean(bivnorm$post))/(sqrt(sd(bivnorm$pre)^2+sd(bivnorm$post)^2-2*cor(bivnorm$pre, bivnorm$post)*sd(bivnorm$pre)*sd(bivnorm$post)))) # we also save the cohens ds that we observed in each simulation
    }
    power_at_n[i] <- mean(p_vals < .05) # check power (i.e. proportion of p-values that are smaller than alpha-level of .05)
    names(power_at_n)[i] <- n
    cohens_ds_at_n[i] <- mean(cohens_ds) # calculate means of cohens ds for each sample-size
    names(cohens_ds_at_n)[i] <- n
    n <- n+1 # increase sample-size by 1
    i <- i+1 # increase index of the while-loop by 1 to save power and cohens d to vector
  }
  power_at_n <- power_at_n[-1] # delete first 0 from the vector
  cohens_ds_at_n <- cohens_ds_at_n[-1] # delete first NA from the vector
  powers_at_cor[[icor]] <- power_at_n # store the entire power curve for this correlation in a list
  cohens_ds_at_cor[[icor]] <- cohens_ds_at_n # do the same for cohens d
  names(powers_at_cor)[[icor]] <- correlations[icor] # name the power-curve in the list according to the tested correlation
  names(cohens_ds_at_cor)[[icor]] <- correlations[icor] # same for cohens d
}

par(mfrow=c(1,3))
plot(2:(length(powers_at_cor$`0.5`)+1), powers_at_cor$`0.5`, xlab = "Number of participants", ylab = "Power",xlim=c(0,30),ylim = c(0,1), axes = TRUE, main = "correlation = 0.5")
abline(h = .80, col = "red")
plot(2:(length(powers_at_cor$`0.58`)+1), powers_at_cor$`0.58`, xlab = "Number of participants", ylab = "Power",xlim=c(0,30),ylim = c(0,1), axes = TRUE, main = "correlation = 0.58")
abline(h = .80, col = "red")
plot(2:(length(powers_at_cor$`0.65`)+1), powers_at_cor$`0.65`, xlab = "Number of participants", ylab = "Power",xlim=c(0,30),ylim = c(0,1), axes = TRUE, main = "correlation = 0.65")
abline(h = .80, col = "red")

# graph cohen's d
par(mfrow=c(1,3))
plot(2:(length(cohens_ds_at_cor$`0.5`)+1), cohens_ds_at_cor$`0.5`, xlab = "Number of participants", ylab = "Cohens D", ylim = c(0,1), axes = TRUE, main = "correlation = 0.5")
abline(h = .50, col = "red")
plot(2:(length(cohens_ds_at_cor$`0.58`)+1), cohens_ds_at_cor$`0.58`, xlab = "Number of participants", ylab = "Cohens D", ylim = c(0,1), axes = TRUE, main = "correlation = 0.58")
abline(h = .50, col = "red")
plot(2:(length(cohens_ds_at_cor$`0.65`)+1), cohens_ds_at_cor$`0.65`, xlab = "Number of participants", ylab = "Cohens D", ylim = c(0,10), axes = TRUE, main = "correlation = 0.65")
abline(h = .50, col = "red")

#Q38
mu_pre_post <- c(pre = 3.75, post = 4.09) #mean of a question
sd_pre <- .802
sd_post <- .51 #sd of a question
correlations <- c(0.5, 0.58, 0.65)

set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
# this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
cohens_ds <- c()

powers_at_cor <- list()
cohens_ds_at_cor <- list()
for(icor in 1:length(correlations)){ # do a power-simulation for each specified simulation
  n <- 14 # sample-size, I can play around by changing it to see how the power changes 
  i <- 2 # index of the while loop for saving things into the right place in the lists
  power_at_n <- c(0) 
  cohens_ds_at_n <- c() 
  sigma <- matrix(c(sd_pre^2, sd_pre*sd_post*correlations[icor], sd_pre*sd_post*correlations[icor], sd_post^2), ncol = 2) #var-covar matrix
  while(power_at_n[i-1] < .90){
    for(sim in 1:n_sims){
      bivnorm <- data.frame(mvrnorm(n, mu_pre_post, sigma)) # simulate the bivariate normal
      p_vals[sim] <- t.test(bivnorm$pre, bivnorm$post, paired = TRUE, var.equal = TRUE, conf.level = 0.95)$p.value # run t-test and extract the p-value
      cohens_ds[sim] <- abs((mean(bivnorm$pre)-mean(bivnorm$post))/(sqrt(sd(bivnorm$pre)^2+sd(bivnorm$post)^2-2*cor(bivnorm$pre, bivnorm$post)*sd(bivnorm$pre)*sd(bivnorm$post)))) # we also save the cohens ds that we observed in each simulation
    }
    power_at_n[i] <- mean(p_vals < .05) # check power (i.e. proportion of p-values that are smaller than alpha-level of .05)
    names(power_at_n)[i] <- n
    cohens_ds_at_n[i] <- mean(cohens_ds) # calculate means of cohens ds for each sample-size
    names(cohens_ds_at_n)[i] <- n
    n <- n+1 # increase sample-size by 1
    i <- i+1 # increase index of the while-loop by 1 to save power and cohens d to vector
  }
  power_at_n <- power_at_n[-1] # delete first 0 from the vector
  cohens_ds_at_n <- cohens_ds_at_n[-1] # delete first NA from the vector
  powers_at_cor[[icor]] <- power_at_n # store the entire power curve for this correlation in a list
  cohens_ds_at_cor[[icor]] <- cohens_ds_at_n # do the same for cohens d
  names(powers_at_cor)[[icor]] <- correlations[icor] # name the power-curve in the list according to the tested correlation
  names(cohens_ds_at_cor)[[icor]] <- correlations[icor] # same for cohens d
}

par(mfrow=c(1,3))
plot(2:(length(powers_at_cor$`0.5`)+1), powers_at_cor$`0.5`, xlab = "Number of participants", ylab = "Power", xlim=c(0,25),ylim = c(0,1), axes = TRUE, main = "correlation = 0.5")
abline(h = .80, col = "red")
plot(2:(length(powers_at_cor$`0.58`)+1), powers_at_cor$`0.58`, xlab = "Number of participants", ylab = "Power", xlim=c(0,25),ylim = c(0,1), axes = TRUE, main = "correlation = 0.58")
abline(h = .80, col = "red")
plot(2:(length(powers_at_cor$`0.65`)+1), powers_at_cor$`0.65`, xlab = "Number of participants", ylab = "Power", xlim=c(0,25),ylim = c(0,1), axes = TRUE, main = "correlation = 0.65")
abline(h = .80, col = "red")

#Q25
mu_pre_post <- c(pre = 3.62, post = 3.87) #mean of a question
sd_pre <- .779
sd_post <- .633 #sd of a question
correlations <- c(0.5, 0.58, 0.65)

set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
# this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
cohens_ds <- c()

powers_at_cor <- list()
cohens_ds_at_cor <- list()
for(icor in 1:length(correlations)){ # do a power-simulation for each specified simulation
  n <- 20 # sample-size, I can play around by changing it to see how the power changes 
  i <- 2 # index of the while loop for saving things into the right place in the lists
  power_at_n <- c(0) 
  cohens_ds_at_n <- c() 
  sigma <- matrix(c(sd_pre^2, sd_pre*sd_post*correlations[icor], sd_pre*sd_post*correlations[icor], sd_post^2), ncol = 2) #var-covar matrix
  while(power_at_n[i-1] < .90){
    for(sim in 1:n_sims){
      bivnorm <- data.frame(mvrnorm(n, mu_pre_post, sigma)) # simulate the bivariate normal
      p_vals[sim] <- t.test(bivnorm$pre, bivnorm$post, paired = TRUE, var.equal = TRUE, conf.level = 0.95)$p.value # run t-test and extract the p-value
      cohens_ds[sim] <- abs((mean(bivnorm$pre)-mean(bivnorm$post))/(sqrt(sd(bivnorm$pre)^2+sd(bivnorm$post)^2-2*cor(bivnorm$pre, bivnorm$post)*sd(bivnorm$pre)*sd(bivnorm$post)))) # we also save the cohens ds that we observed in each simulation
    }
    power_at_n[i] <- mean(p_vals < .05) # check power (i.e. proportion of p-values that are smaller than alpha-level of .05)
    names(power_at_n)[i] <- n
    cohens_ds_at_n[i] <- mean(cohens_ds) # calculate means of cohens ds for each sample-size
    names(cohens_ds_at_n)[i] <- n
    n <- n+1 # increase sample-size by 1
    i <- i+1 # increase index of the while-loop by 1 to save power and cohens d to vector
  }
  power_at_n <- power_at_n[-1] # delete first 0 from the vector
  cohens_ds_at_n <- cohens_ds_at_n[-1] # delete first NA from the vector
  powers_at_cor[[icor]] <- power_at_n # store the entire power curve for this correlation in a list
  cohens_ds_at_cor[[icor]] <- cohens_ds_at_n # do the same for cohens d
  names(powers_at_cor)[[icor]] <- correlations[icor] # name the power-curve in the list according to the tested correlation
  names(cohens_ds_at_cor)[[icor]] <- correlations[icor] # same for cohens d
}

par(mfrow=c(1,3))
plot(2:(length(powers_at_cor$`0.5`)+1), powers_at_cor$`0.5`, xlab = "Number of participants", ylab = "Power", xlim=c(0,30),ylim = c(0,1), axes = TRUE, main = "correlation = 0.5")
abline(h = .80, col = "red")
plot(2:(length(powers_at_cor$`0.58`)+1), powers_at_cor$`0.58`, xlab = "Number of participants", ylab = "Power", xlim=c(0,30),ylim = c(0,1), axes = TRUE, main = "correlation = 0.58")
abline(h = .80, col = "red")
plot(2:(length(powers_at_cor$`0.65`)+1), powers_at_cor$`0.65`, xlab = "Number of participants", ylab = "Power", xlim=c(0,30),ylim = c(0,1), axes = TRUE, main = "correlation = 0.65")
abline(h = .80, col = "red")

#Q34

#Q48
rm(mu_pre_post,sd_pre,sd_post,correlations,n_sims,p_vals,cohens_ds,powers_at_cor,
   cohens_ds_at_cor,power_at_n,cohens_ds_at_n,sigma,n,bivnorm,sim,icor)
