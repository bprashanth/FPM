# To determine if the poisson distriution models a normal distribution 
# we can perform a simple ks test and reject the null hypothesis if the 
# p[D > k] <= 0.05. In this case, D is 0.05 and the probability of 
# such a D value given the null hypothesis is 0.55 so we accept the null. 
# This just means that the test has told us the data is not not normal. 
means = c()
for (i in 1:500) {
  means <- c(means, mean(rpois(100, 5)))
}
hist(means)
print(mean(means))
print(sd(means))
ks.test(means, rnorm(length(means), mean(means), sd(means)))

sampleMeans= c()
for (i in 1:400) {
  sampleMeans <- c(sampleMeans, mean(
    sample(
      c(25:50), size = 150, replace = TRUE, 
      prob = c(rep(0.3, 6), rep(0.1, 5) , 
               rep(0.4, 5), rep(0.1, 5), rep(0.1, 5)))
  ))
}
hist(sampleMeans)

normalMeans <- c()
for (i in 1:400) {
  normalMeans <- c(normalMeans, 
                   mean(rnorm(150, mean(sampleMeans), sd(sampleMeans))))
}

s <- c()
s <- c(s, length(which(sampleMeans > 25 & sampleMeans < 30)))
s <- c(s, length(which(sampleMeans > 30 & sampleMeans < 35))) 
s <- c(s, length(which(sampleMeans > 35 & sampleMeans < 40)))
s <- c(s, length(which(sampleMeans > 40 & sampleMeans < 45)))
s <- c(s, length(which(sampleMeans > 45 & sampleMeans < 50)))

n <- c()
n <- c(n, length(which(normalMeans > 25 & normalMeans < 30)))
n <- c(n, length(which(normalMeans > 30 & normalMeans < 35))) 
n <- c(n, length(which(normalMeans > 35 & normalMeans < 40)))
n <- c(n, length(which(normalMeans > 40 & normalMeans < 45)))
n <- c(n, length(which(normalMeans > 45 & normalMeans < 50)))

chisq.test(s, n)
hist(normalMeans)