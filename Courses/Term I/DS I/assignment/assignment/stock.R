library(Quandl)
library(dplyr)

apiKey <- "emqe_JCJbwo6--VqWaLg"
symbol <- "NSE/ICICIBANK"
companyCSV <- "company.csv"
niftyCSV <- "nifty.csv"
nifty <- "NSE/NIFTY_50"
start <- "2016-09-09"
period <- "weekly"
classes <- c("Low", "Medium", "High")
N <- 106

Quandl.api_key(apiKey)
if ( !file.exists(companyCSV) ) {
  write.csv(Quandl(symbol, collapse = period, 
                   start_date = start, type = "raw"), 
            file = companyCSV)
} 
co <- read.csv(companyCSV, header = TRUE)

if ( !file.exists(niftyCSV) ) {
  write.csv(Quandl(nifty, collapse = period, 
                   start_date = start, type = "raw"), 
            file = niftyCSV)
}
ni <- read.csv(niftyCSV, header = TRUE)

# First, compute the weekly changes in the price (PC ) of the selected stock as well as in the nifty index values (IC).
# I did this by adding a new column to the R data frame that computed:
#     (closePriceCurrent - closePricePrevious) / closePricePrevious  
cDat <- data.frame(co$X, co$Date, co$Close)
cDat <- cbind(cDat, 
              PC = ((co$Close - lag(co$Close, 1)) / lag(co$Close, 1)) * 100)
nDat <- data.frame(ni$X, ni$Date, ni$Close)
nDat <- cbind(nDat,
              IC = ((ni$Close - lag(ni$Close, 1)) / lag(ni$Close, 1)) * 100)

# Classify each of PC and IC in three suitable ranges (calling them Low, Medium and High) 
# I classified them into 3 ranges: Low <=-1, Medium >-1 <1, High >=1
cDat$Class <- with(cDat, 
             ifelse(PC <= -1, classes[1], 
                    ifelse(PC <= 1, classes[2], classes[3])))
nDat$Class <- with(nDat, 
                   ifelse(IC <= -1, classes[1], 
                          ifelse(IC <= 1, classes[2], classes[3])))

# and then construct a two-way table
# TODO: Can do this in a single pass
m <- matrix(0, nrow = 4, ncol = 4)
sanitizedM <- matrix(0, nrow = 3, ncol = 3)
total <- 0
for (i in 1:3) {
  for (j in 1:3) {
    m[i, j] <- length(
      intersect(which(cDat$Class == classes[i]), 
                which(nDat$Class == classes[j])))
    sanitizedM[i, j] <- m[i, j]
    total <- total + m[i, j]
    m[4, j] <- m[4, j] + m[i, j]
  }
  m[i, 4] <- sum(m[i,])
}
m[4, 4] <- total

rownames(m) <-c(classes, "row-sum")
colnames(m) <- c(classes, "col-sum") 
rownames(sanitizedM) <- paste(classes, "icici", sep="-")
colnames(sanitizedM) <- paste(classes, "nifty", sep="-")

# Given that IC is high, what is the probability that PC is low?
# TODO

# Are PC and IC independent?
# No because X-squared = 37.474 and the probability of that happening
# given the null is < alpha=0.05
x <- 0
e <- matrix(0, ncol = 3, nrow = 3)
n <- 106
for(i in 1:3) {
  for (j in 1:3) {
    e[i, j] <- m[i, 4] * m[4, j] / n
    sqDiff <- (m[i, j] - e[i, j]) ^ 2
    x <- x + sqDiff / e[i, j]
  }
}

# chisq.test(sanitizedM, correct = FALSE)
print(paste("chi-squared test value ", x))

# Test if the average weekly PC is at least 0.5%
# Here we assume that all 106 weeks form a sample, and we're trying to 
# make a judgement about the mean of all weeks in the population.
# mu = 0.5
# xBar = sample mean
# sigmaBar = estimate of population mean, i.e sample sd/sqrt(n)
# N = 106, large enough that we can apply CLT and assume it's normal
# So essentially we want to standardize xBar and compare it to the 
# alpha level critical value. Given H0 is true, the probability of 
# getting the given xBar should be sufficiently high (> 0.05), i.e
# the area under the curve for the given z value should be > 0.05.
# Since in this case, the probability is 0.07, we accept the null.
xBar <- mean(cDat$PC, na.rm = TRUE)
mu <- 0.5
z = xBar - 0.5 / (sd(cDat$PC, na.rm = TRUE) / sqrt(n))
area <- pnorm(z)
if (area >= 0.05) {
  print(paste("Accepting H0; area ", area))
} else {
  print(paste("Rejecting H0; area ", area))
}

# Test if 90% of the weeks have ICs more than 0.5%
# Here we assume that all 106 weeks form a sample, and we're trying to 
# make a judgement about the proportion of all weeks in the population.
# p0 = 0.9
# pBar = sample proportion of ICs > 0.5%
# N = 106, large enough that we can apply CLT and assume it's normal
# So essentially we want to standardize pBar and compare it to the 
# alpha level critical value. Given H0 is true, the probability of 
# getting the given pBar should be sufficiently high (> 0.05), i.e
# the area under the curve for the given z value should be > 0.05.
# Since in this case, the probability is 8.58711616457683e-75, 
# we reject the null.
pBar <- length(which(nDat$IC >= 0.05)) / length(nDat$IC)
p0 <- 0.9
z <- (pBar - p0) / sqrt(p0 * (1 - p0) / N)
area <- pnorm(z)
if (area >= 0.05) {
  print(paste("Accepting H0; area", area))
} else {
  print(paste("Rejecting H0; area ", area))
}

# Test if the variation in weekly IC is at most 1%
# Here we assume that all 106 weeks form a sample, and we're trying to 
# make a judgement about the mean of all weeks in the population.
# sigma = 1
# s = sample mean
# N = 106, large enough that we can apply CLT and assume it's normal
# So essentially we want to standardize s and compare it to the 
# alpha level critical value. Given H0 is true, the probability of 
# getting the given s should be sufficiently high (> 0.05), i.e
# the area under the curve for the given chi-squared value should 
# be > 0.05 (the degrees of freedom are 105). Since in this case, 
# the probability is 0.07, we accept the null.
s <- sd(nDat$IC, na.rm = TRUE)
sigma <- 1
chi <- (N - 1) * s ^ 2 /  1
area <- pchisq(chi, N - 1, lower.tail = FALSE)
if (area >= 0.05) {
  print(paste("Accepting H0; area under chi-squared ", area))
} else {
  print(paste("Rejecting H0; area under chi-squared ", area))
}

# Are the weekly PC averages across high and low IC equal?
# So we basically want to know, given N sample means, are the
# population means equal? we have a large enough sample that we 
# can assume that the population of PCs is normally distribuetd,
# so our test statistic will hae a t distribution with N-1
# degrees of freedom. We choose N=15 becase there are only 19
# "High" samples in NIFTY. We can compare the computed t value
# with the critical value for a t distribution at the 97.5% 
# cutoff (since this is a 2 tailed test). If the computed t 
# value is to the left of the cutoff, it means the area under
# the t distribution curve > alpha.
n <- 15
d <- head(cDat$PC[which(nDat$Class == "High")], n) - 
     head(cDat$PC[which(nDat$Class == "Low")], n)
dBar <- mean(d)
sd <- sqrt(sum((d - dBar) ^ 2) / (n - 1))
t <- (dBar - 0) / (sd / sqrt(n))
k <- qt(.975, 14)
if (t <= k) {
  print(paste("Accepting H0; t value ", t))
} else {
  print(paste("Rejecting H0; t value ", t))
}

# Is the variation of weekly PC when IC is low same as that of IC 
# when it is high?
# We can determine this by examining the test statistic s1^2/s2^2. 
# Assuming these are 2 independent random samples, and both the IC
# low/high distribution follow a normal trend, this statistic follows
# an F distribution with degrees of freedom n-1, n-1. Of course this
# only applies when the variences are equal, which only happens when
# H0 is true. 
s1 <- sd(head(cDat$PC[which(nDat$Class == "High")], n), na.rm = TRUE)
s2 <- sd(head(cDat$PC[which(nDat$Class == "Low")], n), na.rm = TRUE)
f <- s1 ^ 2 / s2 ^ 2
k <- qf(.975, 14, 14)
if (f <= k) {
  print(paste("Accepting H0; f value ", f))
} else {
  print(paste("Rejecting H0; f value ", f))
}

# Test if the proportion of weeks with more than 0.5% PC is same 
# when IC is medium or low
# We can determine this by examining the test statistic for (p1-p2). 
# Assuming these are 2 independent random samples, and both the IC
# low/high distribution follow a normal trend, this statistic follows
# a z distribution.
p1 <- head(cDat$PC[which(nDat$Class == "Medium")], n)
p2 <- head(cDat$PC[which(nDat$Class == "Low")], n)
p1 <- length(which(p1 > 0.5)) / length(p1)
p2 <- length(which(p2 > 0.5)) / length(p2)
pBar <-  (p1 + p2) / 2
z <- (p1 - p2) / sqrt( pBar * (1 - pBar) * (2 / n))
k <- qnorm(.975)
if (z <= k) {
  print(paste("Accepting H0; z value ", z))
} else {
  print(paste("Rejecting H0; z value ", z))
}

# Test if the weekly PC averages across three classifications of 
# IC are equal.
# This requires an anova test which should give us an f distribution
# with k-1,n-k degrees of freedom. We can accept H0 if the area under 
# the curve for the given f value is > 0.05. In this case it is not, 
# so we conclude that the means are NOT equal. 
pcxic <- data.frame(cDat$PC, nDat$Class)
pcxic <- pcxic %>% na.omit()
k <- 3
xBarNet <- mean(pcxic$cDat.PC)

nHigh <- length(which(pcxic$nDat.Class == "High"))
xHigh <- mean(pcxic$cDat.PC[which(pcxic$nDat.Class == "High")])
sHigh <- sum(
  (pcxic$cDat.PC[which(pcxic$nDat.Class == "High")] - xHigh) ^ 2) / 
    (nHigh - 1)

nMed <- length(which(pcxic$nDat.Class == "Medium"))
xMed <- mean(pcxic$cDat.PC[which(pcxic$nDat.Class == "Medium")])
sMed <- sum(
  (pcxic$cDat.PC[which(pcxic$nDat.Class == "Medium")] - xMed) ^ 2) / 
    (nMed - 1)

nLow <- length(which(pcxic$nDat.Class == "Low"))
xLow <- mean(pcxic$cDat.PC[which(pcxic$nDat.Class == "Low")])
sLow <- sum(
  (pcxic$cDat.PC[which(pcxic$nDat.Class == "Low")] - xLow) ^ 2) / 
  (nLow - 1)

sst <- nLow * (xLow - xBarNet) ^ 2 + 
       nMed * (xMed - xBarNet) ^ 2 +
       nHigh * (xHigh - xBarNet) ^ 2
mstr <- sst / (k - 1)

sse <- (nLow - 1) * sLow  + 
       (nMed - 1) * sMed  + 
       (nHigh - 1) * sHigh  

mse <- sse / (N - k)

# Simply running: 
# anova(lm(cDat.PC~nDat.Class,data=pcxic))
# Will produce the same effect. 
f <- mstr / mse
area <- df(f, k - 1, n - k)
if ( area >= 0.05 ) {
  print(paste("Accepting H0; f value ", f))
} else {
  print(paste("Rejecting H0; f value ", f))
}

# Find a 95% confidence interval for average PC
# We can find this by simply computing the z score for 0.975,
# which is the number of standard deviations above/below from 
# the mean we would find 95% of value. As show it's -0.079
# +/- 2.097, i.e -2.177004 to 2.017325.
xBar <- mean(cDat$PC, na.rm = TRUE)
error <- qnorm(0.975) * sd(cDat$PC, na.rm = TRUE) / sqrt(n)
print(paste(xBar - error, " to ", xBar + error))

# Find a 95% confidence interval for proportion of weeks with 
# than 0.5% PC.
# We would find this in a similar way to mean, except instead of using
# standard deviation we'd use a different statistic. In this case
# it's shown to be 0.266017883159433  to  0.771717965897171.
pBar <- length(which(cDat$PC >= 0.05)) / length(cDat$PC)
error <- qnorm(0.975) * sqrt( pBar * (1 - pBar) / n )
print(paste(pBar - error, " to ", pBar + error))
