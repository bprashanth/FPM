N = 200
n = 80
nDist39 = rnorm(n*N, 3, 9)
means39 = c()
for (i in 1:N) {
  means39 <- c(means39, mean(sample(nDist39, n)))
}

N = 200
n = 90
nDist69 = rnorm(n*N, 3, 9)
means69 = c()
for (i in 1:N) {
  means69 <- c(means69, mean(sample(nDist69, n)))
}

diffMeans <- means69 - means39
lows <- diffMeans - 1.95 * sd(diffMeans)
highs <- diffMeans + 1.95 * sd(diffMeans)
plot(lows, 1:200, col="green", lwd=5)
par(new=FALSE)
lines(highs, 1:200, col="red", lwd=2)