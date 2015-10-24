# Numerical integration using monte carlo methods
set.seed(456456)
n <- 100000
mu1 <- 0
sd1 <- 1
mu2 <- 0.8 # i.e. cohen's d = 0.8
sd2 <- 1

xs <- seq(min(mu1 - 3*sd1, mu2 - 3*sd2), max(mu1 + 3*sd1, mu2 + 3*sd2), length.out=n)
f1 <- dnorm(xs, mean=mu1, sd=sd1) # dist1
f2 <- dnorm(xs, mean=mu2, sd=sd2) # dist2

ps <- matrix(c(runif(n, min(xs), max(xs)), runif(n, min=0, max=max(f1,f2)) ), ncol=2) # sample x,y from uniform dist

z1<- ps[,2] <= dnorm(ps[,1], mu1, sd1) # dist1
z2<- ps[,2] <= dnorm(ps[,1], mu2, sd2) # dist 2
z12 <- z1 | z2 # both dists
z3 <- ps[,2] <= pmin(dnorm(ps[,1], mu1, sd1), dnorm(ps[,1], mu2, sd2)) # overlap

# plot
plot(ps[!z12, 1], ps[!z12, 2], col='#137072', pch=20, ylim=c(0, max(f1,f2)), xlim=range(xs), xlab="", ylab="")
points(ps[z1,1], ps[z1,2], col="#FBFFC0")
points(ps[z2,1], ps[z2,2], col="#56B292")
points(ps[z3, 1], ps[z3,2], col="#BF223D")
lines(xs, f1, lwd=2)
lines(xs, f2, lty="dotted",lwd=2)


To calculate the overlap we just divide the number of points in the overlap region with the total numbers of points in one of the distributions. To get more stable results I calculate the mean overlap using both distributions. What we’re calculating is sometimes called the overlapping coefficient (OVL).

# proportion of overall overlap
(sum(z3)/sum(z1) + sum(z3)/sum(z2))/2


The faster but less cool way
If we just want to convert from Cohen’s d to OVL, we can use the cumulative distribution function pnorm().

# using cdf, only works when sigma_1 = sigma_2
d <- (mu1-mu2)/sd1
2 * pnorm(-abs(d)/2)


int_f <- function(x, mu1, mu2, sd1, sd2) {
  f1 <- dnorm(x, mean=mu1, sd=sd1)
  f2 <- dnorm(x, mean=mu2, sd=sd2)
  pmin(f1, f2)
}
integrate(int_f, -Inf, Inf, mu1=0, mu2=0.8, sd1=1, sd2=1)
0.6891566 with absolute error < 1.6e-05
