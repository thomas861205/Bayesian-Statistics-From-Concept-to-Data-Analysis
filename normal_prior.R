# inverse gamma(a, b)
a = 3; b = 200
z <- rgamma(n=1000, shape=a, rate=b)
x <- 1/z


# posterior of the mean mu
z <- rgamma(1000, shape=16.5, rate=6022.9)
sig2 <- 1/z
mu <- rnorm(1000, mean=609.3, sd=sqrt(sig2/27.1))

# We can use these simulated draws to help us approximate inferences for μ and 
# σ^2. For example, we can obtain a 95% equal-tailed credible for μ by 
# calculating the quantiles/percentiles of the simulated values.
quantile(x=mu, probs=c(0.025, 0.975))
# Return:
#     2.5%    97.5% 
# 601.9709 616.6901 

foo=function(m, w, a, b, n, ybar, s2) {
    aprime = a + n/2
    bprime = b + (n-1)/2*s2 + w*n/(2*(w+n))*(ybar-m)^2
    mprime = (n*ybar + w*m) / (n + w)
    wprime = w + n

    z <- rgamma(1000, shape=aprime, rate=bprime)
    sig2 <- 1/z # posterior for variance
    mu <- rnorm(1000, mean=mprime, sd=sqrt(sig2/wprime)) # posterior for mean
    return(mu)
}


muB = foo(500, 0.1, 3, 200, 27, 609.7, 401.8)
muA = foo(500, 0.1, 3, 200, 30, 622.8, 403.1)
sum( muA > muB ) / 1000 # Return 0.992
