# likelihood of a bernoulli distribution
likelihood = function(n, y, theta){return(theta ^ y * (1 - theta) ^ (n - y))}
theta = seq(from=0.01, to=0.99, by=0.01)
plot(theta, likelihood(400, 72, theta))
abline(v=.18)

# log likelihood of a bernoulli distribution
loglikelihood = function(n, y, theta){return(y * log(theta) + (n - y) * log(1 - theta))}
plot(theta, loglikelihood(400, 72, theta), type='l')
abline(v=.18)

# Function           What it does
# dnorm(x, mean, sd) Evaluate the PDF at x (mean = µ and sd = sqrt(σ^2)).
# pnorm(q, mean, sd) Evaluate the CDF at q.
# qnorm(p, mean, sd) Evaluate the quantile function at p.
# rnorm(n, mean, sd) Generate n pseudo-random samples from the normal distribution.

# Report the upper end of a 95% equal-tailed interval for θ where θ|y ~ Normal(0, 1)
qnorm(p=0.975, mean=0, sd=1)