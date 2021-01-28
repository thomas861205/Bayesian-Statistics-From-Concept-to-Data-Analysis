golf = read.table('http://users.stat.ufl.edu/~winner/data/pgalpga2008.dat')

datF <- subset(golf, V3==1, select=1:2) # Gender=1(female)
datM <- subset(golf, V3==2, select=1:2) # Gender=2(male)
plot(datF$V1, datF$V2)

y=lm(datF$V2~datF$V1)
# names(summary(y))
df=y$df.residual
out=summary(y)$coefficients
#                Estimate  Std. Error   t value     Pr(>|t|)
# (Intercept) 130.8933146 10.92765160 11.978174 8.078890e-24
# datF$V1      -0.2564907  0.04424461 -5.797106 3.662467e-08

# 95% posterior interval for the slope
out[2, 1] - out[2, 2]*qt(.975, df)
# [1] -0.3438909
out[2, 1] + out[2, 2]*qt(.975, df)
# [1] -0.1690905

attach(datF)
predict(y, data.frame(V1= 260))

# 5.
# Which of the following gives a 95% posterior predictive interval for the 
# driving accuracy of a new female golfer whose average driving distance is 
# x=260 yards?
predict(y, data.frame(V1= 260),interval='prediction')
       # fit      lwr      upr
# 1 64.20573 53.74528 74.66619


# 6.
# What is the correct interpretation of the interval found in Question 5?
# [V] If we select `a new female` golfer who averages 260 yards per drive, our 
#     probability that her driving accuracy will be in the interval is .95.
#     ans: Predictive intervals provide probability statements about a new observation.
# [X] If we select `a new female` golfer who averages 260 yards per drive, we are 
#     95% confident that the posterior mean for her accuracy would be in the 
#     interval.
#     ans: Predictive intervals do not predict posterior means. They predict a new observation.
# [] `For all` female golfers who average 260 yards per drive, we are 95% 
#    confident that all their driving accuracies will be in the interval.
# [X] `For all female` golfers who average 260 yards per drive, our probability is 
#     .95 that the mean of their driving accuracy is in the interval.
#     ans: This describes a credible interval for b_0 + b_1*260, the mean 
#          accuracy for golfers who average 260 yards. This is not a predictive 
#          interval for a single observation because it does not take into 
#          account the variability between golfers.