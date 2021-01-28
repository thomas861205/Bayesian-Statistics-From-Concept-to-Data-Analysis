golf = read.table('http://users.stat.ufl.edu/~winner/data/pgalpga2008.dat')
golf$V3[golf$V3==1] <- 0 # female 1 -> 0
golf$V3[golf$V3==2] <- 1 # male 2 -> 1
attach(golf)
y=lm(V2 ~ V1 + V3)