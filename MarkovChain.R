dat<-data.frame(replicate(20,sample(c(1, 2, 3), size = 100, replace=TRUE)))

#coerce data frame as matrix
DAT <- as.matrix(dat)
DAT

#subset the first row from the matrix DAT
x <- DAT[1,]
x

#compute transition probability matrix
p <- matrix(nrow = 3, ncol = 3, 0)
for (t in 1:(length(x) - 1)) p[x[t], x[t + 1]] <- p[x[t], x[t + 1]] + 1
for (i in 1:3) p[i, ] <- p[i, ] / sum(p[i, ])
p

#compute steady state distribution
n = ncol(p)
A = t(p - diag(n))
A = rbind(A, rep(1, n))
b = c(rep(0, n), 1)

A
b

steadystate = qr.solve(A, b)
steadystate

