#The csv datafile including each row as a sequence of events
#Intend to comput Steady state distribution for each sequence.
#Therefore, I loop the process for each row (j = 1 through 100) 

DAT <- as.matrix(read.csv("", header = T))

for(j in 1:100)
{
  x <- DAT[j,]

  #compute transition probability matrix
  p <- matrix(nrow = 3, ncol = 3, 0) 
  for (t in 1:(length(x) - 1)) p[x[t], x[t + 1]] <- p[x[t], x[t + 1]] + 1 
  for (i in 1:3) p[i, ] <- p[i, ] / sum(p[i, ])
  p[is.nan(p)] <- 0
  
  #compute steady state distribution
  n = ncol(p)
  A = t(p - diag(n))
  A = rbind(A, rep(1, n))
  b = c(rep(0, n), 1)
  steadystate = qr.solve(A, b)
  steadystate 
  SS[j, ] <- steadystate 
  sum(steadystate[1], steadystate[2], steadystate[3])
}

##Final Product
write.csv(SS,"directory/filename.csv" )



