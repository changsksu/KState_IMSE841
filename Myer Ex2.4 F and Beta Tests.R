# Example Myers 2.4 for ANOVA and Beta Tests
#
# the complete computation is in estimating betas and CIs
#
# set the working directory
setwd("C:/Users/changs/Dropbox/Work Space/Class/IMSE 841/Programs/R Language")
A<-read.csv("MyersTable2.2D.csv",header=T)

# convert natual variables into coded variables
x1<-A[,2]
x2<-A[,3]
x1 <- (x1-(range(x1)[2]+range(x1)[1])/2)/((range(x1)[2]-range(x1)[1])/2)
x2 <- (x2-(range(x2)[2]+range(x2)[1])/2)/((range(x2)[2]-range(x2)[1])/2)
n=length(x1)


X <- matrix(c(rep(1,n), x1, x2), n, 3) 
# here X is now a 14 x 4 matrix and ready for calc

# compute the C=inv(X'X) matrix
XpX <- t(X) %*% X
C <- solve(XpX)