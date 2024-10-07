#Prep the data for n=3 example
f=function(x){2*exp(-x)}
x3<-c(-1,0,+1)
y=f(x3)

X= matrix(c(rep(1,3), x3, x3^2), ncol=3, nrow=3)
Xp=t(X)

#To generate the X??X matrix, use either one of the following cross product
Xp %*% X
crossprod(X, X) -> XpX

#To generate the inverse of the X??X
solve(XpX)

#The beta estimate is then
Xp %*% y ->C
solve(XpX) %*% C

#A faster way to estimate beta is to solve linear equations
solve(XpX, C)
