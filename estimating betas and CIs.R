# Data: Myers Table 2.2 
# Example: Myers 2.2, 2.3, and 2.4
# Demo: visalize a data set, compute beta, and normality test via matrix computation
# By Shing Chang, IMSE, K-State 2/11/2022
# set the working directory
setwd("C:/Users/changs/Dropbox/Work Space/Class/IMSE 841/Programs/R Language")
A<-read.csv("MyersTable2.2D.csv",header=T)
# convert natual variables into coded variables
    x1<-A[,2]
    x2<-A[,3]
    x1 <- (x1-(range(x1)[2]+range(x1)[1])/2)/((range(x1)[2]-range(x1)[1])/2)
    x2 <- (x2-(range(x2)[2]+range(x2)[1])/2)/((range(x2)[2]-range(x2)[1])/2)
    y = A[,4]
### generate t values at alaph at 0.05
    #find t critical value
    n= 14
    p= 3
    qt(p=.025, df=n-p, lower.tail=TRUE)
    qt(p=.975, df=n-p, lower.tail=TRUE)    
### Estimation of Beta (first order model)
    X=matrix(c(rep(1,n), x1, x2), ncol=3, nrow=n)
    Xp=t(X)
    XpX=Xp %*% X
    XpXinv=solve(XpX)
    XpXinv
    b=XpXinv %*% Xp %*% y
    b
    #residuals   
    e=y-X%*%b
    e
### SSE & SST computation
    SSE= t(y)%*% y - t(b) %*% Xp %*% y
    SSE
    n=length(y)
    p=3
    MSE= SSE/(n-p)
    MSE
### standard error of b
    C= XpXinv 
    C
    seb1=sqrt(C[2,2]*MSE)
    seb2=sqrt(C[3,3]*MSE)
    seb1
    seb2
### t values for b1 & b2
    tb1= b[2]/seb1
    tb2= b[3]/seb2
    tb1
    tb2
### compute p values of tb1 and tb2
    1-pt(q=tb1, df=n-p, lower.tail=TRUE)
    # pt(q=tb1, df=n-p, lower.tail=FALSE)
    pt(q=tb2, df=n-p, lower.tail=TRUE)
    # since both p values are smaller than 0.05, we would reject H0: bi=0
### CI of b1 and b2
    LCIb1= b[2] + qt(p=0.025,df=n-p, lower.tail=TRUE) * seb1
    UCIb1= b[2] + qt(p=0.975,df=n-p, lower.tail=TRUE) * seb1
    LCIb1
    UCIb1
    LCIb2= b[3] + qt(p=0.025,df=n-p, lower.tail=TRUE) * seb2
    UCIb2= b[3] + qt(p=0.975,df=n-p, lower.tail=TRUE) * seb2
    LCIb2
    UCIb2
### Normality Check on Residuals
    order=rank(e)
    p=(order-0.5)/length(e)
    z=qnorm(p)
    plot(e,z)