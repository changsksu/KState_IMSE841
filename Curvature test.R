#curvature check
#Example 3.4 pp 111
library("rsm")
x1<-matrix(c(-1,1,-1,1,0,0,0,0,0), ncol=1, nrow=9)
x2<-matrix(c(-1,-1,1,1,0,0,0,0,0), ncol=1, nrow=9)
y<-matrix(c(39.3,40.9, 40,41.5,40.3,40.5,40.7,40.2,40.6), ncol=1, nrow=9)
mod1<-rsm(y~FO(x1,x2)+TWI(x1,x2))
summary(mod1)
# what is the difference between the LOF and Curvature test?
# Ans. LOF involves all locations of DOE about prediction but Curvature is for center locations
SScurve=5*4*(mean(y[5:9])-mean(y[1:4]))^2/(5+4)
F=SScurve/0.043 #F is the ratio of SScurature to PE
# Testing of H0: sum(Betajj) = 0
qf(.95, df1=1, df2=4) ##This gives the critical F values at 95% significant level
1 - pf(F, df1=1, df2=4)   ##This gives the p value