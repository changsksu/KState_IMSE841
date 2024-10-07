# Example Myers 2.2 using lm -- linear model
# set the working directory
setwd("C:/Users/changs/Dropbox/Work Space/Class/IMSE 841/Programs/R Language")
A<-read.csv("MyersTable2.2D.csv",header=T)

# convert natual variables into coded variables
x1<-A[,2]
x2<-A[,3]
x1 <- (x1-(range(x1)[2]+range(x1)[1])/2)/((range(x1)[2]-range(x1)[1])/2)
x2 <- (x2-(range(x2)[2]+range(x2)[1])/2)/((range(x2)[2]-range(x2)[1])/2)
n=length(x1)
X <- matrix(c(x1, x2, x1*x2), n, 3)  
# Note that this matrix does not have 1¡¦s
# Note also 12 can be replaced by length(x1)

X <- matrix(c(rep(1,n), x1, x2, x1*x2), n, 4) 
# here X is now a 14 x 4 matrix and ready for calc

# compute the C=inv(X'X) matrix
C <- t(X) %*% X

# Fiting one variable
x<-x1
y<-A[,4]
plot (x, y)
out<-lm(y~x)
abline(out)
anova(out)
coef(out)

# two input variables
out2<-lm(y~x1+x2)
anova(out2)
coef(out2)

### For 2nd order polynomial of x1 (simple regression)
out3<-lm(y~x1+I(x1^2))
y2<-predict(out3, list(x1))
lines(x1, y2, col="red")
anova(out3)
coef(out3)

### Two factor first order, interaction, and one 2nd order term
out4<-lm(y~x1+ x2+ x1*x2+ I(x2^2))
anova(out4)
coef(out4)
summary(out4)

### residual analyses
plot(out4,1) # residuals vs fitted to check homogeneity of variances
plot(out4,2) # normality probability plot
# Extract the residuals
aov_residuals <- residuals(object = out4 )
# Run Shapiro-Wilk test for normality: reject normal when p is small
shapiro.test(x = aov_residuals)

# influential diagnosis - Cook's Distance
# Calculate Cook's distance
cooks_d <- cooks.distance(out4)

# Print Cook's distance
print(cooks_d)

# Optionally, plot Cook's distance
plot(cooks_d, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
