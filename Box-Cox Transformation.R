# Install the required package if you don't have it
# install.packages("car")
# install.packages("carData")

# Load the 'car' package
library(car)
library(MASS)

# Example: Myer Example 2.1 y: Transistor Gain, x1: drive-in time, x2: dose
setwd("C:/Users/changs/Dropbox/Work Space/Class/IMSE 841/Programs/R Language")
A<-read.csv("MyersTable2.2D.csv",header=T)
y<-A[,4]
### Two factor first order, interaction, and one 2nd order term
# convert natual variables into coded variables
x1<-A[,2]
x2<-A[,3]
x1 <- (x1-(range(x1)[2]+range(x1)[1])/2)/((range(x1)[2]-range(x1)[1])/2)
x2 <- (x2-(range(x2)[2]+range(x2)[1])/2)/((range(x2)[2]-range(x2)[1])/2)


# Apply the Cox-Box transformation
# 'family="bcPower"' specifies Box-Cox transformation and handles negative values.
bc_result <- powerTransform(y, family="bcPower")

# Check the optimal lambda
print(bc_result)

# Get the transformed data using the optimal lambda
transformed_data <- bcPower(y, bc_result$lambda)

# Print the transformed data
print(transformed_data)

# Extract the estimated lambda value
lambda_optimal <- bc_result$lambda

# Print the optimal lambda
# note that this result is obtained without fitting a regression model
print(lambda_optimal)


# Generate the Box-Cox transformation plot
out4<-lm(y~x1+ x2)
boxcox(out4, lambda = seq(-2, 2, by = 0.1))

# Optionally, you can save the result and get the best lambda

bc <- boxcox(out4, lambda = seq(-2, 2, by = 0.1))
best_lambda <- bc$x[which.max(bc$y)]
# note that this result is obtained with fitting a regression model
cat("Best Lambda:", best_lambda)

# Here’s a breakdown of the lambda values in the Cox-Box/Box-Cox context:
  
# If 𝜆=  1
# λ=1, the data is left unchanged (no transformation).
#If 𝜆=  0
# λ=0, a logarithmic transformation is applied.
# If 𝜆<   0
# λ<0, an inverse transformation (e.g., reciprocal) is applied.
# Other values of 𝜆
# λ between -5 and 5 apply different power transformations.