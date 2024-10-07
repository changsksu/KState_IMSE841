#Use the agricolae package to run the split-plot analysis
#
#install.packages("lattice")
#install.packages("car")
#install.packages("agricolae")
library(lattice)
library(car)
#set up the data frame
DATA<-read.csv(file.choose(),header=TRUE)
X1=DATA[,2]
X2=DATA[,3]
y=DATA[,4]
Wplot=DATA[,1]
DATA

#set up the whole plot and subplot structure in DATA
library(agricolae)
mod3=with(DATA,sp.plot(Wplot,X1,X2,y))

# this model consider x1 in the whole plot
mod4=aov(y~X1*X2+Error(Wplot/X1),data = DATA)
summary(mod4)

# analysis without the consideration of whole plot
mod5=lm(y~X1+X2+X1*X2)
summary
# residual plot
residualPlots(model=mod5, method = "model.frame")
# Cooks distance
influencePlot(mod5)



