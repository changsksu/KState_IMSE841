#RSM package demo
#Data set: chemical  reaction data: ChemReact 1 and ChemRect 2
#Need the library called RSM
library("rsm")
#
#provide coding computation
 CR1 <- coded.data(ChemReact1, x1 ~ (Time - 85)/5, x2 ~ (Temp - 175)/5)
#
#Fit linear model; similar to lm ~ x1 + x2; observe LOF test; is it adequate?
 CR1.rsm <- rsm(Yield ~ FO(x1, x2), data = CR1)
 summary(CR1.rsm)
#
#Add the interaction term; observe LOF test; is the model adequate?
CR1.rsmi <- update(CR1.rsm, . ~ . + TWI(x1, x2))
summary(CR1.rsmi)
#
#Add more data as block 2; the first data set is considered block 1
( CR2 <- djoin(CR1, ChemReact2) )
#
#Run 2nd order model: FO + TWI + PQ or just SO; observe LOF test; is the model adequate?
CR2.rsm <- rsm(Yield ~ Block + SO(x1, x2), data = CR2)
summary(CR2.rsm)
#
#Contour plot
contour(CR2.rsm, ~x1+x2, image=TRUE, at=summary(CR2.rsm)$canonical$xs) 