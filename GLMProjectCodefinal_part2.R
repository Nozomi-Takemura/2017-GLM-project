################################################
############## Part 2 - Fish data ##############
################################################

##############################################
###### Import data and install packages ######
##############################################

rm(list=ls())

library(readxl)
library(car)
library(ResourceSelection)
library(MASS)
library(splines)
library(fmsb)
library(mgcv)
library(gamlss)
library(xtable)

fish <- read.table("C:/Users/RobbieBroughton/Documents/KULStuff/Semester2/Generalized Linear Models/Fish_data.txt",header=TRUE)
attach(fish)

###################################################
### Preliminary, looking at raw relationships #####
###################################################

summary(fish)

hist(Length) #looks Poisson
plot(Length~Age)
plot(Length~factor(Month), xlab='Month')
plot(Length~factor(Sex), xlab='Sex')
plot(Length~factor(Year), xlab='Year')
detach(fish)

###################################################
##### Recoding variables for further analysis #####
###################################################

##year
summary(fish$Year)
table(fish$Year)
#dummy
fish$Year.class1 <-  as.numeric(I(fish$Year=="1984"))
fish$Year.class2 <-  as.numeric(I(fish$Year=="2010"))
#effect-coded variable for the year of capture
fish$Year.rec <- 1*(fish$Year==1984) - 1*(fish$Year==2010)

##month
summary(fish$Month)
table(fish$Month)
# Replace month with dummy variables, year split in quarters
fish$Month.class1 <- as.numeric(I(fish$Month <= 3))
fish$Month.class2 <- as.numeric(I(3 < fish$Month & fish$Month <= 6))
fish$Month.class3 <- as.numeric(I(6 < fish$Month & fish$Month <= 9))
fish$Month.class4 <- as.numeric(I(9 < fish$Month))

##Age
summary(fish$Age)
table(fish$Age)
#dummy_coding - 
fish$Age.class1 <- as.numeric(I(fish$Age <= 2))
fish$Age.class2 <- as.numeric(I(2 < fish$Age & fish$Age <= 6))
fish$Age.class3 <- as.numeric(I(6 < fish$Age))
#dummy_coding_b - 
fish$Age.class1b <- as.numeric(I(fish$Age <= 2))
fish$Age.class2b <- as.numeric(I(2 < fish$Age & fish$Age <= 4))
fish$Age.class3b <- as.numeric(I(4 < fish$Age & fish$Age <= 8))
fish$Age.class4b <- as.numeric(I(8 < fish$Age & fish$Age <= 12))
fish$Age.class5b <- as.numeric(I(12 < fish$Age & fish$Age <= 18))
#dummy_coding_c
fish$Age.class1c <- as.numeric(I(fish$Age <= 2))
fish$Age.class2c <- as.numeric(I(2 < fish$Age & fish$Age <= 4))
fish$Age.class3c <- as.numeric(I(4 < fish$Age & fish$Age <= 12))
fish$Age.class4c <- as.numeric(I(12 < fish$Age))

##sex
summary(fish$Sex)
table(fish$Sex)
#dummy
fish$Sex.class1 <-  as.numeric(I(fish$Sex=="Female"))
fish$Sex.class2 <-  as.numeric(I(fish$Sex=="Male"))
##dummy
fish$Sex.rec3[fish$Sex=="Female"]<-"0" 
fish$Sex.rec3[fish$Sex=="Male"]<-"1" 
fish$Sex.rec3 <- as.numeric(fish$Sex.rec3)

## effect-coded variable for the sex
fish$Sex.rec <- 1*(fish$Sex=="Female") - 1*(fish$Sex=="Male")

##response variable - length
summary(fish$Length)
table(fish$Length)
# categorical var with four classes
fish$length4 <- 1 + 1*(fish$Length>=33) + 1*(fish$Length>=48)+1*(fish$Length>=63)
fish$length4 <- factor(fish$length4, ordered=TRUE)
table(fish$length4)
fish$length4

##################################################
########### Descriptive analysis #################
##################################################

#Build a Cumulative logit model 
fish.ordinal <- polr(length4 ~ Month.class2 + Month.class3 + Month.class4 + 
                       Age.class2 + Age.class3 + Year.class2 + Sex.class2, data=fish)
summary(fish.ordinal)
confint(fish.ordinal)

##Constructing plots
alpha.month <- fish.ordinal$coefficients[1:3]
alpha.age <- fish.ordinal$coefficients[4:5]
alpha.year <- fish.ordinal$coefficients[6]
alpha.sex <- fish.ordinal$coefficients[7]

fish$effect.month <- alpha.month[1]*fish$Month.class2 + alpha.month[2]*fish$Month.class3 + alpha.month[3]*fish$Month.class4
fish$effect.age <- alpha.age[1]*fish$Age.class2 + alpha.age[2]*fish$Age.class3
fish$effect.year <- alpha.year[1]*fish$Year.class2
fish$effect.sex <- alpha.sex[1]*fish$Sex.class2

fish$ordermonth <- order(fish$Month)
fish$orderage <- order(fish$Age)
fish$orderyear <- order(fish$Year)
fish$ordersex <- order(fish$Sex)

par(mfrow=c(1,1))
par(pty="s")
par(mar = c(5,6,4,2)+0.1)

##plotting

plot(fish$Month[fish$ordermonth],fish$effect.month[fish$ordermonth],
     xlab="Month of capture",ylab="Effect on length of fish",
     type="l",col="steel blue",lwd=3,main="",
     cex.lab=1.5,cex.axis=1.3)

plot(fish$Age[fish$orderage], fish$effect.age[fish$orderage],
     xlab="Age of fish",ylab="Effect on length of fish",
     type="l",lwd=3,col="steel blue",main="",
     cex.lab=1.5,cex.axis=1.3)


#try coding b for age
fish.ordinalb <- polr(length4 ~ Month.class2 + Month.class3 + Month.class4 + 
                        Age.class2b + Age.class3b + Age.class4b + Age.class5b + 
                        Year.class2 + Sex.class2, data=fish)
summary(fish.ordinalb)
confint(fish.ordinalb)

alpha.monthb <- fish.ordinalb$coefficients[1:3]
alpha.ageb <- fish.ordinalb$coefficients[4:7]
alpha.yearb <- fish.ordinalb$coefficients[8]
alpha.sexb <- fish.ordinalb$coefficients[9]

fish$effect.monthb <- alpha.monthb[1]*fish$Month.class2 + alpha.monthb[2]*fish$Month.class3 + alpha.monthb[3]*fish$Month.class4
fish$effect.ageb <- alpha.ageb[1]*fish$Age.class2b + alpha.ageb[2]*fish$Age.class3b + alpha.ageb[3]*fish$Age.class4b + alpha.ageb[4]*fish$Age.class5b
fish$effect.yearb <- alpha.yearb[1]*fish$Year.class2
fish$effect.sexb <- alpha.sexb[1]*fish$Sex.class2

plot(fish$Month[fish$ordermonth],fish$effect.monthb[fish$ordermonth],
     xlab="Month of capture",ylab="Effect on length of fish",
     type="l",col="steel blue",lwd=3,main="",
     cex.lab=1.5,cex.axis=1.3)

plot(fish$Age[fish$orderage], fish$effect.ageb[fish$orderage],
     xlab="Age of fish",ylab="Effect on length of fish",
     type="l",lwd=3,col="steel blue",main="",
     cex.lab=1.5,cex.axis=1.3)
#Problems with coading, two non-NA values when coding is used, cant get Hessian

#try coding c for age
fish.ordinalc <- polr(length4 ~ Month.class2 + Month.class3 + Month.class4 + 
                        Age.class2c + Age.class3c + Age.class4c + Year.class2 
                      + Sex.class2, data=fish)

summary(fish.ordinalc)
confint(fish.ordinalc)

alpha.monthc <- fish.ordinalc$coefficients[1:3]
alpha.agec <- fish.ordinalc$coefficients[4:6]
alpha.yearc <- fish.ordinalc$coefficients[7]
alpha.sexc <- fish.ordinalc$coefficients[8]

fish$effect.monthc <- alpha.monthc[1]*fish$Month.class2 + alpha.monthc[2]*fish$Month.class3 + alpha.monthc[3]*fish$Month.class4
fish$effect.agec <- alpha.agec[1]*fish$Age.class2c + alpha.agec[2]*fish$Age.class3c + alpha.agec[3]*fish$Age.class4c
fish$effect.yearc <- alpha.yearc[1]*fish$Year.class2
fish$effect.sexc <- alpha.sexc[1]*fish$Sex.class2

plot(fish$Month[fish$ordermonth],fish$effect.monthc[fish$ordermonth],
     xlab="Month of capture",ylab="Effect on length of fish",
     type="l",col="dark blue",lwd=3,main="",
     cex.lab=1.5,cex.axis=1.3)

plot(fish$Age[fish$orderage], fish$effect.agec[fish$orderage],
     xlab="Age of fish",ylab="Effect on length of fish",
     type="l",lwd=3,col="dark blue",main="",
     cex.lab=1.5,cex.axis=1.3)

################################
##### Explore interaction ######
################################

##Age and month

#Make categorical variables
#Month
fish$Month.class <- as.numeric(I(fish$Month <= 3))+
  as.numeric(I(fish$Month <= 6))+
  as.numeric(I(fish$Month <= 9))+
  as.numeric(I(9 < fish$Month))


#Age
fish$Age.class <- as.numeric(I(fish$Age <= 2))+
  as.numeric(I(fish$Age <= 4))+
  as.numeric(I(fish$Age <= 12))+
  as.numeric(I(fish$Age <= 18))

#factor interaction between the nonlinear var

par(mfrow=c(1,1))
par(pty="s")
par(mar = c(5,6,4,2)+0.1)

fish.ordinal1 <- polr(length4 ~ I(factor(Month.class):factor(Age.class)) +   
                        Year.class2 + Sex.class2, data=fish)
summary(fish.ordinal1)
coef<- c(0,fish.ordinal1$coefficients[1:11])
coef

mincoef <- -20
maxcoef <- 5
plot(1:4,coef[1:4],ylim=c(mincoef,maxcoef),
     xlab="Age-class",ylab="Coefficient",
     type="l",col="dark red",lwd=2,main="Factor interaction",
     cex.lab=1.5,cex.axis=1.3)
lines(1:4,coef[5:8],col="dark blue",lwd=2)
lines(1:4,coef[9:12],col="darkgreen",lwd=2)
text(1.0,-10,"Month-class 2",col="dark red",cex=1.2,adj=0)
text(1.0,-13,"Month-class 3",col="dark blue",cex=1.2,adj=0)
text(1.0,-16,"Month-class 4",col="dark green",cex=1.2,adj=0)

#continous interaction 
fish.ordinal2 <- polr(length4 ~ Month.class + Age.class +I(Month.class*Age.class) +   
                        Year.class2 + Sex.class2, data=fish)
summary(fish.ordinal2)
coef2<- fish.ordinal2$coefficients[1:3]

coef.1 <- coef2[1]+(1:4)*coef2[2]+(1:4)*coef2[3]
coef.2 <- 2*coef2[1]+(1:4)*coef2[2]+2*(1:4)*coef2[3]
coef.3 <- 3*coef2[1]+(1:4)*coef2[2]+3*(1:4)*coef2[3]

mincoef <- -25
maxcoef <- 0
plot(1:4,coef.1-0.3,ylim=c(mincoef,maxcoef),
     xlab="Age-class",ylab="Coefficient",
     type="l",col="dark red",lwd=2,main="Continuous interaction",
     cex.lab=1.5,cex.axis=1.3)

lines(1:4,coef.2-0.3,col="dark blue",lwd=2)
lines(1:4,coef.3-0.3,col="dark green",lwd=2)
text(1.0,-17,"Month-class 2",col="dark red",cex=1.2,adj=0)
text(1.0,-19,"Month-class 3",col="dark blue",cex=1.2,adj=0)
text(1.0,-21,"Month-class 4",col="dark green",cex=1.2,adj=0)

#some indication of a categorical interaction - 
#no indication for the continous case

####year and month###

fish$Year.rec[fish$Year=="1984"]<-"1"
fish$Year.rec[fish$Year=="2010"]<-"2"

fish.ordinal1b <- polr(length4 ~ I(factor(Month.class):factor(Year.rec)) +   
                         Age.class + Sex.class2, data=fish)
summary(fish.ordinal1b)
coef<- c(0,fish.ordinal1b$coefficients[1:5])
coef


mincoef <- -5
maxcoef <- 5
plot(1:2,coef[1:2],ylim=c(mincoef,maxcoef),
     xlab="Year-class",ylab="Coefficient",
     type="l",col="red",lwd=2,main="Factor interaction",
     cex.lab=1.5,cex.axis=1.3)
lines(1:2,coef[3:4],col="blue",lwd=2)
lines(1:2,coef[5:6],col="darkgreen",lwd=2)
text(1.0,-1,"Month-class 2",col="red",cex=1,adj=0)
text(1.0,-2.5,"Month-class 3",col="blue",cex=1,adj=0)
text(1.0,-4,"Month-class 4",col="darkgreen",cex=1,adj=0)

#nothing special here
#continous interaction 

fish$Year.rec <- as.numeric(fish$Year.rec)
fish.ordinal2b <- polr(length4 ~ Month.class+Year.rec+I(Month.class*Year.rec) +   
                         Age.class + Sex.class2, data=fish)

summary(fish.ordinal2b)
coef2<- fish.ordinal2b$coefficients[1:3]

coef.1 <- coef2[1]+(1:2)*coef2[2]+(1:2)*coef2[3]
coef.2 <- 2*coef2[1]+(1:2)*coef2[2]+2*(1:2)*coef2[3]
coef.3 <- 3*coef2[1]+(1:2)*coef2[2]+3*(1:2)*coef2[3]

mincoef <- -5
maxcoef <- 5
plot(1:2,coef.1+0.85,ylim=c(mincoef,maxcoef),
     xlab="Age-class",ylab="Coefficient",
     type="l",col="red",lwd=2,main="Continuous interaction",
     cex.lab=1.5,cex.axis=1.3)

lines(1:2,coef.2+0.85,col="blue",lwd=2)
lines(1:2,coef.3+0.85,col="darkgreen",lwd=2)

####Sex and month
#factor integration
fish.ordinal1c <- polr(length4 ~ I(factor(Month.class):factor(Sex.rec3)) +   
                         Age.class + Year.class2, data=fish)
summary(fish.ordinal1c)
coef<- c(0,fish.ordinal1c$coefficients[1:5])
coef

mincoef <- -3
maxcoef <- 3
plot(0:1,coef[1:2],ylim=c(mincoef,maxcoef),
     xlab="Sex-class",ylab="Coefficient",
     type="l",col="red",lwd=2,main="Factor interaction",
     cex.lab=1.5,cex.axis=1.3)
lines(0:1,coef[3:4],col="blue",lwd=2)
lines(0:1,coef[5:6],col="darkgreen",lwd=2)
text(0,-1,"Month-class 2",col="red",cex=1,adj=0)
text(0,-1.75,"Month-class 3",col="blue",cex=1,adj=0)
text(0,-2.5,"Month-class 4",col="darkgreen",cex=1,adj=0)

#continous integration
fish.ordinal2c <- polr(length4 ~ Month.class+Sex.rec3+I(Month.class*Sex.rec3) +   
                         Age.class + Year.class2, data=fish)

summary(fish.ordinal2c)
coef2<- fish.ordinal2c$coefficients[1:3]

coef.1 <- coef2[1]+(1:2)*coef2[2]+(1:2)*coef2[3]
coef.2 <- 2*coef2[1]+(1:2)*coef2[2]+2*(1:2)*coef2[3]
coef.3 <- 3*coef2[1]+(1:2)*coef2[2]+3*(1:2)*coef2[3]

mincoef <- -5
maxcoef <- 5
plot(1:2,coef.1+0.4,ylim=c(mincoef,maxcoef),
     xlab="Age-class",ylab="Coefficient",
     type="l",col="red",lwd=2,main="Continuous interaction",
     cex.lab=1.5,cex.axis=1.3)

lines(1:2,coef.2+0.4,col="blue",lwd=2)
lines(1:2,coef.3+0.4,col="darkgreen",lwd=2)

####Age and sex
#factor integration
#have to change coding

fish$Month.class_b <- as.numeric(I(fish$Month <= 3))+
  as.numeric(I(fish$Month <= 6))+
  as.numeric(I(fish$Month <= 9))+
  as.numeric(I(fish$Month <=12))

fish$Age.class_b <- as.numeric(I(fish$Age <= 2))+
  as.numeric(I(fish$Age <= 6))+
  as.numeric(I(fish$Age <= 10))+
  as.numeric(I(10 < fish$Age))

fish.ordinal1d <- polr(length4 ~ I(factor(Age.class_b):factor(Sex.rec3)) +   
                         Month.class_b + Year.class2, data=fish)
summary(fish.ordinal1d)
coef<- c(0,fish.ordinal1d$coefficients[1:5])

mincoef <- -20
maxcoef <- 0
plot(0:1,coef[1:2],ylim=c(mincoef,maxcoef),
     xlab="Sex-class",ylab="Coefficient",
     type="l",col="red",lwd=2,main="Factor interaction",
     cex.lab=1.5,cex.axis=1.3)
lines(0:1,coef[3:4],col="blue",lwd=2)
lines(0:1,coef[5:6],col="darkgreen",lwd=2)
text(0,-15,"Age-class 2",col="red",cex=1,adj=0)
text(0,-17,"Age-class 3",col="blue",cex=1,adj=0)
text(0,-19,"Age-class 4",col="darkgreen",cex=1,adj=0)

#continous integration
fish.ordinal2d <- polr(length4 ~ Age.class_b+Sex.rec3+I(Age.class_b*Sex.rec3) +   
                         Month.class_b + Year.class2, data=fish)

summary(fish.ordinal2d)
coef2<- fish.ordinal2d$coefficients[1:3]

coef.1 <- coef2[1]+(1:2)*coef2[2]+(1:2)*coef2[3]
coef.2 <- 2*coef2[1]+(1:2)*coef2[2]+2*(1:2)*coef2[3]
coef.3 <- 3*coef2[1]+(1:2)*coef2[2]+3*(1:2)*coef2[3]

mincoef <- -30
maxcoef <- -10
plot(0:1,coef.1+-5.9,ylim=c(mincoef,maxcoef),
     xlab="Age-class",ylab="Coefficient",
     type="l",col="red",lwd=2,main="Continuous interaction",
     cex.lab=1.5,cex.axis=1.3)
lines(0:1,coef.2+-5.9,col="blue",lwd=2)
lines(0:1,coef.3+-5.9,col="darkgreen",lwd=2)

####Age and year
#factor integration

fish.ordinal1e <- polr(length4 ~ I(factor(Age.class_b):factor(Year.rec)) +   
                         Sex.rec3 + Month.class_b, data=fish)
summary(fish.ordinal1e)
coef<- c(0,fish.ordinal1e$coefficients[1:5])
coef

mincoef <- -25
maxcoef <- 0
plot(0:1,coef[1:2],ylim=c(mincoef,maxcoef),
     xlab="Year-class",ylab="Coefficient",
     type="l",col="red",lwd=2,main="Factor interaction",
     cex.lab=1.5,cex.axis=1.3)
lines(0:1,coef[3:4],col="blue",lwd=2)
lines(0:1,coef[5:6],col="darkgreen",lwd=2)
text(0,-15,"Age-class 2",col="red",cex=1,adj=0)
text(0,-17,"Age-class 3",col="blue",cex=1,adj=0)
text(0,-19,"Age-class 4",col="darkgreen",cex=1,adj=0)

#continous integration
fish.ordinal2e <- polr(length4 ~ Age.class_b+Year.rec+I(Age.class_b*Year.rec) +   
                         Month.class_b + Sex.class2, data=fish)
summary(fish.ordinal2d)
coef2<- fish.ordinal2d$coefficients[1:3]

coef.1 <- coef2[1]+(1:2)*coef2[2]+(1:2)*coef2[3]
coef.2 <- 2*coef2[1]+(1:2)*coef2[2]+2*(1:2)*coef2[3]
coef.3 <- 3*coef2[1]+(1:2)*coef2[2]+3*(1:2)*coef2[3]

mincoef <- -25
maxcoef <- 0 
plot(0:1,coef.1+-5.9,ylim=c(mincoef,maxcoef),
     xlab="Age-class",ylab="Coefficient",
     type="l",col="red",lwd=2,main="Continuous interaction",
     cex.lab=1.5,cex.axis=1.3)
lines(0:1,coef.2+-5.9,col="blue",lwd=2)
lines(0:1,coef.3+-5.9,col="darkgreen",lwd=2)

### kept the above code in to show that we looked for further interactions but found nothing interesting

################################################
########### Model building, Poly ###############
################################################
attach(fish)
#have to order for plots
fish <- fish[order(Age),] 

#####polynomial regression model#####
par(mfrow=c(2,3))

### degree 2 ####
orthpoly <- poly(fish$Age, order=2)
fish$xo1 <- orthpoly[,1]
fish$xo2 <- orthpoly[,2]

polymodel1 <- lm(Length ~ xo1 + xo2, data=fish)
fish$fitted1 <- fitted(polymodel1)
AIC1<-AIC(polymodel1)#4417.878

plot(fish$Age,fish$Length,xlab="Age",ylab="Length",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,
     main="Degree 2")
lines(fish$Age,fish$fitted1,col="dark blue",lwd=3 )

# Find orthogonal polynomials of degree 3

orthpoly <- poly(fish$Age, order=3)
fish$xo1 <- orthpoly[,1]
fish$xo2 <- orthpoly[,2]
fish$xo3 <- orthpoly[,3]


polymodel2 <- lm(Length ~ xo1 + xo2 + xo3, data=fish)
fish$fitted2 <- fitted(polymodel2)
AIC2<-AIC(polymodel2)

plot(fish$Age,fish$Length,xlab="Age",ylab="Length",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,
     main="Degree 3")
lines(fish$Age,fish$fitted2,col="dark blue",lwd=3 )

# Find orthogonal polynomials of degree 5

orthpoly <- poly(fish$Age, order=5)
fish$xo1 <- orthpoly[,1]
fish$xo2 <- orthpoly[,2]
fish$xo3 <- orthpoly[,3]
fish$xo4 <- orthpoly[,4]
fish$xo5 <- orthpoly[,5]


polymodel3 <- lm(Length ~ xo1 + xo2 + xo3 + xo4 + xo5, data=fish)
fish$fitted3 <- fitted(polymodel3)
AIC3<-AIC(polymodel3)

plot(fish$Age,fish$Length,xlab="Age",ylab="Length",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,
     main="Degree 5")
lines(fish$Age,fish$fitted3,col="dark blue",lwd=3 )

# Find orthogonal polynomials of degree 9

orthpoly <- poly(fish$Age, order=9)
fish$xo1 <- orthpoly[,1]
fish$xo2 <- orthpoly[,2]
fish$xo3 <- orthpoly[,3]
fish$xo4 <- orthpoly[,4]
fish$xo5 <- orthpoly[,5]
fish$xo6 <- orthpoly[,6]
fish$xo7 <- orthpoly[,7]
fish$xo8 <- orthpoly[,8]
fish$xo9 <- orthpoly[,9]


polymodel4 <- lm(Length ~ xo1 + xo2 + xo3 + xo4 + xo5 + xo6 + xo7 + xo8 + xo9, data=fish)
fish$fitted4 <- fitted(polymodel4)
AIC4<-AIC(polymodel4)

plot(fish$Age,fish$Length,xlab="Age",ylab="Length",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,
     main="Degree 9")
lines(fish$Age,fish$fitted4,col="dark blue",lwd=3 )

# Find orthogonal polynomials of degree 12

orthpoly <- poly(fish$Age, order=12)
fish$xo1 <- orthpoly[,1]
fish$xo2 <- orthpoly[,2]
fish$xo3 <- orthpoly[,3]
fish$xo4 <- orthpoly[,4]
fish$xo5 <- orthpoly[,5]
fish$xo6 <- orthpoly[,6]
fish$xo7 <- orthpoly[,7]
fish$xo8 <- orthpoly[,8]
fish$xo9 <- orthpoly[,9]
fish$xo10 <- orthpoly[,10]
fish$xo11<- orthpoly[,11]
fish$xo12 <- orthpoly[,12]

polymodel5 <- lm(Length ~ xo1 + xo2 + xo3 + xo4 + xo5 + xo6 + xo7 + xo8 + xo9 + xo10 + xo11 + xo12 , data=fish)
fish$fitted5 <- fitted(polymodel5)
AIC5<-AIC(polymodel5)

plot(fish$Age,fish$Length,xlab="Age",ylab="f(x)",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,
     main="Degree 12")
lines(fish$Age,fish$fitted5,col="dark blue",lwd=3 )

# Find orthogonal polynomials of degree 15

orthpoly <- poly(fish$Age, order=15)
fish$xo1 <- orthpoly[,1]
fish$xo2 <- orthpoly[,2]
fish$xo3 <- orthpoly[,3]
fish$xo4 <- orthpoly[,4]
fish$xo5 <- orthpoly[,5]
fish$xo6 <- orthpoly[,6]
fish$xo7 <- orthpoly[,7]
fish$xo8 <- orthpoly[,8]
fish$xo9 <- orthpoly[,9]
fish$xo10 <- orthpoly[,10]
fish$xo11<- orthpoly[,11]
fish$xo12 <- orthpoly[,12]
fish$xo13 <- orthpoly[,13]
fish$xo14 <- orthpoly[,14]
fish$xo15 <- orthpoly[,15]

polymodel6 <- lm(Length ~ xo1 + xo2 + xo3 + xo4 + xo5 + xo6 + xo7 + xo8 + xo9 
                 + xo10 + xo11 + xo12 + xo13 + xo14 + xo15, data=fish)
fish$fitted6 <- fitted(polymodel6)
AIC6<-AIC(polymodel6)

plot(fish$Age,fish$Length,xlab="Age",ylab="f(x)",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,
     main="Dgree 15")
lines(fish$Age,fish$fitted6,col="dark blue",lwd=3 )

detach(fish)

#compare AIC for each
AIC. <- c(AIC1, AIC2, AIC3,AIC4,AIC5,AIC6)
names(AIC.) <- c("model1", "model2", "model3","model4","model5","model6")
sort(AIC.)
#model 6 is best 

###########################################
###### Truncated polynomial splines #######
###########################################

par(mfrow=c(1,3))

attach(fish)

##knots=2, equal distance
age_eval <- seq(1,18,length=893)
age_eval
knots <- c(1,18)

#basis functions
b1 <- Age
b2 <- Age^2

#grid to evaluate
b_eval <- matrix(0,893,3)
b_eval[,1] <- rep(1,893)
b_eval[,2] <- age_eval
b_eval[,3] <- age_eval^2
b_eval

#fit model and get rounded fitted values and calculate AIC
lm1 <- lm(fish$Length~b1+b2)
AIC_TP1<-AIC(lm1)
fitted1 <- round(b_eval%*%coef(lm1),5)

#plot
plot(Age,Length,xlab="Age",ylab="Length",ylim=c(15,80),
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,
     main="2 knots")
lines(age_eval,fitted1,lwd=3,col="dark blue")

##knots 3 - equal distance
knots <- c(0,9,18)

#same basis functions
b1 <- Age
b2 <- Age^2
b3 <- (Age-knots[2])^2*(Age > knots[2])

b_eval <- matrix(0,893,4)
b_eval[,1] <- rep(1,893)
b_eval[,2] <- age_eval
b_eval[,3] <- age_eval^2
b_eval[,4] <- (age_eval-knots[2])^2*(age_eval > knots[2])


#fit model and calculate AIC and get rounded fitted values
lm2 <- lm(fish$Length~b1+b2+b3)
AIC_TP2<-AIC(lm2)
fitted2 <- round(b_eval%*%coef(lm2),5)
fitted2

#plot
plot(Age,Length,xlab="Age",ylab="Length",ylim=c(15,80),
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,
     main="3 knots")
lines(age_eval,fitted2,lwd=3,col="dark blue")

##knots 5 approx equal distance, but not much data after 12, therfore chosen manually
knots <- c(1,4,9,12,18)

#basis functions
b1 <- Age
b2 <- Age^2
b3 <- (Age-knots[2])^2*(Age > knots[2])
b4 <- (Age-knots[3])^2*(Age > knots[3])
b5 <- (Age-knots[4])^2*(Age > knots[4])

b_eval <- matrix(0,893,6)
b_eval[,1] <- rep(1,893)
b_eval[,2] <- age_eval
b_eval[,3] <- age_eval^2
b_eval[,4] <- (age_eval-knots[2])^2*(age_eval > knots[2])
b_eval[,5] <- (age_eval-knots[3])^2*(age_eval > knots[3])
b_eval[,6] <- (age_eval-knots[4])^2*(age_eval > knots[4])

#fit model and calculate AIC and get rounded fitted values
lm3 <- lm(fish$Length~b1+b2+b3+b4+b5)
fitted3 <- round(b_eval%*%coef(lm3),5)
AIC_TP3<-AIC(lm3)

#plot
plot(Age,Length,xlab="Age",ylab="Length",ylim=c(15,80),
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,
     main="5 knots")
lines(age_eval,fitted3,lwd=3,col="dark blue")

#compare AIC of all TP splines
AIC. <- c(AIC_TP1, AIC_TP2, AIC_TP3)
names(AIC.) <- c("model1", "model2", "model3")
sort(AIC.)
#model 6 is best 
detach(fish)

###########################################
######### B-splines of degree 2 ###########
###########################################

attach(fish)
par(mfrow=c(1,3))

#fit bsplines of degree 2 and dif number of knots - use equidistant knots otherwise it gets too complicated

## m = 3 knots ##
nrknots <- 3
minx <- min(Age)-0.001
miny<-min(Length)
maxx <- max(Age)+0.001
maxy<-max(Length)
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
inner.knots
knots <- seq(minx-2*step,maxx+2*step,by=step)
knots

#7 knots total, makes sense since knots m+2l
xseq <- seq(minx,maxx,length=500)
xseq

#evaluate design matrix for the bsplines
B <- spline.des(knots=knots,Age,ord=3)$design
Bfit <- spline.des(knots=knots,xseq,ord=3)$design 

#compute AIC
S <- B%*%solve(t(B)%*%B)%*%t(B)
fit <- as.vector(S%*%fish$Length) #same thing as betahat below
n<-dim(fish)[1]
diags <- diag(S)
trs <- mean(diags)
df <- sum(diags)
AIC_B1 <- n*log(sum((Length-fit)^2)/n) + 2*(df+1)

#plot
betahat <- solve(t(B)%*%B)%*%t(B)%*%fish$Length
fitted3 <- Bfit%*%betahat
plot(Age, Length,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,main="3 knots")
lines(xseq,fitted3,lwd=2,col="dark blue")

## m = 5 knots ##
nrknots <- 5
minx <- min(Age)-0.001
maxx <- max(Age)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
inner.knots
knots <- seq(minx-2*step,maxx+2*step,by=step)
knots

#9 knots total, makes sense since knots m+2l
xseq <- seq(minx,maxx,length=500)
xseq

#evaluate design matrix for the bsplines
B <- spline.des(knots=knots,Age,ord=3)$design
Bfit <- spline.des(knots=knots,xseq,ord=3)$design

#compute AIC [ord=3, because order=l(degree+1)]
S <- B%*%solve(t(B)%*%B)%*%t(B)
fit <- as.vector(S%*%fish$Length) #same thing as betahat below
n<-dim(fish)[1]
diags <- diag(S)
trs <- mean(diags)
df <- sum(diags)#6
AIC_B2 <- n*log(sum((Length-fit)^2)/n) + 2*(df+1)

#plot
betahat <- solve(t(B)%*%B)%*%t(B)%*%fish$Length
fitted5 <- Bfit%*%betahat
plot(Age, Length,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3, main="5 knots")
lines(xseq,fitted5,lwd=2,col="dark blue")

## m = 8 knots ##
nrknots <- 8
minx <- min(Age)-0.001
maxx <- max(Age)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
inner.knots
knots <- seq(minx-2*step,maxx+2*step,by=step)
knots

#12 knots, makes sense since knots m+2l
xseq <- seq(minx,maxx,length=500)

#evaluate design matrix for the bsplines
B <- spline.des(knots=knots,fish$Age,ord=3)$design
#get an error message saying that xseq is outside of interval of 
#"inner"knots - looked at xseq
xseq
#is within the interval of the inner knots, so this error is strange
#do rounding to fix and also, outer.ok=TRUE
xseqb <- round(xseq, 2)
xseqb
Bfitb <- spline.des(knots=knots,xseqb,ord=3)$design
#also do this to see
Bfit <- spline.des(knots=knots,xseq,ord=3, outer.ok = TRUE)$design

# get AIC [ord=3, because order=l(degree+1)]
S <- B%*%solve(t(B)%*%B)%*%t(B)
fit <- as.vector(S%*%fish$Length) #same thing as betahat below
n<-dim(fish)[1]
diags <- diag(S)
trs <- mean(diags)
df <- sum(diags)
AIC_B3 <- n*log(sum((Length-fit)^2)/n) + 2*(df+1)

#plot
betahat <- solve(t(B)%*%B)%*%t(B)%*%fish$Length
fitted8 <- Bfit%*%betahat
fitted8b <-  Bfitb%*%betahat
#plot for both look the same
plot(Age, Length,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3, main="8 knots")
lines(xseq,fitted8,lwd=2,col="dark blue")
plot(Age, Length,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3, main="8 knots")
lines(xseq,fitted8b,lwd=2,col="dark blue")


### Plot all B-splines together ###
par(mfrow=c(1,3))
plot(Age, Length,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3,main="3 knots")
lines(xseq,fitted3,lwd=2,col="dark blue")
plot(Age, Length,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3, main="5 knots")
lines(xseq,fitted5,lwd=2,col="dark blue")
plot(Age, Length,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3, main="5 knots")
lines(xseq,fitted8,lwd=2,col="dark blue")

#compare all AICs of Bsplines
AIC. <- c(AIC_B1, AIC_B2, AIC_B3)
names(AIC.) <- c("model1", "model2", "model3")
sort(AIC.)
#model 3 is best in terms of AIC

detach(fish)

##########################################
######## P-splines of degree 3 ###########
##########################################

attach(fish)

## 5 knots ##
nrknots <- 5
minx <- min(Age)-0.001
maxx <- max(Age)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)

#make the difference matric, here r=2
D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots)
{
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}

#first order penalty matrix
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Age,ord=4)$design

#create grid of lambda
lambda <- seq(0.00001,10,length=500)
gcv <- rep(0,length(lambda))
aic <- rep(0,length(lambda))

#crossvalidation to find optimal lamba
for(i in 1:length(lambda))
{
  S <- B%*%solve(t(B)%*%B + lambda[i]*K2)%*%t(B)
  diags <- diag(S)
  trs <- mean(diags)
  df <- sum(diags)
  fit <- as.vector(S%*%fish$Length)
  gcv[i] <- mean(((fish$Length-fit)/(1-trs))^2)
  #  aic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + 2*df
  #  bic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + log(n)*df
  sigma2 <- sum((fish$Length-fit)^2)/n
  aic[i] <- n*log(sigma2) + 2*(df+1)
}

par(mfrow=c(1,1))

#make plots to see optimal lambda in terms of AIc and GCv
plot(lambda,gcv,type="l",lwd=2,
     xlab="lambda",ylab="GCV",main="5 knots",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
plot(lambda,aic,type="l",lwd=2,main="5 knots",
     xlab="lambda",ylab="AIC",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)

lambdamingcv <- lambda[which(gcv==min(gcv))]
lambdaminaic <- lambda[which(aic==min(aic))]

lambdamingcv;lambdaminaic

# fit model with lambda=0.00001 and get AIC
S <- B%*%solve(t(B)%*%B + 0.00001*K2)%*%t(B)
diags <- diag(S)
trs <- mean(diags)
df <- sum(diags)
fit <- as.vector(S%*%fish$Length)
sigma2 <- sum((fish$Length-fit)^2)/(n)
AIC_P1 <- n*log(sigma2) + 2*(df+1)

## 8 knots ##
nrknots <- 8
minx <- min(Age)-0.001
maxx <- max(Age)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)

#make the difference matric, here r=2
D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots)
{
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}

#first order penalty matrix
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Age,ord=4)$design

#creating a grid of lambdas
lambda <- seq(0.00001,10,length=500)
gcv <- rep(0,length(lambda))
aic <- rep(0,length(lambda))

#crossvalidation to get optimal lambda
for(i in 1:length(lambda))
{
  S <- B%*%solve(t(B)%*%B + lambda[i]*K2)%*%t(B)
  diags <- diag(S)
  trs <- mean(diags)
  df <- sum(diags)
  fit <- as.vector(S%*%fish$Length)
  gcv[i] <- mean(((fish$Length-fit)/(1-trs))^2)
  #  aic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + 2*df
  #  bic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + log(n)*df
  sigma2 <- sum((fish$Length-fit)^2)/n
  aic[i] <- n*log(sigma2) + 2*(df+1)
}

par(mfrow=c(1,1))

#plot to see optimal lambda
plot(lambda,gcv,type="l",lwd=2,
     xlab="lambda",ylab="GCV",main="8 knots",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
plot(lambda,aic,type="l",lwd=2,main="8 knots",
     xlab="lambda",ylab="AIC",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)

lambdamingcv <- lambda[which(gcv==min(gcv))]
lambdaminaic <- lambda[which(aic==min(aic))]

lambdamingcv;lambdaminaic;

#fit model with lambda=0.4609314 and calculate AIC
S <- B%*%solve(t(B)%*%B + 0.4609314*K2)%*%t(B)
diags <- diag(S)
trs <- mean(diags)
df <- sum(diags)
fit <- as.vector(S%*%fish$Length)
sigma2 <- sum((fish$Length-fit)^2)/(n) #im unsure why they used -22 above but in general i think this is the formula (he uses this within the loop)
AIC_P2 <- n*log(sigma2) + 2*(df+1)#1805.521 (looks about right)

## 20 knots ##
nrknots <- 20
minx <- min(Age)-0.001
maxx <- max(Age)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)

#make the difference matric, here r=2
D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots)
{
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}

#first order penalty matrix
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Age,ord=4)$design

#create grid of lambdas
lambda <- seq(0.00001,50,length=500)
gcv <- rep(0,length(lambda))
aic <- rep(0,length(lambda))

#crossvalidation to find optimal lambda
for(i in 1:length(lambda))
{
  S <- B%*%solve(t(B)%*%B + lambda[i]*K2)%*%t(B)
  diags <- diag(S)
  trs <- mean(diags)
  df <- sum(diags)
  fit <- as.vector(S%*%fish$Length)
  gcv[i] <- mean(((fish$Length-fit)/(1-trs))^2)
  #  aic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + 2*df
  #  bic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + log(n)*df
  sigma2 <- sum((fish$Length-fit)^2)/n
  aic[i] <- n*log(sigma2) + 2*(df+1)
}

par(mfrow=c(1,1))

#make plots to see optimal lambda in terms of GCV and AIc
plot(lambda,gcv,type="l",lwd=2,
     xlab="lambda",ylab="GCV",main="20 knots",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
plot(lambda,aic,type="l",lwd=2,main="20 knots",
     xlab="lambda",ylab="AIC",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)

lambdamingcv <- lambda[which(gcv==min(gcv))]
lambdaminaic <- lambda[which(aic==min(aic))]

lambdamingcv;lambdaminaic

# fit model with lambda=0.0.00001 and calculate AIC
S <- B%*%solve(t(B)%*%B + 0.00001*K2)%*%t(B)
diags <- diag(S)
trs <- mean(diags)
df <- sum(diags)
fit <- as.vector(S%*%fish$Length)
sigma2 <- sum((fish$Length-fit)^2)/(n) #im unsure why they used -22 above but in general i think this is the formula (he uses this within the loop)
AIC_P3 <- n*log(sigma2) + 2*(df+1)#1781.384 (looks about right)

# Making plots of all P-splines
par(mfrow=c(1,3))
####5 knots
nrknots <- 5
minx <- min(Age)-0.001
maxx <- max(Age)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)

ageseq <- seq(minx,maxx,length=893)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1))
{
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}
D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots)
{
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}
K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Age,ord=4)$design
Bfit <- spline.des(knots=knots,ageseq,ord=4,outer.ok=TRUE)$design

lambda <- 0.00001

betahat <- solve(t(B)%*%B + lambda*K2)%*%t(B)%*%Length
fitted <- Bfit%*%betahat

Bfitscaled <- Bfit
for(i in 1:(nrknots+2))
  Bfitscaled[,i] <- Bfit[,i]*betahat[i]

plot(Age, Length,xlab="Age (months)",ylab="Length",
     main="5 knots, lambda = 0.00001",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3)
lines(ageseq,fitted,lwd=2,col="dark blue")

####8 knots
nrknots <- 8
minx <- min(Age)-0.001
maxx <- max(Age)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1))
{
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}
D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots)
{
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}
K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Age,ord=4)$design
Bfit <- spline.des(knots=knots,ageseq,ord=4,outer.ok=TRUE)$design

lambda <- 0.4609314

betahat <- solve(t(B)%*%B + lambda*K2)%*%t(B)%*%fish$Length
fitted <- Bfit%*%betahat

Bfitscaled <- Bfit
for(i in 1:(nrknots+2))
  Bfitscaled[,i] <- Bfit[,i]*betahat[i]

plot(Age, Length,xlab="Age (months)",ylab="Length",
     main="8 knots, lambda = 0.46",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3)
lines(ageseq,fitted,lwd=2,col="dark blue")

####20 knots
nrknots <- 20
minx <- min(Age)-0.001
maxx <- max(Age)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1))
{
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}
D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots)
{
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}
K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,Age,ord=4)$design
Bfit <- spline.des(knots=knots,ageseq,ord=4,outer.ok=TRUE)$design

lambda <- 0.00001

betahat <- solve(t(B)%*%B + lambda*K2)%*%t(B)%*%Length
fitted <- Bfit%*%betahat

Bfitscaled <- Bfit
for(i in 1:(nrknots+2))
  Bfitscaled[,i] <- Bfit[,i]*betahat[i]

plot(Age, Length,xlab="Age (months)",ylab="Length",
     main="20 knots, lambda = 0.00001",
     cex.lab=1.5,cex.axis=1.3,col="dark red",cex=1.3)
lines(ageseq,fitted,lwd=2,col="dark blue")

#compare Pspline AICS
AIC. <- c(AIC_P1, AIC_P2, AIC_P3)
names(AIC.) <- c("model1", "model2", "model3")
sort(AIC.)

#########################################
#### Compare all models based on AIC ####
#########################################

AIC.<- c(AIC1, AIC2, AIC3,AIC4, AIC5, AIC6,AIC_TP1, AIC_TP2, AIC_TP3,AIC_B1, AIC_B2, AIC_B3,AIC_P1, AIC_P2, AIC_P3)
names(AIC.)<-c("poly2", "poly3", "poly5","poly9", "poly12", "poly15","TP1","TP2","TP3","B1","B2","B3","P1","P2","P3")
sort(AIC.)
# therefore final model is P3 or P1 based on AIC (both the same in this case)

#####################################################
######### Answering Research Questions ##############
#####################################################

#convert year and also month to binary (in order to answer the research questions)
fish$Year[fish$Year=="2010"]=1
fish$Year[fish$Year=="1984"]=0

fish$Month[fish$Month %in% c(1,2,3,4,5,6,10,11,12)]=0
fish$Month[fish$Month %in% c(7,8,9)]=1

#take a look at the distribution of length so we can try a model
par(mfrow=c(1,1))
hist(Length,col="blue")

####################################
#### Try to find the best model ####
####################################

#fit model but note the number of knots should be less than the number of covar values (which is 18) so we use 5
#dont use id as predictor
poismodel1 <- gam(Length ~ s(Age,bs="ps",k=17)+factor(Sex) + factor(Year) + factor(Month),
                  family = poisson(link="log"),data = fish)

#model summary
summary(poismodel1)
AIC1<-AIC(poismodel1)
par(mfrow=c(2,2))

#model check
gam.check(poismodel1)

#try quasi poisson
poismodel2 <- gam(Length ~ s(Age,bs="ps",k=17)+factor(Sex) + factor(Year) + factor(Month),
                   family = poisson(link="log"),scale=-1,data = fish) 

#model summary
summary(poismodel2)
AIC2<-AIC(poismodel2)
par(mfrow=c(2,2))

#model check
gam.check(poismodel2)

#try neg binomial
negbin<- gam(Length ~ s(Age,bs="ps",k=17)+factor(Sex) + factor(Year) + factor(Month),
                  family = nb(link="log"), data=fish)

#model summary
summary(negbin)
AIC3<-AIC(negbin)
par(mfrow=c(2,2))

#model check
gam.check(negbin)

#try regression model
linreg<- gam(Length ~ s(Age,bs="ps",k=17)+factor(Sex) + factor(Year) + factor(Month),
             family = gaussian, data=fish)
summary(linreg)
gam.check(linreg)
AIC4<-AIC(linreg)

#compare all models in terms of AIC and fitted vs response graphs 
obs <- fish$Length
fit1 <- fitted(linreg)
fit2<-fitted(poismodel1)
fit3<-fitted(negbin)
fit4<-fitted(poismodel2)

#change margins of display
op0=par()
op1=op0$mar
par(mar=c(4,4,3,3))

par(mfrow=c(2,2))
plot(fit1,obs,xlab="Fitted Values - Linear Regression",ylab="Response") #lin reg
plot(fit2,obs,xlab="Fitted Values - Poisson",ylab="Response") #poisson
plot(fit3,obs,xlab="Fitted Values - Negative Binomial",ylab="Response") #negbin
plot(fit4,obs,xlab="Fitted Values - Quasi Poisson",ylab="Response") #quasi

#compare them all in terms of AIC
AIC(poismodel1,poismodel2,negbin,linreg)
#linear regression is best - use this model

########################################
###### Address Research Questions ######
########################################

# test hypotheses of interest
anova(linreg)

#have a look to see is the coeff for month neg or pos (see report for how i answered these questions)
summary(linreg)


##########################################
### Take log of length as response now ###
##########################################

modelnew<- gam(log(Length) ~ s(Age,bs="ps",k=17)+ factor(Sex) + factor(Year) + factor(Month), family = gaussian,residuals=TRUE, data=fish)
summary(modelnew)
summary(linreg)

#compare to previous model - base on model disagnosis

### Compare R^2 values ###
summary(linreg)
summary(modelnew)

### check if model assumptions are better satisfied ###
par(mfrow=c(2,2))
gam.check(modelnew) 
gam.check(linreg)

#get res vs fit and normality plots
resid1<-residuals(modelnew)
resid2<-residuals(linreg)
par(mfrow=c(1,2))
qqnorm(resid1,xlab="Theoretical Quantiles - New model")
qqline(resid1)
qqnorm(resid2,xlab="Theoretical Quantiles - Original model")
qqline(resid2)
par(mfrow=c(1,2))
plot(fitted(modelnew),resid1)
lines(lowess(resid1 ~ fitted(modelnew)), col = "red")
plot(fitted(linreg),resid2)
lines(lowess(resid2 ~ fitted(linreg)), col = "red")

### compare nonlin relationship of 2 models ###
plot(modelnew,residuals=TRUE,cex=1.3,col='blue',shade=TRUE,xlab="AGE - New Model")
plot(linreg,residuals=TRUE,cex=1.3,col='red',shade=TRUE,xlab="AGE - Original Model")

# get response vs fitted graphs to visually comapre the fits
obs1 <- fish$Length
obs2<- log(fish$Length)
fit1<-fitted(linreg)
fit2<-fitted(modelnew)
op0=par()
op1=op0$mar
par(mar=c(4,4,3,3))

par(mfrow=c(1,2))
plot(fit1,obs1,xlab="Fitted Values - Original model",ylab="Response") #lin reg
lines(lowess(obs1 ~ fit1),col="red")
plot(fit2,obs2,xlab="Fitted Values - New model",ylab="Response") #poisson
lines(lowess(obs2 ~ fit2), col = "red")

#redo F tests to confirm results
anova(modelnew)
#same conclusions

##################################################################
####### Logistic regression - Fish of commercial interest ########
##################################################################

#need to get original vars for month (we dichotomized previously)
fish<-read.table("C:/Users/RobbieBroughton/Documents/KULStuff/Semester2/Generalized Linear Models/Fish_data.txt",header=TRUE)
attach(fish)

#dichotomoize length to make new variable
fish$commercialfish<-as.numeric(fish$Length>=30)

#we assume binomial distribution but need to compare link functions
initial.logit<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Sex) + factor(Year) + factor(Month),family = binomial("logit"),residuals=TRUE)
initial.logit<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Sex) + factor(Year) + factor(Month),family = binomial("logit"),residuals=TRUE)
initial.probit<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Sex) + factor(Year) + factor(Month),family = binomial("probit"),residuals=TRUE)
initial.clog<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Sex) + factor(Year) + factor(Month),family = binomial("cloglog"),residuals=TRUE)
initial.cauchit<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Sex) + factor(Year) + factor(Month),family = binomial("cauchit"),residuals=TRUE)
AIC(initial.logit,initial.probit,initial.clog,initial.cauchit)
#prefer logit link

#summary of logit and inspect relationship of age and binary length
summary(initial.logit)

### variable selection to get best model ###
fullmodel<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Month) + factor(Year) + factor(Sex),family = binomial("logit"),
                 residuals=TRUE)
summary(fullmodel)
anova(fullmodel)
#we find Sex and Year not significant for this binary response

#Using AIC as model selection and we also check for any interactions here
m2<-gam(fish$commercialfish~s(Age,bs="ps",k=17),family = binomial("logit"),
        residuals=TRUE)
summary(m2)

m3<-gam(fish$commercialfish~s(Age,bs="ps",k=17)+factor(Sex),family = binomial("logit"),
        residuals=TRUE)
summary(m3)

m4<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Sex)+factor(Year),family = binomial("logit"),
        residuals=TRUE)
summary(m4)

m5<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Sex)+factor(Month),family = binomial("logit"),
        residuals=TRUE)
summary(m5)

m6<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Month)+factor(Year),family = binomial("logit"),
        residuals=TRUE)
summary(m6)

m7<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Month),family = binomial("logit"),
        residuals=TRUE)
summary(m7)

m8<-gam(fish$commercialfish~s(Age,bs="ps",k=17) + factor(Month)+ Age*factor(Month),family = binomial("logit"),
        residuals=TRUE)
summary(m8)


AIC(initial.logit,m2,m3,m4,m5,m6,m7,m8)

# Hence by AIC, we go for m7= model with 'age' and 'Month' only
# parameters in other models are not significant- hence interaction term also not added

### GOF tests ###

#byhosmerLemeshowtest
hoslem.test(m7$y,fitted(m7),g=10)
NagelkerkeR2(m7)#0.86goodenough
#meaning the model does good job predicting whether the fish can be of commercial interest or not


### Predictive accuracy ###

#Concordance measure
OptimisedConc=function(model)
{
  Data = cbind(model$y, model$fitted.values) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}
OptimisedConc(m7) #97.65%conc= again indicating quality of prediction

### Check for Outliers ###
#Influentialobservations
par(mfrow=c(1,1))
par(pty="s")
par(mar=c(5,6,4,2)+0.1)

cook<-cooks.distance(m7)
plot(cook,type="l",
     xlab="Identification",ylab="Cook's distance",
     cex.lab=1.5,cex.axis=1.3,col="red")
cook
order(cook,decreasing=TRUE)
out

#Influentialobservationsbycook are 594  and 884 but dont seem to be mismeasurements

### Prediction using final model ###
medianmonth<-median(Month)
medianyear<-median(Year)
valuesex<- "Male"
minAge<-min(Age)
maxAge<-max(Age)

grid<-seq(minAge,maxAge,1)
m7_data<-data.frame(Age=grid,Month=medianmonth, Year= medianyear, Sex= valuesex)
str(m7_data)
p1<-predict(m7,newdata=m7_data,se=T,type="response")
par(mfrow=c(1,1))
par(pty="m")
par(mar=c(5,5,4,2)+0.1)
matplot(grid,p1$fit,lty=c(1,2,2),type="l",lwd=1,col="red",
        xlab="Age",ylab="Probability fish is of commercial value",
        cex.lab=1.5,cex.axis=1.3)
legend(2,0.2,legend=c("male fish April 2010"),lwd=1,col="red")
detach(fish)
