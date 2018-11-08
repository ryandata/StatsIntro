# Probability and Statistics with Small Data
# Ryan Womack
# Rutgers University
# Prepared for Data-Driven Management
# Executive Education Mini-MBA Course
# 2018-11-08

# install packages (this may take a while)
install.packages("psych", dependencies=TRUE)

# Card draw
pclub<-0.25
for (i in 1:2)
  final_prob<-pclub^i
final_prob

club<-function(n)
{
  for (i in 1:n)
    final_prob<-pclub^i
  final_prob
}

# more precisely
exact_prob<-(1/4)*(12/51)*(11/50)
exact_prob
1-exact_prob

precise_club<-function(n)
{
  final_prob=1
  for (i in 1:n)
    final_prob<-final_prob*((14-i)/(53-i))
  final_prob
}

deck<-c("club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart",
        "club","diamond","spade","heart")

sample(deck, 3)

# Price Quotes
pricedata<-read.csv("pricequotes.csv")
pricedata
mymean<-c(mean(pricedata$Barry.Price), mean(pricedata$Mary.Price))
mymean
mysd<-c(sd(pricedata$Barry.Price), sd(pricedata$Mary.Price))
mysd

# psych library is a useful shortcut
install.packages("psych",dependencies = TRUE)
library(psych)
describe(pricedata)

# calculate difference
mydiff<-(pricedata$Barry.Price - pricedata$Mary.Price)
mydiff
sign(mydiff)

#t-Test
t.test(mydiff)

#2 sample t-test
t.test(pricedata$Barry.Price, pricedata$Mary.Price, paired=TRUE)

# Cell Data
celldata<-read.csv("cellphoneservice.csv")
celldata

summary(celldata)
describe(celldata)

install.packages("tabplot", dependencies = TRUE)
library(tabplot)
tableplot(celldata)

#linear regression on Wind Speed

cor(celldata$Bad_Calls, celldata$Wind_Speed)
plot(celldata$Bad_Calls~celldata$Wind_Speed)

my_reg_wind<-lm(Bad_Calls ~ Wind_Speed, data=celldata)
summary(my_reg_wind)
plot(my_reg_wind)

#linear regression on Pressure

cor(celldata$Bad_Calls, celldata$Pressure)
plot(celldata$Bad_Calls~celldata$Pressure)

my_reg_pressure<-lm(Bad_Calls ~ Pressure, data=celldata)
summary(my_reg_pressure)
plot(my_reg_pressure)

#multivariate regression

cor(celldata$Pressure, celldata$Wind_Speed)
install.packages("lattice", dependencies = TRUE)
library(lattice)
splom(celldata)
splom(celldata[,-1])

my_reg_multi<-lm(Bad_Calls~Wind_Speed + Pressure, data=celldata)
summary(my_reg_multi)
plot(my_reg_multi)


# Bayesian Computation
# https://rstudio.cloud/project/56157

install.packages("rstan", dependencies = TRUE)
library(rstan)
rstan_options(auto_write = TRUE)

#plot beta densities

a=1
b=1
p=seq(0,1,.002)
plot(p,dbeta(p,a,b),typ='l')

#Brute force approach to the binomial

# y~Binomal(p,N)
# p~beta(a,b)

# find p|y

y=78
n=100
a=1
b=1

product=function(p) dbinom(y,n,p)*dbeta(p,a,b)
p.data=integrate(product,0,1)$value

plot(p,product(p)/p.data,typ="l",lwd=3,col="green",lty=2)
lines(p,dbeta(p,y+a,n-y+b),col='darkred')
lines(p,dbeta(p,a,b),col='darkblue')


# see 
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

# You can run many of the BUGS examples 
# and some others that we have created in Stan by executing

model <- stan_demo()

# and choosing an example model from the list that pops up. 
# The first time you call stan_demo(), it will ask you if 
# you want to download these examples. You should choose 
# option 1 to put them in the directory where rstan was 
# installed so that they can be used in the future 
# without redownloading them. The model object above 
# is an instance of class stanfit, so you can call 

print(model)
plot(model)

# pairs, extract, etc. on it afterward.

# another explanatory link
# http://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/src/bayesianstats.html

# and another
# https://alexanderetz.com/2015/07/25/understanding-bayes-updating-priors-via-the-likelihood/
