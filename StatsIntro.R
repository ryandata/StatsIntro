# Probability and Statistics with Small Data
# Ryan Womack
# Rutgers University
# Prepared for Data-Driven Management
# Executive Education Mini-MBA Course
# 2019-05-15

# install packages (this may take a while)
install.packages("psych", dependencies=TRUE)
install.packages("tabplot", dependencies=TRUE)
install.packages("lattice", dependencies=TRUE)
install.packages("rstan", dependencies=TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("truncnorm", dependencies = TRUE)

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
library(psych)
describe(pricedata)

# calculate difference
mydiff<-(pricedata$Barry.Price - pricedata$Mary.Price)
mydiff
sign(mydiff)

# t-Test
t.test(mydiff)

# 2 sample t-test
t.test(pricedata$Barry.Price, pricedata$Mary.Price, paired=TRUE)

# A/B test simulation

# see appendix for how this data was generated

# two samples of 100,000 observations have been created
# suppose the number represents the dollar amount sold on a website under two different designs (A,B)
# can you tell if there is a significant difference in the average price between A and B?

# read in data
sample_A_full <- read.csv("groupA.csv")
sample_B_full <- read.csv("groupB.csv")

sample_A_full <- sample_A_full$x
sample_B_full <- sample_B_full$x

# create small, medium, and large extracts

sample_A_small <- sample(sample_A_full, 10)
sample_B_small <- sample(sample_B_full, 10)

sample_A_medium <- sample(sample_A_full, 100)
sample_B_medium <- sample(sample_B_full, 100)

sample_A_large <- sample(sample_A_full, 1000)
sample_B_large <- sample(sample_B_full, 1000)

# t-test for small
t.test(sample_A_small, sample_B_small, paired=FALSE)

# t-test for medium
t.test(sample_A_medium, sample_B_medium, paired=FALSE)

# t-test for large
t.test(sample_A_large, sample_B_large, paired=FALSE)

# t-test for full
t.test(sample_A_full, sample_B_full, paired=FALSE)

# try this again as a function

my_t_test <- function()
{

sample_A_full <- read.csv("groupA.csv")
sample_B_full <- read.csv("groupB.csv")

sample_A_full <- sample_A_full$x
sample_B_full <- sample_B_full$x

# create small, medium, and large extracts

sample_A_small <- sample(sample_A_full, 10)
sample_B_small <- sample(sample_B_full, 10)

sample_A_medium <- sample(sample_A_full, 100)
sample_B_medium <- sample(sample_B_full, 100)

sample_A_large <- sample(sample_A_full, 1000)
sample_B_large <- sample(sample_B_full, 1000)

# t-test for small
small <- t.test(sample_A_small, sample_B_small, paired=FALSE)

# t-test for medium
medium <- t.test(sample_A_medium, sample_B_medium, paired=FALSE)

# t-test for large
large <- t.test(sample_A_large, sample_B_large, paired=FALSE)

# t-test for full
full <- t.test(sample_A_full, sample_B_full, paired=FALSE)

output<- list("small", small, "medium", medium, "large", large, "full", full)
return(as.data.frame(do.call(cbind, output)))
}

# now try it out - each time it will generate different random samples

my_t_test()




# Cell Data
celldata<-read.csv("cellphoneservice.csv")
celldata

summary(celldata)
describe(celldata)

library(tabplot)
tableplot(celldata)

# linear regression on Wind Speed
library(lattice)

cor(celldata$Bad_Calls, celldata$Wind_Speed)
plot(celldata$Bad_Calls~celldata$Wind_Speed)

my_reg_wind<-lm(Bad_Calls ~ Wind_Speed, data=celldata)
summary(my_reg_wind)
plot(my_reg_wind)
xyplot(celldata$Bad_Calls~celldata$Wind_Speed, type=c("p","r"))

# linear regression on Pressure

cor(celldata$Bad_Calls, celldata$Pressure)
plot(celldata$Bad_Calls~celldata$Pressure)
xyplot(celldata$Bad_Calls~celldata$Pressure, type=c("p","r"))


my_reg_pressure<-lm(Bad_Calls ~ Pressure, data=celldata)
summary(my_reg_pressure)
plot(my_reg_pressure)

# multivariate regression

cor(celldata$Pressure, celldata$Wind_Speed)
splom(celldata)
splom(celldata[,-1])

my_reg_multi<-lm(Bad_Calls~Wind_Speed + Pressure, data=celldata)
summary(my_reg_multi)
plot(my_reg_multi)


# Bayesian Computation
# https://rstudio.cloud/project/56157

# install.packages("rstan", dependencies = TRUE)
library(rstan)
rstan_options(auto_write = TRUE)

# plot beta densities

a=1
b=1
p=seq(0,1,.002)
plot(p,dbeta(p,a,b),typ='l')

# Brute force approach to the binomial

# y~Binomal(p,N)
# p~beta(a,b)

# find p|y

y=6
n=10
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


# appendix on generating data
#   this function was used to generate two random data samples, examples of which are on the github site
#   try different numbers for n, mean, and sd
#   for the first in-class example, we will use n=100000,mean=100,sd=10
#   note we impose a lower bound (0) and upper bound (1000) on the numbers
#   therefore the distribution is a truncated normal

library(truncnorm)

myexperiment<-function(n,mean,sd)
{
  rtruncnorm(n,0,1000,mean,sd)
}

groupA<-myexperiment(100000,100,30)
groupB<-myexperiment(100000,105,30)

write.csv(groupA, file="groupA.csv")
write.csv(groupB, file="groupB.csv")