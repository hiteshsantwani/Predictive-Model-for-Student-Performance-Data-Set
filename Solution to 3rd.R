#######################################################
# This code is solution to HW 1 question 3 Statistical data mining
#
# Hitesh santwani
# Created: 09/13/2018
#######################################################

rm(list = ls())

## install packages
# install.packages("MASS")
# install.packages("ggplot2")
# install.packages("DataExplorer")

## load libraries
library(MASS)
library(ggplot2)
library(DataExplorer)

## load data
Boston$chas <- as.factor(Boston$chas)
nrow(Boston)

head(Boston)

## a) correlation matrix and some detailed graphs

ggplot(data = Boston, aes( x = Boston$nox, y = Boston$crim)) + geom_point() +
geom_smooth(method = "lm")

ggplot(data = Boston, aes( x = Boston$rm, y = Boston$crim)) + geom_point() +
geom_smooth(method = "lm")

ggplot(data = Boston, aes( x = Boston$age, y = Boston$crim)) + geom_point() +
geom_smooth(method = "lm")

ggplot(data = Boston, aes( x = Boston$dis, y = Boston$crim)) + geom_point() +
geom_smooth(method = "lm")

ggplot(data = Boston, aes( x = Boston$age, y = Boston$medv)) + geom_point() +
geom_smooth(method = "lm")

Boston$chas <- as.numeric(Boston$chas)
Boston$rad <- as.numeric(Boston$rad)
pairs(Boston)

## b) predictors associated with per - capita crime rate

## we can use co-relation matrix or heatmap
plot_correlation(Boston, maxcat = 5L, use = "pairwise.complete.obs")

# Older homes and crime
ggplot(data = Boston, aes( x = Boston$age, y = Boston$crim)) + geom_point() +
  geom_smooth(method = "lm")

# Closer to work and crime
ggplot(data = Boston, aes( x = Boston$dis, y = Boston$crim)) + geom_point() +
  geom_smooth(method = "lm")

#accessibility to radial highways and crime
ggplot(data = Boston, aes( x = Boston$rad, y = Boston$crim)) + geom_point() +
  geom_smooth(method = "lm")

# tax rate and crime
ggplot(data = Boston, aes( x = Boston$tax, y = Boston$crim)) + geom_point() +
  geom_smooth(method = "lm")

# Higher pupil:teacher ratio, more crime
ggplot(data = Boston, aes( x = Boston$ptratio, y = Boston$crim)) + geom_point() +
  geom_smooth(method = "lm")


##c) Do any of the suburbs of Boston appear to have particularly high crime rates ? Tax rates ? Pupil-teacher ratios 


# Most of the Boston is safe except 18 Suburbs
## there are indeed some neighborhoods where the crime rate is alarmingly high
ggplot(data = Boston, aes(x = crim)) + geom_histogram(color = "black") +
scale_x_continuous(breaks = seq(0,50,5)) + 
labs(x = "Crime rate", y = "Number of Suburbs")
nrow(Boston[Boston$crim > 20, ])


# Big difference between suburbs with low tax rates and a peak
## There are few neighborhoods where tax rates are relatively higher
ggplot(data = Boston, aes(x = tax)) + geom_histogram(color = "black") +
  scale_x_continuous(breaks = seq(0,800,50)) + 
  labs(x = "Tax rates", y = "Number of Suburbs")
nrow(Boston[Boston$tax == 666, ])


# a skew towards high ratios 
ggplot(data = Boston, aes(x = ptratio)) + geom_histogram(color = "black") +
scale_x_continuous(breaks = seq(0,50,2.5)) + 
labs(x = "Pupil-teacher ratio by town", y = "Number of Suburbs")
nrow(Boston[Boston$ptratio > 20, ])

qplot(Boston$crim, binwidth=2.5 , xlab = "Crime rate", ylab="Suburbs" )

qplot(Boston$ptratio, binwidth=2.5, xlab ="Pupil-teacher ratio", ylab="Suburbs")


## need to comment on the range of each predictor
summary(Boston$crim)
## from summary range of the predictor is 88.9762 - 0.00632
summary(Boston$tax)
## from summary range of the predictor is 711 - 187
summary(Boston$ptratio)
## from summary range of the predictor is 22 - 12.6


## d) .

more_than_seven <- subset(Boston, rm>7)
nrow(more_than_seven)
rm("more_than_seven")

more_than_eight <- subset(Boston, rm>8)
nrow(more_than_eight)

summary(more_than_eight)


