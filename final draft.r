###########################################################################
## This code is solution to question 1 and 2 of the assignment 1 of statistical data mining:
## Using Data Mining to build predictive model for first period grades of Student Performance Data Set on the UCI machine learning repository
## Hitesh Santwani
## Created: September 7, 2018
###########################################################################

rm(list = ls())

## Before running the code please make sure all libraries are installed and comment out the setwd command instead use yours :) 
## note : pleaes set the directory according to your dir structure before exceuting the script on R console
# set the current working directory
setwd("/Users/hiteshsantwani/Desktop/codeR")

# load libarary
library(DataExplorer)
library(ggplot2)
library(scales)

## load the data for mathematics and portugueses grades and merge them in the third frame based on the parameters mentioned below
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3))

## priliminary data exploration
## check for missing values
plot_missing(d3)

## plot the bar charts
plot_bar(d3)

## based on the preliminary data exploration we find duplicate columns
## let's remove them

## pre-processing
d3 <- subset(d3, select = -c(freetime.y,health.y, famsup.y, goout.y, Dalc.y, Walc.y, romantic.y, higher.y, activities.y, guardian.y, traveltime.y, studytime.y, schoolsup.y, famrel.y))

## lets rename the common columns that both affect the maths and portuguese results

## not the priority

## Histograms using ggplot
plot_histogram(d3)

ggplot(data = d3, aes(x = G1.x)) + geom_histogram(color = "black") +
scale_x_continuous(breaks = seq(0,100,5)) + 
labs(x = "First period grades", y = "age")

## lets go for co relation heatmap
plot_correlation(d3, type = "c", use = "pairwise.complete.obs")
plot_correlation(d3, type = "d", use = "pairwise.complete.obs")

##lets go for box plots
plot_boxplot(d3, by = "G1.x")
plot_boxplot(d3, by = "G1.y")
ggplot(data = d3, aes(x = G1.x, y = age)) + geom_boxplot()

## lets go for scatter plots
plot_scatterplot(d3, by = "G1.x", size = 1)
plot_scatterplot(d3, by = "G1.y", size = 1)

## Histograms using ggplot
ggplot(data = d3, aes(x = G1.x)) + geom_histogram(color = "black") +
  scale_x_continuous(breaks = seq(0,100,5)) + 
  labs(x = "First period grades", y = "age")

## Scatter plots for age vs first period grades
ggplot(data = d3, aes( x = G1.x , y = age)) + geom_point() + labs(x = "Period 1 grades for Maths", y = "age") +
  geom_smooth(method = "lm")
ggplot(data = d3, aes( x = G1.y , y = age)) + geom_point() + labs(x = "Period 1 grades for Portuguese", y = "age") +
  geom_smooth(method = "lm")

## Scatter plots for activities vs first period grades
ggplot(data = d3, aes( x = G1.x , y = activities.x)) + geom_point() + labs(x = "Period 1 grades for Maths", y = "age") +
  geom_smooth(method = "lm")
ggplot(data = d3, aes( x = G1.y , y = activities.x)) + geom_point() + labs(x = "Period 1 grades for Portuguese", y = "age") +
  geom_smooth(method = "lm")

## histograms and jitter plots invesigating paid and activities vs first period grades
ggplot(data = d3, aes(x = G1.x, fill = activities.x)) + geom_bar() +
  labs(x = "period 1 grades Maths", y = "Extra Cirricular Activities") + coord_flip()  +
  scale_y_discrete()

ggplot(data = d3, aes(x = G1.y, fill = activities.x)) + geom_bar() +
  labs(x = "period 1 grades portuguese", y = "Extra Cirricular Activities") + coord_flip() + 
  scale_y_discrete()

ggplot(data = d3, aes(x = G1.x, fill = paid.x)) + geom_bar() +
  labs(x = "period 1 grades Maths", y = "paid classes") + coord_flip() + 
  scale_y_discrete()

ggplot(data = d3, aes(x = G1.y, fill = paid.x)) + geom_bar() + 
  labs(x = "period 1 grades portuguese", y = "paid classes") + coord_flip() +
  scale_y_discrete()

ggplot(d3, aes(G1.x, activities.x)) + labs(x = "Period 1 grades for Maths", y = "Extra Cirricular Activities") +
  geom_jitter()

ggplot(d3, aes(G1.x, paid.x)) + labs(x = "Period 1 grades for portuguese", y = "Extra Cirricular Activities") +
  geom_jitter()

## checking relations between first period grade  and final period grade
ggplot(data = d3, aes( x = G3.x, y = G1.x)) + geom_point() +
  geom_smooth(method = "lm") + scale_x_continuous(breaks = seq(0,100,5))

## checking relations between school and first period grades
ggplot(data = d3, aes(x = G1.x, fill = school)) + geom_bar() +
  labs(x = "period 1 grades Maths", y = "Extra Cirricular Activities") + coord_flip()  +
  scale_y_discrete()

## checking the relation between failures and first period grades.
ggplot(data = d3, aes( x = G1.x, y = failures.x)) + geom_point() +
  geom_smooth(method = "lm") + scale_x_continuous(breaks = seq(0,100,5))

## there is high correlation between going for higher education and first period grades
ggplot(data = d3, aes(x = G1.x, fill = higher.x)) + geom_bar() +
  labs(x = "period 1 grades Maths") + coord_flip()  +
  scale_y_discrete()

## High correlation between maths first period and portuguese first period grades
ggplot(data = d3, aes( x = G1.x, y = G1.y)) + geom_point() +
  geom_smooth(method = "lm") + scale_x_continuous(breaks = seq(0,100,5)) + labs(x = "period 1 grades Maths", y = "period 1 grades portuguese")

## pre-processing
## removing the outliers
d3$age[!d3$age %in% boxplot.stats(d3$age)$out]

d3$G2.x[!d3$G2.x %in% boxplot.stats(d3$G2.x)$out]
d3$G3.x[!d3$G2.x %in% boxplot.stats(d3$G2.x)$out]

d3$G2.y[!d3$G2.y %in% boxplot.stats(d3$G2.y)$out]
d3$G3.y[!d3$G2.y %in% boxplot.stats(d3$G2.y)$out]

## pre-processing
## transform the variabls first period grades for maths and portuguese to 4 classes from 20

d3$G1.y[d3$G1.y <= 5 & d3$G1.y >= 1] <- 1
d3$G1.y[d3$G1.y <= 10 & d3$G1.y >= 6] <- 2
d3$G1.y[d3$G1.y <= 15 & d3$G1.y >= 11] <- 3
d3$G1.y[d3$G1.y <= 20 & d3$G1.y >= 16] <- 4

d3$G1.x[d3$G1.x <= 5 & d3$G1.x >= 1] <- 1
d3$G1.x[d3$G1.x <= 10 & d3$G1.x >= 6] <- 2
d3$G1.x[d3$G1.x <= 15 & d3$G1.x >= 11] <- 3
d3$G1.x[d3$G1.x <= 20 & d3$G1.x >= 16] <- 4


## solution to question 2

## model 1 for predicting maths grade 1 output
## precise model
model1 = lm(G1.x ~ age*higher.x*absences.x*failures.x*G2.x*G3.x, data=d3)
summary(model1)

## Extravagent model with interactions involving * and :
model2 = lm(G1.x ~ age*higher.x * absences.x * failures.x * G2.x * G3.x * Medu * Fedu * studytime.x : traveltime.x * freetime.x, data=d3)
summary(model2)

## First period grades vs all the columns
model3 = lm(G1.x ~ ., data=d3)
summary(model3)
save(d3, file ="d3.RData")
## it is clear from the summary that model 1 is good model so are the interactions associated with this model.