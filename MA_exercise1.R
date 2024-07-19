rm(list=ls())

library(ggplot2)
library(dplyr)
library(corrplot)
install.packages("patchwork")
library(patchwork)
library(shiny)
#
#lets load our data
datanew=read.csv("datanew.csv", header=TRUE, sep=',')
str(datanew)
#check for missing values
any(is.na(datanew))
#datanew=datanew[complete.cases(datanew),]

#lets understand our dataset better
head(datanew)
datanew[1:5,2]
datanew[1:5, 'Income']
datanew$Income[1:5]
#picking the income and Age of the second and seventh column
datanew[c(2,7), c('Income','Age')]#colnames(datanew) givesus the name of the columns



datanew$Marriage=factor(datanew$Marriage)#changing to categorical data
datanew$Education=factor(datanew$Education)#changing eduction to categorical data

#give a quick view of how the data looks like
summary(datanew[,c('Income','Age','Marriage','Education')])

#ploting /visualization
dev.off()
ggplot(datanew, aes(x=Income))+
  geom_histogram(colour='black', fill='lightblue',bins=50)+
  ggtitle('Income Distribution')

ggplot(datanew, aes(x=Age))+
  geom_histogram(colour='black', fill='lightblue',bins=50)+
  ggtitle('Age')

ggplot(datanew, aes(x=Education))+
  geom_bar(colour='black',fill='lightblue')+
  ggtitle('Education levels')
  xlab('')#to overwrite the x and y labels

  ggplot(datanew, aes(x=Education))+
    geom_bar()
   
  
  
  
#plottingincome by eductional level
  ggplot(datanew, aes(x=Education, y=Income))+
    geom_boxplot(colour='black', fill='lightblue')+
    ggtitle('Income Distributions for Educational Levels')

#looking at the products
summary(datanew[,c(8:14)])

p1=ggplot(datanew, aes(x=MntWines))+
  geom_histogram(colour='black', fill='lightblue')+
  ggtitle('Wines')

p2=ggplot(datanew, aes(x=MntMeatProducts))+
  geom_histogram(colour='black', fill='lightblue')+
  ggtitle('Meat')

p3=ggplot(datanew, aes(x=MntRegularProds))+
  geom_histogram(colour='black', fill='lightblue')+
  ggtitle('Regular Products')

p1
p1+p2+p3
p1/p2
(p1+p2+p3)

#lets have a look at the  campaigns acceptance
summary(datanew$AcceptedCmpOverall)

ggplot(datanew, aes(x=factor(AcceptedCmpOverall),y=Income))+
  geom_boxplot(colour='black', fill='lightblue')

ggplot(datanew, aes(x=factor(AcceptedCmpOverall),y=NumWebPurchases))+
  geom_boxplot(colour='black', fill='lightblue')

# Which campaigns were most frequently accepted?
datanew%>%
  select(c(2,21:26))%>%
  filter(AcceptedCmpOverall>0)%>%
  summarize_all(mean)

datanew%>%
  select(c(2,21:26))%>%
  filter(AcceptedCmpOverall>0)%>%
  group_by(AcceptedCmpOverall)%>%
  summarize_all(mean)





