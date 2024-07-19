rm(list=ls())
install.packages("recommenderlab")
library(recommenderlab)
library(ggplot2)
library(dplyr)

#

#First simple rating matrix

mat = matrix(c(4,NA,NA,5,1,NA,NA,
               5,NA,5,NA,NA,4,NA,
               NA,NA,NA,2,4,4,NA,
               2,NA,NA,1,NA,5,NA), byrow=TRUE, nrow=4)
mat
#Give the matrix colnames and Rownames
rownames(mat)= paste("User",1:4, sep='')
colnames(mat)= paste("items",1:7, sep='')
mat


barplot(rowMeans(mat,na.rm = TRUE),ylab = "Average Rating", main = "Average user Ratings")
barplot(colMeans(mat,na.rm = TRUE),ylab = "Average Rating", main = "Average item Ratings")

#lets convert our data into a Rating MATRIX (real- valued)
r<-as(mat,'realRatingMatrix')
r
getRatings(r)
getRatingMatrix(r)

# Mean-centered data
rnorm=normalize(r, method='center', row=TRUE)

getRatingMatrix(rnorm)

avgr = rowMeans(mat,na.rm = TRUE)

#Illustrate the differences
dev.off()
image(r,main = 'Raw Ratings')
image(rnorm,main = 'Main - centered Ratings')


#cosine similarity
similarity(r, method="cosine")
similarity(rnorm, method="cosine")

# our simple user-based collaborative filter

ourCF =function(r,user,item, k=2, sw=TRUE){
  
  #only consider rows of neighbours that have ratings for the item
  rrated =r[as.logical(hasRating(r[,item])),]
  #calculate similarity values
  simr = similarity(r[user,],rrated, method='cosine')
  #determine similar users and ratings
  simusers =order(simr, decreasing = TRUE)[1:k]
  simratings = getRatings(rrated[simusers,item])
  simvalues =simr[simusers]
  #calculate user-based CF ratings
  if(sw){
    myrating = weighted.mean(simratings,simvalues)
  }else{
    myrating=mean(simratings)
  }
  #Return the ouput
  return(myrating)
}
#testing our function
ourCF(r, user=1, item=6, k=2, sw=FALSE)
ourCF(r, user=1, item=6, k=2, sw=TRUE)

ourCF(rnorm, user=1, item=6, k=3, sw=FALSE) + avgr[1]
ourCF(rnorm, user=1, item=6, k=2, sw=TRUE) + avgr[1]


# Recommederlab function for user-based collaborative filter (UCBF)
recommenderRegistry$get_entry('UBCF', datatype ='realRatingMatrix')

#set up the model parameters
model_params = list(method ='cosine',
                   nn = 2+1,
                   sample = FALSE,
                   normalize = 'center',
                   weighted = FALSE)

# USING THE UBCF
modelUB= Recommender(r,'UBCF',parameter = model_params)

#Apply model to missing ratings
predUB = predict(modelUB,r, type='ratings')
getRatingMatrix(predUB)



# alternative way

listUB = as(predUB, "list")
list$`0`




#let's now use the item-based collaborative filter (IBCF)
recommenderRegistry$get_entry('IBCF', datatype ='realRatingMatrix')

#set up the model parameters
model_params = list(method ='cosine',
                    k = 2+1,
                    normalize = 'center')

#Lets run the IBCF
modelIB = Recommender(r, 'IBCF', parameter = model_params)

#lets make our predictions
predIB = predict(modelIB, r, type ='ratings')
getRatingMatrix(predIB)


#second example

rm(list=ls())
data("Jester5k")
set.seed(1234)
r=sample(Jester5k, 1000)

rmat = as(r,'matrix')

#lets load ProductTypes
products =read.csv("ProductTypes.csv", header = TRUE, sep=";")

products
str(products)

barplot(rowMeans(rmat,na.rm = TRUE),ylab = "Average Rating", main = "Average user Ratings")
barplot(colMeans(rmat,na.rm = TRUE),ylab = "Average Rating", main = "Average item Ratings")
dev.off()
products$AvgRating = colMeans(rmat, na.rm=TRUE)

ggplot(products, aes(x=Product, y=AvgRating, fill =Type))+
  geom_bar(stat= 'identity')+
  ggtitle('Average Product Ratings')+
  theme(axis.text.x = element_text(angle =90))


# average rating per product
typeavg = products %>%
  group_by(Type)%>%
  summarise(Avg=mean(AvgRating))

ggplot(typeavg, aes(x=Type, y=Avg, fill =Type))+
  geom_bar(stat ='identity')+
  ggtitle("Average Rating by Type")

# Building our recommender system

#set up the model parameters
model_params = list(method ='cosine',
                    nn = 10+1,
                    sample = FALSE,
                    normalize = 'center',
                    weighted = TRUE)

# USING THE UBCF
modelUB= Recommender(r,'UBCF',parameter = model_params)

#Apply model to missing ratings
predUB = predict(modelUB,r, type='ratings')

getRatingMatrix(predUB)

resultmat = as(getRatingMatrix(predUB['u20648',]),'matrix')

colnames(predUB)[order(resultmat,decreasing = TRUE)[1:3]]

# instead of getting the ratings, lets get the top rated items
predUB = predict(modelUB, r, type='topN', n=3)
topitems= as(predUB,'list')

#top items for user 1
topitems$`0`

#
topitems = as.data.frame(unlist(topitems))
topitems = as.data.frame(table(topitems))
topitems

colnames(topitems) = c("Product", "Frequency")
#lets join this information to our data frame
productsagg= merge(products, topitems, by="Product")


#lets make some visualizations

#Which products would be frequently recommended
ggplot(productsagg,aes(x=Product, y=Frequency, fill =Type))+
  geom_bar(stat='identity')+
  ggtitle('Frequency of Recommendation')+
  theme(axis.text.x=element_text(angle=90))

productsagg%>%
  arrange(Frequency)

# lets compare the average ratings with average frequencies
productsagg%>%
  filter(AvgRating>=0)%>%
  ggplot(aes(x=AvgRating, y=Frequency, col =Type, label=Product))+
  geom_point(size =5)+
  geom_text(hjust=0, nudge_x = 0.05)+
  ggtitle('Average Rating and Frequency of Recommedations')


# how many products are recommended by category
productsagg %>%
  filter(AvgRating >=0) %>%
  group_by(Type)%>%
  summarise(n(), Avg=mean(AvgRating,na.rm=TRUE))












