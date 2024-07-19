rm(list=ls())

library(recommenderlab)
library(ggplot2)
library(dplyr)

#simple latent example
mat = matrix(c(0,0,0,1,1,1,
               0,0,0,1,1,1,
               0,0,0,1,1,1,
               -1,-1,-1,1,1,1,
               1,1,1,-1,-1,-1,
               1,1,1,-1,-1,-1,
               1,1,1,-1,-1,-1,), byrow=TRUE, nrow = 7)


rownames(mat)= paste("User",1:7, sep="")
colnames(mat)= paste("item", 1:6, sep="")
mat

#convert mat to a really rating matrix

r=as(mat,"realRatingMatrix")

#lets use the recomenderlab Package

recommenderRegistry$get_entries()
recommenderRegistry$get_entry("SVDF", datatype='realRatingMatrix')

model_params =list(k=2, gamma =0, lambda = 0.1, normalize = 'center')

model_0 =Recommender(r,"SVDF", parameter =model_params)
model_0

Qmat = model_0@model[["svd"]][["U"]]
Pmat = model_0@model[["svd"]][["V"]]

round(Qmat,1)
round(Pmat,1)

Qmat%*%t(Pmat)

round(Qmat%*%t(Pmat),0)






#lets use the MovieLense data set
rm(list=ls())
data('MovieLense')

MovieLense@data@Dimnames

#Intial Analysis

rmat = as(getRatingMatrix(r),'matrix')
rmat =data.frame(rmat)
rmat[rmat==0] = NA

str(rmat)


#Lets extend our additional data
MovieLenseUser$avgr = round(rowMeans(rmat, na.rm = TRUE),2)

str(MovieLenseUser)

#AVERAGE USER RATINGS
ggplot(MovieLenseUser, aes(x=avgr))+
  geom_histogram(fill='grey', color='black')+
  ggtitle('Average User Ratings')

#Differences in Average User Ratings by sex
MovieLenseUser %>%
  group_by(sex)%>%
  summarize(avgs = mean(avgr)) %>%
  ggplot(aes(x=sex, y=avgs, fill= sex))+
  geom_bar(stat='identity')+
  ggtitle("average rating by sex")

# Difference by Occupation 
Moviesum = MovieLenseUser%>%
  group_by(occupation)%>%
  summarize(avgo=mean(avgr), avga=mean(age))

ggplot(Moviesum, aes(x=occupation, y=avgo, fill = occupation))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x=element_text(angle=90))

# Difference by occupation and age
ggplot(Moviesum, aes(x=avga, y=avgo, col = occupation, label=occupation))+
  geom_point(size=5)+
  geom_text(hjust=0, nudge_x = 1)

# Movie Meta Data
str(MovieLenseMeta)

 MovieLenseMeta$avgr = round(colMeans(rmat, na.rm=TRUE),2)

ggplot(MovieLenseMeta, aes(x=avgr))+
  geom_histogram(fill='grey', colour='black')
ggtitle('average movie ratings')


#average movie ratings over time
ggplot(MovieLenseMeta, aes(x=year, y=avgr))+
  geom_point()+
  ggtitle("average movie ratings over time")

#which movie got the most ratings
MovieLenseMeta%>%
  select(title, year, avgr)%>%
  group_by(year)%>%
  summarize(n())


#cross validation
set.seed(1234)
e=evaluationScheme(r, method='split', train=0.9, given=10, k =1)
min(rowCounts(r))

#lets look at the data from the split
getData(e,'train')
getData(e, 'known') # known part of the test part
getData(e,"unknown") # not known part of the test data (to predict)




#model training
#model parameters for the latent factor model

model_params = list(k=2, gamma= 0.015, lambda = 0.001, normalize ='center')
model_0 = Recommender(getData(e,"train"),"SVDF", parameter = model_params)
model_1 = Recommender(getData(e,"train"),"UBCF", param = list(normalize='center', method='cosine'))
model_2 = Recommender(getData(e,"train"),"IBCF", param = list(normalize='center', method='cosine'))


# SORT LOOK AT THE LATENT CONCEPT (for users an d items)
Qmat = model_0@model[["svd"]][["U"]]
Pmat = model_0@model[["svd"]][["V"]]

#lets make our predictions using the model we trained

pred_0 = predict(model_0, getData(e,'known'),type='ratings')
pred_1 = predict(model_1, getData(e,'known'),type='ratings')
pred_2 = predict(model_2, getData(e,'known'),type='ratings')


#calculation of the selected error measures

error = rbind(LatFac = calcPredictionAccuracy(pred_0,getData(e,'unknown')),
              UBCF = calcPredictionAccuracy(pred_1,getData(e,'unknown')),
              IBCF=calcPredictionAccuracy(pred_2,getData(e,'unknown')))

#plot the comparison of methods
barplot(error, main='Comparison of Recommender systems', beside=TRUE, xlab='metrics', ylab='error', col=c('brown', 'darkorange', 'yellow'))
legend('topleft', legend =c('Latent Factor', 'UBCF', 'IBCF'), fill = c('brown', 'darkorange', 'yellow'), bty='n')


pred_1
realRatingMatrix

















