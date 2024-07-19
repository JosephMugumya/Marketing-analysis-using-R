rm(list=ls())

dev.off()

library(NbClust)
library(purrr)
library(scales)
library(dplyr)
library(ggplot2)
library(patchwork)

#First data set

initdata =data.frame(read.csv("artificial2.csv", header=TRUE, sep=","))

#lets have an initial glance at the data

str(initdata)

summary(initdata)

initdata

# check thru visualization if there are clusters
ggplot(initdata,aes(x=BrandLoyalty, y=PriceSensitivity))+
  geom_point()+
  ggtitle("First Data set")


# lets scale our data
data = data.frame(apply(initdata,2,rescale, to=c(0,1)))
summary(data)



###########################################
# Hierarchical Clustering: get our distance matrix 
dismat = dist(data, method='euclidean')
#################################################
# do the actual clustering
hmdl = hclust(dismat, method = "complete")

# lets make our dendogram
dendhmdl = as.dendrogram(hmdl)

#lets plot our findings
plot(dendhmdl, xlab="observation",ylab="Height of the Tree",
     main="Dendrogram")

#LETS GET THE TWO CLUSTER SOLUTION
clustermember = factor(cutree(hmdl, k=2))

#lets plot with the unscaled data to understand better
ggplot(initdata, aes(x=BrandLoyalty, y=PriceSensitivity,
       col = clustermember))+
  geom_point()+
  ggtitle("Two cluster solution")

#LETS GET THE Three CLUSTER SOLUTION
clustermember = factor(cutree(hmdl, k=3 ))

#lets plot with the unscaled data to understand better
ggplot(initdata, aes(x=BrandLoyalty, y=PriceSensitivity,
                     col = clustermember))+
  geom_point()+
  ggtitle("Three cluster solution")



#########################################
##K MEANS
#########################################

kmeansmdl = kmeans(data, centers = 2, nstart=25)

kmeansmdl$cluster
kmeansmdl$centers
kmeansmdl$withinss
kmeansmdl$tot.withinss

ggplot(data, aes(x=BrandLoyalty, y=PriceSensitivity,
                     col = factor(kmeansmdl$cluster)))+
  geom_point()+
  geom_point(aes(x=kmeansmdl$centers[1,1], y=kmeansmdl$centers[1,2]), colour ="black", size=5)+
  geom_point(aes(x=kmeansmdl$centers[2,1], y=kmeansmdl$centers[2,2]), colour ="black", size=5)+
  ggtitle("Kmeans - Two cluster solution")



kmeansmdl = kmeans(data, centers = 3, nstart=25)

kmeansmdl$cluster
kmeansmdl$centers
kmeansmdl$withinss
kmeansmdl$tot.withinss

ggplot(data, aes(x=BrandLoyalty, y=PriceSensitivity,
                 col = factor(kmeansmdl$cluster)))+
  geom_point()+
  geom_point(aes(x=kmeansmdl$centers[1,1], y=kmeansmdl$centers[1,2]), colour ="black", size=5)+
  geom_point(aes(x=kmeansmdl$centers[2,1], y=kmeansmdl$centers[2,2]), colour ="black", size=5)+
  geom_point(aes(x=kmeansmdl$centers[3,1], y=kmeansmdl$centers[3,2]), colour ="black", size=5)+
  ggtitle("Kmeans - Two cluster solution")



# Elbow Methods   
par(mfow= c(1,1))
tot_within_ss = map_dbl(1:10, function(k){
  kmeansmdl = kmeans(data, centers = k, nstart =25)
  kmeansmdl$tot.withinss
})

tot_within_ss = data.frame(tot_within_ss)


ggplot(tot_within_ss, aes(x=1:10, y=tot_within_ss,))+
                geom_line()+
  ggtitle('Elbow Method')

# another way to plot

plot(1:10, tot_within_ss$tot_within_ss, type='o')
grid()


# lets do the silhouette Method, calinski- Harabasz, Gap-statistic

silClust = NbClust(data, distance='euclidean', min.nc=2, max.nc =10, method = 'kmeans', index='silhouette')
GapClust  = NbClust(data,distance='euclidean', min.nc=2, max.nc =10, method = 'kmeans', index='gap')
CHClust = NbClust(data, distance='euclidean', min.nc=2, max.nc =10, method = 'kmeans', index='ch')

par(mfrow=c(1,3))
plot(2:10, silClust$All.index, type='o', col='blue', xlab='Numberof Clusters k', ylab='Silhouette Width') 
grid()
plot(2:10, GapClust$All.index, type='o', col='blue', xlab='Numberof Clusters k', ylab='Gap statistics Width') 
grid()
plot(2:10, CHClust$All.index, type='o', col='blue', xlab='Numberof Clusters k', ylab='Calinski Harabasz Width')
grid()


# lets do the clustering of our data
kmeansmdl = kmeans(data, centers = 3, nstart = 25)
initdata$cluster = kmeansmdl$cluster
str(initdata)

#lets look at the data

initdata %>%
  group_by(cluster)%>%
  summarise_all(c(Avg=mean,std=sd))






##########################################################
# lets use the second data set
##########################################################
rm(list=ls())
dev.off()
initdata =data.frame(read.csv("Mall.csv", header=TRUE, sep=";"))
str(initdata)

#lets have an initial glance at the data


colnames(initdata)= c("ID", "LoyaltyCard","Age", "Income", "Spending")
str(initdata)

initdata = initdata[,-1]
str(initdata)

plot(initdata)

# lets scale our data
data = data.frame(apply(initdata,2,rescale, to=c(0,1)))
summary(data)

###########################################
# Hierarchical Clustering: get our distance matrix 
dismat = dist(data, method='euclidean')
#################################################
# do the actual clustering
hmdl = hclust(dismat, method = "complete")

# lets make our dendogram
dendhmdl = as.dendrogram(hmdl)

#lets plot our findings
par(mfrow = c(1,1))
plot(dendhmdl, xlab="observation",ylab="Height of the Tree",
     main="Dendrogram")



#LETS GET THE TWO CLUSTER SOLUTION
clustermember = factor(cutree(hmdl, k=2))

plot(initdata, col =clustermember, main="Two Clusters")
grid()







#lets plot with the unscaled data to understand better
ggplot(initdata, aes(x=BrandLoyalty, y=PriceSensitivity,
                     col = clustermember))+
  geom_point()+
  ggtitle("Two cluster solution")


#lets remove the loyaltycard from our data

data= data[,-1]
data


#Elbow method
par(mfow= c(1,1))
tot_within_ss = map_dbl(1:10, function(k){
  kmeansmdl = kmeans(data, centers = k, nstart =25)
  kmeansmdl$tot.withinss
})

tot_within_ss = data.frame(tot_within_ss)




# another way to plot

plot(1:10, tot_within_ss$tot_within_ss, type='o')
grid()


# lets do the silhouette Method, calinski- Harabasz, Gap-statistic

silClust = NbClust(data, distance='euclidean', min.nc=2, max.nc =10, method = 'kmeans', index='silhouette')
GapClust  = NbClust(data,distance='euclidean', min.nc=2, max.nc =10, method = 'kmeans', index='gap')
CHClust = NbClust(data, distance='euclidean', min.nc=2, max.nc =10, method = 'kmeans', index='ch')

par(mfrow=c(1,3))
plot(2:10, silClust$All.index, type='o', col='blue', xlab='Numberof Clusters k', ylab='Silhouette Width') 
grid()
plot(2:10, GapClust$All.index, type='o', col='blue', xlab='Numberof Clusters k', ylab='Gap statistics Width') 
grid()
plot(2:10, CHClust$All.index, type='o', col='blue', xlab='Numberof Clusters k', ylab='Calinski Harabasz Width')
grid()


# lets do the clustering of our data
kmeansmdl = kmeans(data, centers = 5, nstart = 25)
initdata$cluster = factor(kmeansmdl$cluster)
str(initdata)

#lets look at the data

initdata %>%
  group_by(cluster)%>%
  summarise_all(c(Avg=mean,std=sd))


p1= ggplot(initdata, aes(x=cluster, y=Age, fill=cluster))+
  geom_boxplot()+
  ggtitle("Age by cluster")

p2= ggplot(initdata, aes(x=cluster, y=Spending, fill=cluster))+
  geom_boxplot()+
  ggtitle("Spending by cluster")


p1/p2




