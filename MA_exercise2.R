#empty the global enviroment
rm(list=ls())

#clean the plots of the global enviroment

#ctrl + l to clean the console

install.packages("tidyr")
install.packages("igraph")
install.packages("igraphdata")
library(tidyr)
library(igraph)
library(igraphdata)
library(ggplot2)

#First example(3 nodes: yahoo, amazon, Microsoft)
#defining our adjancence matrix

matrix_int<-matrix(
  c(1,1,0,
    1,0,1,
    0,1,0),
  nrow=3, ncol=3, byrow=TRUE)

#lets construct our graph
graph_int<-graph.adjacency(matrix_int, mode = "directed")
V(graph_int)$label=c("Y", "A", "M")
tkplot(graph_int, vertex.color="green")

dIn=degree(graph_int, mode="in")
dOut=degree(graph_int, mode="out")

stackeddata = rbind(dIn,dOut)
barplot(stackeddata, xlab="Node", ylab="Node Degree",names.arg=V(graph_int)$label,
        beside=TRUE, legend=c("In-degree", "Out-degree"))

#Lets set up our PageRank
Mmat = t(matrix_int)/matrix(dOut,nrow = 3, ncol=3, byrow=TRUE)

r = matrix(c(1/3, 1/3, 1/3), nrow=3, ncol=1)

rall=r # save all r values over time
rchange=1 # intial value for the change

n=1
while(rchange>0.01){
  r=Mmat%*% r # new probabilities
  rall = cbind(rall, r) # save the new r
  rchange = max(abs(rall[,n]-r)) # update differences bewteen the old and new r values
  n= n+1 # update our loop counter
}

#lets get the data in the right shape for plotting
rall=data.frame(rall)
colnames(rall)=1:dim(rall)[2]
rall$webpage=c("Y","A","M")

rall = pivot_longer(rall, cols=1:n, names_to="Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall,aes(x=Iteration, y=Prob, fill=webpage))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Example 1: simple Network")


#######################################
#second example: Dead end
#######################################

matrix_int<-matrix(
  c(1,1,0,
    1,0,1,
    0,0,0),
  nrow=3, ncol=3, byrow=TRUE)

#lets construct our graph
graph_int<-graph.adjacency(matrix_int, mode = "directed")
V(graph_int)$label=c("Y", "A", "M")
tkplot(graph_int, vertex.color="green")

dIn=degree(graph_int, mode="in")
dOut=degree(graph_int, mode="out")

stackeddata = rbind(dIn,dOut)
barplot(stackeddata, xlab="Node", ylab="Node Degree",names.arg=V(graph_int)$label,
        beside=TRUE, legend=c("In-degree", "Out-degree"))

#Lets set up our PageRank
Mmat = t(matrix_int)/matrix(dOut,nrow = 3, ncol=3, byrow=TRUE)
Mmat[matrix(dOut,nrow=3,ncol=3, byrow = TRUE)==0]=0
r = matrix(c(1/3, 1/3, 1/3), nrow=3, ncol=1)

rall=r # save all r values over time
rchange=1 # intial value for the change

n=1
while(rchange>0.001){
  r=Mmat%*% r # new probabilities
  rall = cbind(rall, r) # save the new r
  rchange = max(abs(rall[,n]-r)) # update differences bewteen the old and new r values
  n= n+1 # update our loop counter
}

#lets get the data in the right shape for plotting
rall=data.frame(rall)
colnames(rall)=1:dim(rall)[2]
rall$webpage=c("Y","A","M")

rall = pivot_longer(rall, cols=1:n, names_to="Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall,aes(x=Iteration, y=Prob, fill=webpage))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Example 2: dead end")

##########################################################
# example three: 
##############################################
matrix_int<-matrix(
  c(1,1,0,
    1,0,1,
    0,0,1),
  nrow=3, ncol=3, byrow=TRUE)

#lets construct our graph
graph_int<-graph.adjacency(matrix_int, mode = "directed")
V(graph_int)$label=c("Y", "A", "M")
tkplot(graph_int, vertex.color="green")


dIn=degree(graph_int, mode="in")
dOut=degree(graph_int, mode="out")

stackeddata = rbind(dIn,dOut)
barplot(stackeddata, xlab="Node", ylab="Node Degree",names.arg=V(graph_int)$label,
        beside=TRUE, legend=c("In-degree", "Out-degree"))

#Lets set up our PageRank
Mmat = t(matrix_int)/matrix(dOut,nrow = 3, ncol=3, byrow=TRUE)
Mmat[matrix(dOut,nrow=3,ncol=3, byrow = TRUE)==0]=0
r = matrix(c(1/3, 1/3, 1/3), nrow=3, ncol=1)

rall=r # save all r values over time
rchange=1 # intial value for the change

n=1
while(rchange>0.001){
  r=Mmat%*% r # new probabilities
  rall = cbind(rall, r) # save the new r
  rchange = max(abs(rall[,n]-r)) # update differences bewteen the old and new r values
  n= n+1 # update our loop counter
}

#lets get the data in the right shape for plotting
rall=data.frame(rall)
colnames(rall)=1:dim(rall)[2]
rall$webpage=c("Y","A","M")

rall = pivot_longer(rall, cols=1:n, names_to="Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall,aes(x=Iteration, y=Prob, fill=webpage))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Example 3: Spider trap")


######################################################
## Teleportation #####################################
######################################################

beta =0.8
Amat= beta * Mmat + (1-beta)* matrix(1/3, nrow = 3, ncol = 3)

r = matrix(c(1/3, 1/3, 1/3), nrow=3, ncol=1)

rall=r # save all r values over time
rchange=1 # intial value for the change

n=1
while(rchange>0.001){
  r=Amat%*% r # new probabilities
  rall = cbind(rall, r) # save the new r
  rchange = max(abs(rall[,n]-r)) # update differences bewteen the old and new r values
  n= n+1 # update our loop counter
}

#lets get the data in the right shape for plotting
rall=data.frame(rall)
colnames(rall)=1:dim(rall)[2]
rall$webpage=c("Y","A","M")

rall = pivot_longer(rall, cols=1:n, names_to="Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall,aes(x=Iteration, y=Prob, fill=webpage))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Example 4: Teleportation")


###################################################
### Topic specific Page Rank ######################
###################################################

Teleportset = c(1,1,0)
Amat=beta*Mmat+ (1-beta)*matrix(Teleportset/sum(Teleportset), ncol=3,
nrow=3, byrow=FALSE)

r = matrix(c(1/3, 1/3, 1/3), nrow=3, ncol=1)

rall=r # save all r values over time
rchange=1 # intial value for the change

n=1
while(rchange>0.001){
  r=Amat%*% r # new probabilities
  rall = cbind(rall, r) # save the new r
  rchange = max(abs(rall[,n]-r)) # update differences bewteen the old and new r values
  n= n+1 # update our loop counter
}

#lets get the data in the right shape for plotting
rall=data.frame(rall)
colnames(rall)=1:dim(rall)[2]
rall$webpage=c("Y","A","M")

rall = pivot_longer(rall, cols=1:n, names_to="Iteration", values_to = "Prob")
rall$Iteration = factor(rall$Iteration, levels = 1:n)

ggplot(rall,aes(x=Iteration, y=Prob, fill=webpage))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Example 5: Topic-sensitive PageRank")
























