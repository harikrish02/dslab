install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
library("dplyr")
library("ggplot2")
library("ggfortify")
summary(iris)
head(iris)
data<-select(iris,c(1:4))
# We need only the datapoints without the output variables for clustering as the output will be decidid by the ethod

wssplot<- function(data,nc=15,seed=1234) {
  wss<-(nrow(data)-1)*sum(apply(data,2,var)) # apply() is used to apply var(varriance) function on data, 2 means column based 1 means row based
  for (i in 2:nc) {
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type="b",xlab="no of clusters",ylab="within group sum squares")
  wss
}
wssplot(data)
kmean<-kmeans(data,3)
kmean$centers
autoplot(kmean,data,frame=TRUE)
