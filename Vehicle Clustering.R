# Amber Garner
# Nov. 14, 2016
# Clustering on vehicle.csv


# install necessary packages
install.packages("cluster")
library(cluster)
install.packages("fpc")
library(fpc)
# Load credit vehicle dataset
veh <- read.csv("vehicle.csv", header = T, sep = ",")

# View dataset
head(veh)
summary(veh)
str(veh)

# create new variable
myvehicle <- veh

# reproduce same output
set.seed(25)

# remove Class
myvehicle$Class <- NULL

# verify removal
head(myvehicle)

# scale data
myvehicle <- scale(myvehicle)

# how many of each class
summary(veh$Class)

# build cluster model with 4 clusters
kc <- kmeans(myvehicle, 4)

# print results
kc
# number observations in each cluster
kc$size

# between sum of squares
kc$betweenss

# within sum of squares
kc$tot.withinss

# number iterations
kc$iter

# cluster centers
kc$centers

# total sum of squared distances
kc$totss

# check accuracy
table(veh$Class, kc$cluster)

# how many correctly clustered 
# highest #'s from each row divided by total numbers
((65+110+104+88)/nrow(myvehicle))*100

# plot clustering
clusplot(myvehicle, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myvehicle, kc$cluster)

# reproduce same output
set.seed(26)

# build cluster model with 5 clusters
kc5 <- kmeans(myvehicle, 5)

# number observations in each cluster
kc5$size

# between sum of squares
kc5$betweenss

# within sum of squares
kc5$tot.withinss

# number iterations
kc5$iter

# plot clustering
clusplot(myvehicle, kc5$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myvehicle, kc5$cluster)

# reproduce same output
set.seed(27)

# build cluster model with 6 clusters
kc6 <- kmeans(myvehicle, 6)

# number observations in each cluster
kc6$size

# between sum of squares
kc6$betweenss

# within sum of squares
kc6$tot.withinss

# number iterations
kc6$iter

# plot clustering
clusplot(myvehicle, kc6$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myvehicle, kc6$cluster)

# reproduce same output
set.seed(28)

# build cluster model with 7 clusters
kc7 <- kmeans(myvehicle, 7)

# number observations in each cluster
kc7$size

# between sum of squares
kc7$betweenss

# within sum of squares
kc7$tot.withinss

# number iterations
kc7$iter

# plot clustering
clusplot(myvehicle, kc7$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myvehicle, kc7$cluster)

# reproduce same output
set.seed(29)

# build cluster model with 3 clusters
kc3 <- kmeans(myvehicle, 3)

# number observations in each cluster
kc3$size

# between sum of squares
kc3$betweenss

# within sum of squares
kc3$tot.withinss

# number iterations
kc3$iter

# plot clustering
clusplot(myvehicle, kc3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myvehicle, kc3$cluster)

# reproduce same output
set.seed(30)

# build cluster model with 8 clusters
kc8 <- kmeans(myvehicle, 8)

# number observations in each cluster
kc8$size

# between sum of squares
kc8$betweenss

# within sum of squares
kc8$tot.withinss

# number iterations
kc8$iter

# plot clustering
clusplot(myvehicle, kc8$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myvehicle, kc8$cluster)

# reproduce same output
set.seed(31)

# build cluster model with 8 clusters
kc2 <- kmeans(myvehicle, 2)

# number observations in each cluster
kc2$size

# between sum of squares
kc2$betweenss

# within sum of squares
kc2$tot.withinss

# number iterations
kc2$iter

# plot clustering
clusplot(myvehicle, kc2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# more simple plot
plotcluster(myvehicle, kc2$cluster)

# find ideal number of clusters using within sum of squares plot
kideal <- (nrow(myvehicle)-1)*sum(apply(myvehicle,2,var))
for (i in 2:15) kideal[i] <- sum(kmeans(myvehicle,centers=i)$withinss)
plot(1:15, kideal, type="b", col="red",xlab="Number of Clusters",ylab="Within groups sum of squares")
