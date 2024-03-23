# empty the environment
rm(list=ls())                                  
#set your directory
setwd("C:/Users/hridi/OneDrive/Desktop/Fall 2023/DSBA 6276 Strategic B.Ay_ Dr.Du/Assignment 02")

# read wine data
wine = read.csv("hw2_data_wine.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(wine)
# sanity check of the imported data
head(wine)
str(wine)
# data preparation
#check if there is missing values in the data - this shows that there are no missing values
sum(is.na(wine))


# check variable types(All numeric)
str(wine)
# standardize variables - scale gives us the standardization
s_wine = scale(wine)

####### clustering ####### 


# apply k-means algorithm
wine2 = kmeans(s_wine, centers = 2)                               # specify number of clusters, k= 2
wine2 = kmeans(s_wine, centers = 2, nstart = 20)                  # multiple (20) initial configurations and report on the best one.
wine2                                                             # look at the outcomes
wine2$cluster                                                     # look at each point's cluster assignment
wine2$size                                                        # the number of points in each cluster
wine2$tot.withinss                                                # total within-cluster sum of squares
sum(wine2$withinss)                                               # the sum of all the within-cluster sum of squares = total within-cluster sum of squares

# visualize the clusters
fviz_cluster(wine2, data = s_wine) + theme_classic() + ggtitle("Two Clusters")
# save the plot
ggsave("two_clusters.jpeg", scale = 3)

                  
wine3 = kmeans(s_wine, centers = 3)                               # specify 3 clusters 
fviz_cluster(wine3, data = s_wine) + theme_classic() + ggtitle("Three Clusters")
ggsave("three_clusters.jpeg", scale = 3)


wine4 = kmeans(s_wine, centers = 4)                               # specify 4 clusters 
fviz_cluster(wine4, data = s_wine) + theme_classic() + ggtitle("Four Clusters")
ggsave("four_clusters.jpeg", scale = 3)


# decide the optimal number of clusters
fviz_nbclust(s_wine, kmeans, method = "wss")                     # look at the plot to determine the optimal number of clusters using elbow method
ggsave("elbow_method.jpeg", scale = 3)

fviz_nbclust(s_wine, kmeans, method = "silhouette")              # find the clusters such that maximize the avg silhouette width(Another method)
ggsave("silhouette_method.jpeg", scale = 3)




#finalized our clustering with the results segment the data according to finalized clusters ## want to use 20 - 25
wine_optimal = kmeans(s_wine, centers = 3, nstart = 20)
fviz_cluster(wine_optimal, data = s_wine) + theme_classic() + ggtitle("optimal Clusters")
ggsave("optimal_clusters.jpeg", scale = 3)

# the number of points in each cluster
wine_optimal$size 
# total within-cluster sum of squares
wine_optimal$tot.withinss
# the sum of all the within-cluster sum of squares
sum(wine_optimal$withinss) 

##segment with our original data - this allows us to add back in the demographic variables we removed
winedata1 = wine[wine_optimal$cluster == 1, ]
winedata2 = wine[wine_optimal$cluster == 2, ]
winedata3 = wine[wine_optimal$cluster == 3, ]
summary(winedata1)
summary(winedata2)
summary(winedata2)


