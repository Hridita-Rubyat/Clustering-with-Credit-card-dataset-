
# empty the environment
rm(list=ls())                                  

#setwd("your computer/your folder")           
#Alternatively, use drop-down menu: "Session-Set Working Directory-Choose Directory" to choose the folder you want to work in
getwd()
setwd("C:/Users/hridi/OneDrive/Desktop/Fall 2023/DSBA 6276 Strategic B.Ay_ Dr.Du/Assignment 02")

##### Clustering Analysis #####
# install packages needed for cluster analysis, only need to install for the first time
install.packages("cluster")      
install.packages("factoextra")

# load the packages
library(ggplot2)                
library(cluster)                 
library(factoextra)

# read wine data
wine = read.csv("hw2_data_wine.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(wine)

# sanity check of the imported data
head(wine)
str(wine)

# data preparation
#check if there is missing values in the data - this shows that there are no missing values
sum(is.na(wine))

# if there are missing value
# if want to remove those missing values - this creates a variable for the omitted values. In our case there are none.
wine_noNA = na.omit(wine)

                                 # if want to impute the missing values with mean for a certain variable - gives us the mean value for our variable for malic
                                 malic_mean = mean(wine$malic)

                                ## then we use index to fill in the missing value. ## No comma after the condition in the index, ONLY indexing BRIAN and not the entire DATA FRAME
                                 wine$malic[is.na(wine$malic) == TRUE] = malic_mean

# check the class of a variable
class(wine$hue)

                                # check levels of a factor variable
                             /      n 

                                # remove object - if our environment has too many objects, we can remove some of them - rm is short for remove
                                rm(wine_noNA) ## this removes the object from the environment

# look at number of observations in the data set
nrow(wine) ## 178 rows

                                # get number of levels for factor variable ## this shows 7 different levels of education
                                level_edu = unique(comedy$Edu)
                                length(level_edu)

                                # plot a boxplot for viewer's liking score of Russell by sex *****sex variable is Sex - case sensitive
                                ggplot(data = wine) + geom_boxplot(mapping = aes(x = alcohol, y = hue)) + theme_bw()

                                # plot a boxplot for viewer's liking score of Russell by sex and get each sub-group by education level
                                ##facet_wrap - the only thing that is different in this case
                                ggplot(data = comedy) + geom_boxplot(mapping = aes(x = Sex, y = Russell)) + facet_wrap(~Edu) + theme_bw()

                                # save the plot
                                ggsave("boxplot_Russell.jpeg", scale = 3)

####### clustering 
                                # only include variables that can apply k-means clustering
                                # exclude variables Sex, Age, Race, Edu - this is just the last 4 columns we want to exclude. We only want 1-19
                                km_comedy = comedy[,1:19] ## has the same number of obs but only 19 variables. 
                                km_comedy

                                # convert survey agree/disagree variables into numeric for k-means analysis
                                ## these categorical variables can be converted to numerical because they are equally spaced
                                km_comedy = data.frame(sapply(km_comedy, unclass))

                                # check variable types ## everything is numerical now.
                                str(wine)

# standardize variables - scale gives us the standardization
s_wine = scale(wine)

# apply k-means algorithm
# specify number of clusters = 2 - the result is a list of 9 elements and a large object
wine2 = kmeans(s_wine, centers = 2)

# multiple (20) initial configurations and report on the best one.
##randomly start with the centroid - tell R to do multiple starting points and pick the best one.
wine2 = kmeans(s_wine, centers = 2, nstart = 20)
# look at the outcomes
wine2
                            # look at each point's cluster assignment
                            ## we have 2 clusters
                            wine2$cluster ## this gives us 1,2,1,2 responses


                            # the number of points in each cluster
                            wine2$size ## - 1st cluster 123, and second cluster has 79, order of cluster doesn't matter

# total within-cluster sum of squares
wine2$tot.withinss

                            # the sum of all the within-cluster sum of squares = total within-cluster sum of squares
                            sum(wine2$withinss) ## these last two lines give you the same output

# visualize the clusters - plots the clusters - the underlying plot function is ggplot as well. 
##You can add layers to this plot: + theme_bw() & ggtitle("Two Clusters") are layers we have added to the original fviz_cluster() function
fviz_cluster(wine2, data = s_wine) + theme_bw() + ggtitle("Two Clusters")

                            # save the plot
                            ggsave("two_clusters.jpeg", scale = 3)

# specify 3 clusters 
wine3 = kmeans(s_wine, centers = 3)
fviz_cluster(wine3, data = s_wine) + theme_bw() + ggtitle("Three Clusters")
ggsave("three_clusters.jpeg", scale = 3)

# specify 4 clusters 
wine4 = kmeans(s_wine, centers = 4)
fviz_cluster(wine4, data = s_wine) + theme_bw() + ggtitle("Four Clusters")
ggsave("three_clusters.jpeg", scale = 3)

# decide the optimal number of clusters
# look at the plot to determine the optimal number of clusters using the elbow method (within sum of squares) = wss
fviz_nbclust(s_wine, kmeans, method = "wss")
ggsave("elbow_method.jpeg", scale = 3)

# find the #clusters such that maximize the avg silhouette width 
## only difference here is the method
fviz_nbclust(s_wine, kmeans, method = "silhouette")
ggsave("silhouette_method.jpeg", scale = 3) ##now we can finalize our clustering with the results 

# segment the data according to finalized clusters ## want to use 20 - 25
wine_optimal = kmeans(s_wine, centers = 3, nstart = 20)
                         ##segment with our original data - this allows us to add back in the demographic variables we removed
                         winedata1 = wine[wine_optimal$cluster == 1, ]
                         winedata2 = wine[wine_optimal$cluster == 2, ]
                         winedata3 = wine[wine_optimal$cluster == 3, ]
summary(winedata1)
# look at each point's cluster assignment
## we have 3 clusters
wine_optimal $cluster ## this gives us 3,3,1,2 responses
# the number of points in each cluster
wine_optimal$size
#  plots the clusters
fviz_cluster(wine_optimal, data = s_wine) + theme_bw() + ggtitle("Three Clusters")
# save the plot
ggsave("three_clusters.jpeg", scale = 3)





getwd()
setwd("C:/Users/hridi/OneDrive/Desktop/Fall 2023/DSBA 6276 Strategic B.Ay_ Dr.Du/Assignment 02")
# read wine data
creditcard = read.csv("hw2_data_credit_card.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(creditcard)
sum(is.na(creditcard))
str(creditcard)



##### Hierarchical Clustering #####
# calculate dissimilarity matrix using the cleaned, standardized data
dis_matrix = dist(creditcard, method = "euclidian")

# hierarchical clustering using complete linkage -- this gives us a list of 7 elements in the environment
hc_complete = hclust(dis_matrix, method = "complete")

# plot the obtained dendrogram -- this plots all 202 obs 
plot(hc_complete)

## to make it a little prettier, we add some code
plot(hc_complete, hang = -1) ## this moves all the labels to the bottom so they don't overlap with the lines
plot(hc_complete, hang = -1, cex = 0.5) ## makes the labels smaller

# save the graph from plot() ## not using ggplot so we use a different save function. Open device, plot, and close the device
jpeg(file = "dendrogram.jpeg", width = 1000, height = 1000 ) ##default is  ## opens the device
plot(hc_complete, hang = -1, cex = 0.5, main = "Dendrogram Complete Method") ## plots the graph
dev.off() ## closes the device

########### THIS IS WHERE WE LEFT OFF IN CLASS. Will finish in next class ################

# another function to do hierarchical clustering


# then we can compare across different methods using agnes$ac
# look at the agglomerative coefficients, which method identifies the strongest clustering structure?


# working with dendrograms: the height of the cut to dendrogram controls the number of clusters obtained

# cut tree into several groups
# cut tree into 3 clusters

# cut tree into 4 clusters


# visualize and save the results

# cut agnes() tree into 3 groups


# determine optimal #clusters

# the optimal hc cluster



