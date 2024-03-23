getwd()
setwd("C:/Users/hridi/OneDrive/Desktop/Fall 2023/DSBA 6276 Strategic B.Ay_ Dr.Du/Assignment 02")

# read creditcard data
creditcard = read.csv("hw2_data_credit_card.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(creditcard)

# sanity check of the imported data
str(creditcard)

# data preparation
#check if there is missing values in the data - this shows that there are no missing values
sum(is.na(creditcard))


####### clustering #######

#exclude variables id, Age, line_limit
ex_creditcard = creditcard[,2:13] 
# standardize variables - scale gives us the standardization
s_creditcard = scale(ex_creditcard)


### k-means clustering with k=2 
creditcard_k2 = kmeans(s_creditcard, centers = 2, nstart = 20)
# plots the clusters
fviz_cluster(creditcard_k2, data = s_creditcard) + theme_classic() + ggtitle("Two Clusters")
# save the plot
ggsave("two_clusters.jpeg", scale = 3)
# Create subsets of the data for each cluster (k=2)
creditcard_k2_cluster1 = creditcard[creditcard_k2$cluster == 1, ]
creditcard_k2_cluster2 = creditcard[creditcard_k2$cluster == 2, ]

summary(creditcard_k2_cluster1)
summary(creditcard_k2_cluster2)
                            ### Altreative ###
#Calculate mean and median for age for Cluster 1
mean_age_creditcard_k2_cluster1 <- mean(creditcard_k2_cluster1$age)
median_age_creditcard_k2_cluster1  <- median(creditcard_k2_cluster1$age)

# Calculate mean and median for line_limit for Cluster 2
mean_line_limit_creditcard_k2_cluster1 <- mean(creditcard_k2_cluster1$line_limit)
mean_line_limit_creditcard_k2_cluster1 
median_line_limit_creditcard_k2_cluster1<-  median(creditcard_k2_cluster1$line_limit)
median_line_limit_creditcard_k2_cluster1



### k-means clustering with k=3 
creditcard_k3 = kmeans(s_creditcard, centers = 3, nstart = 20)
# plots the clusters
fviz_cluster(creditcard_k3, data = s_creditcard) + theme_classic() + ggtitle("Three Clusters")
# save the plot
ggsave("three_clusters.jpeg", scale = 3)

# Create subsets of the data for each cluster (k=3)
creditcard_k3_cluster1 = creditcard[creditcard_k3$cluster == 1, ]
creditcard_k3_cluster2 = creditcard[creditcard_k3$cluster == 2, ]
creditcard_k3_cluster3 = creditcard[creditcard_k3$cluster == 3, ]

summary(creditcard_k3_cluster1)
summary(creditcard_k3_cluster2)
summary(creditcard_k3_cluster3)



##### Hierarchical Clustering #####
# calculate dissimilarity matrix using the cleaned, standardized data
dis_matrix = dist(s_creditcard, method = "euclidian")

# hierarchical clustering using ward linkage
hc_ward = hclust(dis_matrix, method = "ward.D2")

# plot the obtained dendrogram
plot(hc_ward)
plot(hc_ward, hang = -1)              # labels at the same level
plot(hc_ward, hang = -1, cex = 0.5)   # scale the plotting text and symbols smaller

# save the graph from plot() 
jpeg(file = "dendrogram.jpeg", width = 1000, height = 1000 ) 
plot(hc_ward, hang = -1, cex = 0.5, main = "Dendrogram ward.D2 Method") 
dev.off()