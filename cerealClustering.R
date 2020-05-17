#Libraries Used
install.packages("factoextra")
install.packages('corrplot')
install.packages("DMwR")
install.packages("fpc")
install.packages("clusterSim")
library(factoextra)
library(DMwR)
library(fpc)
library(clusterSim)
library(corrplot)
library(cluster) 
library(factoextra)

#Exploratory Data Analysis
str(cereal)
summary(cereal)
head(cereal)
tail(cereal)

#Null Value Analysis
sum(is.na(cereal))
cereals_data <- knnImputation(cereal, k = 3, scale = T)
sum(is.na(cereals_data))

#Data preparation(scaled/unscaled) for clustering
rownames(cereals_data) <- cereals_data$name
cereals_data$name = NULL
cereals_data$mfr = NULL
cereals_data$type = NULL
temp_cereal = cereal
temp_cereal$name = NULL
temp_cereal$mfr = NULL
temp_cereal$type = NULL
cereals_data <- scale(cereals_data, center = T, scale = T)
View(cereals_data)

#Euclidian distance calculation of each data point from each data point
distance <- get_dist(cereals_data,method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Naive Kmeans clustering application to get an idea of what is happening
set.seed(123)
km_basic <- kmeans(cereals_data, 5, nstart = 10)
str(km_basic)
fviz_cluster(km_basic, cereals_data)

#Performing Elbow analysis on scaled as well as unscaled data
set.seed(123)
fviz_nbclust(cereals_data, kmeans, method = "wss")
fviz_nbclust(temp_cereal, kmeans, method = "wss")

#Analysing and deciding which value of k is better from either 3 or 5
km_clust1 <- kmeans(cereals_data, 3, nstart = 10)
km_points1 <- km_clust1$cluster
cereals_clusts_km1 <- as.data.frame(cbind(km_clust1$cluster, cereals_data))
head(cereals_clusts_km1)
colnames(cereals_clusts_km1)[1] <- "cluster_km"
fviz_cluster(km_clust1, cereals_data)

km_clust2 <- kmeans(cereals_data, 5, nstart = 10)
km_point2 <- km_clus2$cluster
cereals_clusts_km2 <- as.data.frame(cbind(km_clust2$cluster, cereals_data))
head(cereals_clusts_km2)
colnames(cereals_clusts_km2)[1] <- "cluster_km"
fviz_cluster(km_clust2, cereals_data)

#performing Davies-Bouldin analysis
print(index.DB(cereals_data, km_clust1$cluster, centrotypes="centroids"))
print(index.DB(cereals_data, km_clust2$cluster, centrotypes="centroids"))

#performing silhouette analysis
fviz_nbclust(cereals_data, kmeans, method='silhouette')

#creating cluster profile for k=3 and k=5
cluster_summary_3 <- cbind(km_clust1$cluster,cereal)
cluster_summary_3$name = NULL
cluster_summary_3$mfr = NULL
cluster_summary_3$type = NULL
summary1 = aggregate(cluster_summary_3, by = list(km_clust1$cluster),FUN = "mean")
summary1

cluster_summary_5 <- cbind(km_clust2$cluster,cereal)
cluster_summary_5$name = NULL
cluster_summary_5$mfr = NULL
cluster_summary_5$type = NULL
summary2 = aggregate(cluster_summary_5, by = list(km_clust2$cluster),FUN = "mean")
summary2