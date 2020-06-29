library(cluster)
library(factoextra)
# Loading Wine data
data <- read.csv(file.choose()) ## use read.csv for csv files
View(data)

## removing the first column in data has Types of Wine
data <- data[,-1]
attach(data)

##correlation coefficient
cor(data)

##performing PCA
pca <- princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)###performing PCA and also normalizing the data
summary(pca)
str(pca)
loadings(pca)
plot(pca)##### By this graph we can see first 3 princpal component have higher variances  
biplot(pca)

##helps to choose the number of principal component
plot(cumsum(pca$sdev*pca$sdev)*
       100/(sum(pca$sdev*pca$sdev)),type="b")
pca$scores[,1:3]

################
#### Considering top 3 principal component scores and binding them with mydata
mydata <- cbind(data,pca$scores[,1:3])
View(mydata)

# Hierarchial Clustering
# preparing data for clustering 
clus_data <- mydata[,8:10]

# Normalizing the data 
norm_clus <- scale(clus_data) # Scale function is used to normalize data
dist <- dist(norm_clus,method = "euclidean") # method for finding the distance by using Euclidean distance

# Clustering the data using hclust function - Hierarchical
fit <- hclust(dist,method="complete") 

plot(fit) # Dendrogram
rect.hclust(fit, k=6, border="red")

####
groups <- cutree(fit,7) # Cutting the dendrogram for 7 clusters

membership_1 <- as.matrix(groups) # cluster numbering 

View(membership_1)

final <- cbind(membership_1,data) # binding column wise with orginal data
View(final)
final_1 <- aggregate(final[,-c(2,16:18)],by=list(membership_1),FUN=mean)
View(final_1) # Inferences can be

# drawn from the aggregate of the wine data on membership_1
write.csv(final,file="wine_cluster.csv",row.names = F,col.names = F)
getwd()

##### k-mean clusters
my_data <- read.csv(choose.files())
View(my_data)
str(my_data)
norm_data <- scale(my_data[,2:14])
View(norm_data)

wss = (nrow(norm_data)-1)*sum(apply(norm_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(norm_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
title(sub = "K-Means Clustering Scree-Plot")

fitting <- eclust(norm_data, "kmeans", k = 6, nstart = 25, graph = FALSE) # 6 cluster solution
fviz_cluster(fitting, geom = "point", frame.type = "norm")
