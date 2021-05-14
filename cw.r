#getting the readxl library
library(readxl)
vehicles <- read_excel("vehicles.xlsx")
#getting the ggplot2 library
library(ggplot2)
#ploting the dataset as comp as the x axis and Circ as the y axis
ggplot(vehicles, aes(Comp, Circ)) + geom_point(aes(col=Class), size=4)
View(vehicles)
#get the summary of the dataset
summary(vehicles)
#getting the tidyverse library
library(tidyverse)
vehi_fact <- mutate(vehicles, Class = as.factor(vehicles$Class))
summary(vehi_fact)
#cleaning the dataset using janitor
library(janitor)
vehi_j <- janitor::clean_names(vehi_fact)
summary(vehi_j)
#detecting the outliers for each class 
#plotting the detection of outliers 
vehi_j %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'van'")
vehi_j %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'bus'")
vehi_j %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'saab'")
vehi_j %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'opel'")

#removing outliers from each class
library(scales)
vehi_bus = vehi_j %>%
  filter(class == "bus") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.1, .8)))))
vehi_van = vehi_j %>%
  filter(class == "van") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
vehi_opel = vehi_j %>%
  filter(class == "opel") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
vehi_saab = vehi_j %>%
  filter(class == "saab") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .9)))))
combined = bind_rows(list(vehi_bus,vehi_van,vehi_opel,vehi_saab)) %>%
  arrange(samples)

print(combined)
#plotting outliers for each class
combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: 'van'")
combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: 'opel'")
combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: 'saab'")
combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: 'bus'")

#only the numerical data is available for the algorithm to be scaled
vehicles_data_points = combined %>%
  select(-samples, -class)
vehicles_scaled = vehicles_data_points %>%
  mutate(across(everything(), scale))

view(vehicles_scaled)

#using the elbow method to detect the optimal number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(vehicles_scaled, nc=10)

#getting the NbClust library
library(NbClust)

#detecting the optimal number of clusters using euclidean distance and manhattan distance 
set.seed(123)
cluster_euclidean = NbClust(vehicles_scaled, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

cluster_manhattan = NbClust(vehicles_scaled, distance = "manhattan", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

kmeans.clus = kmeans(x=vehicles_scaled, centers = 2, nstart = 25)
kmeans.clus

kmeans.clus1 = kmeans(x=vehicles_scaled, centers = 4, nstart = 25)
kmeans.clus1

#getting the cluster centers
kmeans.clus$centers

#getting the fpc library and plotting the clusters
library(fpc)
plotcluster(vehicles_scaled, kmeans.clus$cluster)

library(cluster)
clusplot(vehicles_scaled, kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)

vehicles$Clusters <- kmeans.clus$cluster
View(vehicles)

#getting the cluster profile
aggr = aggregate(vehicles[,-c(1,2, 8)],list(vehicles$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(vehicles$Clusters)),
                            aggr[,-1])

View(clus.profile)

# confusionMatrix(
# 
#   factor(kmeans.clus$cluster, levels = 1:4),
#   factor(as.numeric(vehicles$Class), levels = 1:4)
# )

#the table of each cluster and its relevant classes 
table(vehicles$Class, kmeans.clus$cluster)




