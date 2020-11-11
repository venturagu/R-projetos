library(tidyr)
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)
library(stringr)
library(class)
library(rpart)
library(rpart.plot)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)


data <- read.csv2("wdbc.data", sep =",", na.strings = c('','NA','na','N/A','n/a','NaN','nan'), header = FALSE,  dec=".", stringsAsFactors=FALSE)

# Renomeando as colunas com base no indicado no link do dataset https://www.kaggle.com/uciml/breast-cancer-wisconsin-data
cancers <- data %>%
  rename(
    id = V1,
    diagnosis = V2, # Significado: Câncer de mama M -Maligno, B - Beligno
    radius_mean = V3, # Significado: Raio medio do câncer
    texture_mean = V4, # Significado: Média de escala de cinza do câncer
    perimeter_mean = V5, # Significado: Media de perimetro do câncer
    area_mean = V6, # Significado: Média de area do câncer 
    smoothness_mean = V7,
    compactness_mean = V8,
    concavity_mean = V9,
    concave_points = V10,
    symmetry_mean = V11,
    fractal_dimension = V12,
    radius_se = V13,
    texture_se = V14,
    perimeter_se = V15,
    area_se = V16,
    smoothness_se = V17,
    compactness_se = V18,
    concavity_se = V19,
    concave_points_se = V20,
    symmetry_se = V21,
    fractal_dimension_se = V22,
    radius_worst = V23,
    texture_worst = V24,
    perimeter_worst = V25,
    area_worst = V26,
    smoothness_worst = V27,
    compactness_worst = V28,
    concavity_worst = V29,
    concave_points_worst = V30,
    symmetry_worst = V31,
    fractal_dimension_worst = V32
  )

# Entendimento previo do dataset
dim(cancers)
sum(str_count(cancers$diagnosis, "M")) # 212 Maligno
sum(str_count(cancers$diagnosis, "B")) # 357 Benigno

#Ordenando o dataset para aplicar o Kmeans
cancers <- cancers[order(cancers$diagnosis),]

data <- cancers[, -1] #Retiro id
data <- data[, -1] #Retiro classificação
classes <-cancers[, 2]

cl <- kmeans(data, 2)

#Comparar obtido com real do dataset


classes <-replace(classes, classes == "B", 1) #Atribuindo maligno com 1
classes <-replace(classes, classes == "M", 2) # Atribuindo benigno com 2
classes <- as.numeric(classes)

cl$cluster

compar <- classes == cl$cluster
taxaAcerto <- (table(compar)[names(table(compar)) == TRUE] * 100) / length(compar) #Contando a taxa de acerto: 14,5
taxaAcerto

#Algoritmo do cotovelo
#Resumo: Achar graficamente um numero otimo de K's (agrupamentos)
k.max<- 2
wss <- sapply(2:k.max, function(k){kmeans(data, k, nstart=2 )$tot.withinss})
print(wss)
#plot(2:k.max, wss, type="b", pch = 19,  xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
fviz_nbclust(data, kmeans, method = "wss") 

##Codigo da dani
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
