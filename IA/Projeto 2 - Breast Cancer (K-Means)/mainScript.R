library(tidyr)
library(dplyr)
library(ggplot2)
library(dslabs)
library(rpart.plot)
library(tidyverse)  # data manipulation
# install.packages("tidyverse")
library(cluster)    # clustering algorithms
library(factoextra)
library(FactoMineR)

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


# Ordenando o dataset para aplicar o Kmeans
cancers <- cancers[order(cancers$diagnosis),] # ordena primeiro benigno depois maligno

data <- cancers[, -1] # Retiro id
data <- data[, -1] # Retiro classificação
View(data)
# dataset com muitas features, aplicar escala
data_scale<-as.data.frame(scale(data))

classes <-cancers[, 2]

# Comparar obtido com real do dataset tranformando "B" e "M" em dados numérico
classes <-replace(classes, classes == "B", 1) # Atribuindo benigno com 1
classes <-replace(classes, classes == "M", 2) # Atribuindo maligno com 2
classes <- as.numeric(classes)

# plot para visualizar distribuição das classes considerando primieira features do dataset
plot(cancers[, 2:5], col = classes)

cl1 <- kmeans(data_scale, 2)
cl2 <- kmeans(data, 2)

cl$cluster
classes

compar1 <- classes == cl1$cluster
compar2 <- classes == cl2$cluster

taxaAcerto1 <- (table(compar1)[names(table(compar1)) == TRUE] * 100) / length(compar1) # Contando a taxa de acerto: 14,5
taxaAcerto1

taxaAcerto2 <- (table(compar2)[names(table(compar2)) == TRUE] * 100) / length(compar2) # Contando a taxa de acerto: 14,5
taxaAcerto2

fviz_cluster(cl1, data = data_scale, geom = "point", main = "Gráfico de cluster normalizado")
fviz_cluster(cl2, data = data, geom = "point", main = "Gráfico de cluster não normalizado")

# ------------------------ Algoritmo do cotovelo -------------------------------

# função para calcular a soma total do quadrado dentro do cluster
# total within-cluster (tot.withinss)
wss <- function(k) {kmeans(data_scale, k)$tot.withinss}

# Calcule e plote wss para k = 1 a k = 10
k.values <- 1:10

# extrair wss para 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Número K de clusters",
     ylab="Soma total dos quadrados dentro dos clusters")

# Metodo cotovelo já implementado por fviz_nbclust
n_clust <- fviz_nbclust(data_scale, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

n_clust

n_clust_list <- n_clust$data
n_clust_list


# plot para visualizar distribuição das classes de forma geral
plot(cancers[, 2:4])
