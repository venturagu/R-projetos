library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)
library(stringr)
library(class)

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

# Verificar se existem dados vazios
sum(is.na(cancers)) # Nenhum valor de atributo ausente

# Entendimento previo do dataset
dim(cancers)
str(train)
sum(str_count(cancers$diagnosis, "M")) # 212 Maligno
sum(str_count(cancers$diagnosis, "B")) # 357 Benigno

# Remoção da feature id para não ser considerado em plots e em algoritmos de IA
cancers=subset(cancers,select = -id)

# Dataset de treino 75%  da base original e teste 25% base original.
set.seed(123)
smp_size <- floor(0.75 * nrow(cancers))
train_ind <- sample(seq_len(nrow(cancers)), size = smp_size)

train <- cancers[train_ind, ]
test <- cancers[-train_ind, ]

# ---------- Gráficos para verificar se visualmente os dados são classificaveis---------------------------------------------

# Gráfico de pizza - quantidade de cancer diagnosticado como maligno ou benigno
diagnostico_frequencia <-table(train$diagnosis)
diagnostico_porcentagem <- round(prop.table(diagnostico_frequencia)*100) # "% of total sum of row of column"
diagnostico_porcentagem # B -> 65% e M -> 35%
diagnostico_tabela <-as.data.frame(diagnostico_porcentagem)
colnames(diagnostico_tabela)[1] <- "Diagnostico"

bp<- ggplot(diagnostico_tabela, aes(x="", y= Freq, fill=Diagnostico))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0) +
  labs(x=NULL,y=NULL,title="Diagnostico B- Benigno / M- Maligno") 
pie

# plots de teste:
hist(train$radius_mean)

ggplot(data=train, aes(x=radius_mean, y=texture_mean, group=diagnosis, color=diagnosis)) +
  geom_bar(stat='identity')

ggplot(train, aes(x=texture_mean, y=area_mean)) + geom_point()

# id = 1:nrow(train)
# id_name = "id"
# train <- cbind(id = id, train)

# Gráfico histograma - frequencia dos dados de cada feature filtrado para maligno e outro benigno
train_M <- filter(train, diagnosis == "M")
train_B <- filter(train, diagnosis == "B")

train_M = subset(train_M,select = -diagnosis)
train_B = subset(train_B,select = -diagnosis)

ggplot(gather(train_M), aes(x = value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  labs( x = "Dados", y = "Frequencia") + 
  ggtitle("Visualização geral dos dados referente a cancer tipo maligno classifcado por feature")

ggplot(gather(train_B), aes(x = value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') + 
  labs( x = "Dados", y = "Frequencia") + 
  ggtitle("Visualização geral ddos dados referente a cancer tipo benigno classifcado por feature")

# ----------------------- Algoritmo KNN com k = 1,3,5 e 11 vizinhos ------------------------------

# train[,1] -> coluna diagnosis

verificaknn <- function(datasetTrain, datasetTest, vetorK, posicaoClassificador){
  classesTrain <- datasetTrain[ ,posicaoClassificador]
  datasetTrain <- datasetTrain[ , -posicaoClassificador]
  
  classesTest <- datasetTest[ , posicaoClassificador]
  datasetTest <- datasetTest[ , -posicaoClassificador]
  
  result <- knn(datasetTrain, datasetTest, classesTrain, vetorK)
  
  # Matriz de confusão
  print(table(classesTest, result))
  
  # Calculo do índice de acerto de k
  print(round(length(result) / length(datasetTest), digits = 2))
}

# k = 1
verificaknn(train, test, 1, 1)
# k = 3
verificaknn(train, test, 3, 1)
# k = 5
verificaknn(train, test, 5, 1)
# k = 11
verificaknn(train, test, 11, 1)