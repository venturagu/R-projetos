# library(tidyr)
# library(dplyr)
# library(ggplot2)
# library(dslabs)
# library(rpart.plot)
# library(tidyverse)  # data manipulation
# # install.packages("tidyverse")
# library(cluster)    # clustering algorithms
# library(factoextra)
# library(FactoMineR)
library(stringr)

data <- read.delim("cardiovascular.txt", sep =";", na.strings = c('','NA','na','N/A','n/a','NaN','nan'))

# Entendimento previo do dataset
View(head(data, n=5))
dim(data) # 462 linhas e 11 colunas (sendo uma coluna identificador)
str(data)

# Verificar dados vazios em cada coluna e contabilidar
View(sapply(data, function(x) sum(is.na(x))))
sum(is.na(data))

# retirar coluna de identificação
data <- data[-1]

# Dividir em conjunto de treino e teste (80% 20%)
set.seed(123)
smp_size <- floor(0.80 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

dim(train)
dim(test)

# preparando chd para colorir plot
chd <- as.factor(data$chd)

famhist <-data$famhist

# tranformando "Present" e "Absent" em dados númerico
data$famhist <- replace(data$famhist, data$famhist == "Present", "1") # Atribuindo Present com 1
data$famhist <- replace(data$famhist, data$famhist == "Absent", "2") # Atribuindo Absent com 2
data$famhist <- as.numeric(data$famhist)

# ----------------- Exploração inicial dos dados ------------------------------
# Plot geral das features
plot(data, col = chd)

# É possivél ver um excesso de gordura no organismo ao longo da idade, e também o aumento de numero 
# de paciantes com resultado de doença coronaria. 
plot(x = data$age, y = data$adiposity, col = chd, pch =19, main="Adiposidade x Idade",
     ylab="Adiposidade", xlab="Idade")

# 192 com historico familiar presente
sum(str_count(data$famhist, "Present"))
# 270 com historico familiar ausente
sum(str_count(data$famhist, "Absent"))
