library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)

data <- read.csv2("wdbc.data", sep =",", na.strings = c('','NA','na','N/A','n/a','NaN','nan'), header = FALSE)

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


# Dataset de treino 75%  da base original e teste 25% base original.
set.seed(123)
smp_size <- floor(0.75 * nrow(cancers))
train_ind <- sample(seq_len(nrow(cancers)), size = smp_size)

train <- cancers[train_ind, ]
test <- cancers[-train_ind, ]
