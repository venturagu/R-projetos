library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)
library(stringr)
library(class)
library(rpart)
library(rpart.plot)
library('caret')
library(plyr)
# install.packages('caret')
library(factoextra)
# install.packages("factoextra")


# ------------------------------------ Primeiro passo -----------------------------
# Leitura de um arquivo da quarta linha em diante.
file <- read.csv("./DigitosCompleto/0_001.BMP.inv.pgm", header = FALSE, skip = 3, sep = " ")

# Removendo ultima coluna que contém apenas dados vazios
file <- file[-18]

# Exploração do arquivo
dim(file)
View(head(file, n=5))
str(file)

# Transformar em uma matrix 64x64
v<-as.vector(t(file))
NROW(v) # Temos 4097 linhas
NCOL(v) # Temos 1 coluna
v<-v[-4097]
v<-as.numeric(v)
v <- t(v)
mt<-matrix(v, byrow =F, 64,64)

# Apresentação do conteúdo do arquivo
image(1:64, 1:64, mt, col=gray((0:255)/255))


# ------------------------------------ Segundo Passo -----------------------------
# Montar um dataframe com o conteúdo de todos os arquivos

# Caminho raiz do CSV
files <- list.files(path = "./DigitosCompleto")

# Vetor com nome de todos os arquivos
vect_files <- as.vector(t(files))

# Ambiente de Teste - 34 arquivos para leitura : (descomentar)
# vect_files <- head(vect_files,100)
df <- data.frame()

for (x in vect_files) {
  filepath <- file.path(paste("./DigitosCompleto/", x ,sep=""))
  file_read <- read.csv(filepath, header = FALSE, skip = 3, sep = " ")
  file_read <- file_read[-18]
  v<-as.vector(t(file_read))
  v<-v[-4097]
  v<-as.numeric(v)
  v <- t(v)
  
  file_name <- unlist(strsplit(x, "\\."))
  file_name <- unlist(strsplit(file_name[1], "\\_"))
  v <- cbind(v, number = file_name[1])
  v <- as.numeric(v)
  
  if(length(v) == 4097){
    df <- rbind(df, v)  
  }
}

# Atribuindo nome para classe de digitos (0 a 9)
colnames(df)[4097] <- "number"

View(head(file, n=5))
str(df[4097]) # classe de numeros
dim(df)


# ------------------------------------ Terceiro passo -----------------------------

# Análise de variância de cada uma das colunas (var). 
# Alguma coluna apresenta variância muito menor do que outras? Se sim, quantas e quais?
variance <- sapply(df, var)
summary(variance)

low_variance <- variance[unlist(variance >= 0.000000 & variance <= 0.0009)]
no_variance <- variance[unlist(variance == 0.000000)]

length(low_variance)
length(no_variance)

# Verificando as features que tem nenhuma variancia, ou seja, colunas com valores apenas em 0 ou apenas 1
no_variance <- which(apply(df, 2, var) >= 0 & apply(df, 2, var) <= 0.0009)
View(no_variance)

classe <- df[4097] # guardando a classe de numeros
df <- df[-4097] # removendo a classe de numeros para não ser considerada como variancia

# Novo dataframe sem colunas que não apresentam variancia
new_df <- df[ - as.numeric(which(apply(df, 2, var) >= 0 & apply(df, 2, var) <= 0.0009))]
dim(new_df)

# Aplicando PCA para verificar a sugestão de redução de dimensão de forma estatistica
pca <- prcomp(new_df, center = TRUE, scale. = TRUE) #1999 Componentes principais

# PCA com 2 principais componentes
pca_two <- prcomp(new_df, rank. = 2 )

options(max.print=999999)
summary(pca) # A partir do 332 componente, a taxa de riqueza acumulada dos dados se mantem estavel em 90%

# plot porcentagem de explicação de variancias por dimensões
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 10))

# Resultados da análise de componentes principais para variáveis
var <- get_pca_var(pca)
var

# PCA Inividuos por classe de numeros
fviz_pca_ind(pca, geom="point", pointsize= 1.5, habillage = classe$number, alpha.ind = 1)

# PCA variavel com maior contribuição
fviz_pca_var(pca, geom="text", select.var = list(contrib = 1))

# Direção da variavel com maior contribuição por distribuição de individuos 
fviz_pca_biplot(pca, select.var = list(contrib = 1), label="none", pointsize= 1,habillage = classe$number, alpha.ind = 1,
                col.var = "black", # Variables color
)
