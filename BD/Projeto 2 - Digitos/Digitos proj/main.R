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
install.packages('caret')



# ------------------------------------ First task -----------------------------
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



# ------------------------------------ Second task -----------------------------
# Montar um dataframe com o conteúdo de todos os arquivos



# Caminho raiz do CSV
files <- list.files(path = "./DigitosCompleto")



# Identificar o zero 



#Gerar um dataframe a partir dos arquivos lidos
#Cada arquivo sera uma linha do dataframe



# Vetor com nome de todos os arquivos
vect_files <- as.vector(t(files))



# Ambiente de Teste - 34 arquivos para leitura : (descomentar)
# vect_files <- head(vect_files,35)



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
  v <- cbind(number = file_name[1], v)
  v <- as.numeric(v)
  
  if(NCOL(v) != 4096){
    df <- rbind(df, v)  
  }
}



View(df)