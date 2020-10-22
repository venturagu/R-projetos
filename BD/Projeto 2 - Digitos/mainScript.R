library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2) # melt a data.frame

# ------------------------------------ Leitura do CSV --------------------------
file <- read.csv("./DigitosCompleto/0_001.BMP.inv.pgm", header = FALSE, skip = 3, sep = " ")
files <- list.files(path = "./DigitosCompleto")
file$V18 <- NULL

# ----------------------------------- Exploração do arquivo --------------------
nrow(file)
ncol(file)
dim(files)

View(head(files, n=5))
str(files)

# Tranformar em uma matrix 64x64
v<-as.vector(t(file))
v<-v[-4097]
v<-as.numeric(v)
mt<-matrix(v, byrow =F, 64,64)

#Identificar o zero 

#Gerar um dataframe a partir dos arquivos lidos ---------------------
#Cada arquivo sera uma linha do dataframe
vect_files <-as.vector(t(files))
main_table

for (x in vect_files) {
  file_x <- read.csv("./DigitosCompleto/"+x, header = FALSE, skip = 3, sep = " ")
  v<-as.vector(t(file_x))
}
