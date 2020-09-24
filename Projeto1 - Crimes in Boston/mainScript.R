library(dplyr)
library(ggplot2)
library(dslabs)

crimes <- read.csv("crime.csv", sep=",")
ncol(crimes)
nrow(crimes)

is.na(crimes)  # há registros que contem NA nas features Reporting area Lat e Long

# TODO verificar os registros que não possuem valores em algumas features (NA != vazio)


#Verificando sentido em medidas estatisticas ----
vandalism <- filter(crimes, OFFENSE_DESCRIPTION == "VANDALISM")
vandalism


year2015 <- filter(crimes, YEAR == 2015)

ggplot(year2015, aes(OFFENSE_CODE_GROUP, STREET)) + 
  geom_point()

plot(crimes$OFFENSE_CODE, crimes$REPORTING_AREA)

#Nesse decidimos aplicar algo para calcular frequencia de dados nominais/ordinais
#Chamados com mais frequencia
categorias_de_crimes <- factor(crimes$OFFENSE_CODE_GROUP)
categorias_de_crimes
categoryTable <- table(categorias_de_crimes)
converted <- as.data.frame(categoryTable)

crimes_mais_cometidos <- filter(converted, Freq > 15000)
crimes_mais_cometidos

ggplot(crimes_mais_cometidos ,aes(x = categorias_de_crimes, y = Freq)) +
  geom_point()

#Distritos com mais ocorrencias 
distritos <- factor(crimes$DISTRICT)
distritosTable <- table(distritos)
convertedDistrict <- as.data.frame(distritosTable)
convertedDistrict

ggplot(convertedDistrict ,aes(x = distritos, y = Freq)) +
  geom_point()



#Fim das medidas estatisticas

#Quando os crimes ocorrreram? - Para o crime de assassinato, drogas(drug violation)
#e furto (larceny),  plote um gráfico que apresente índices de acordo com os horários do dia (independente do ano)

##################### PARTE VENTURA ###########################

library("dplyr")
library("ggplot2")
library("tidyr")

# setwd("~/Documents/R aulas/projeto1")

# ------------------------------------ Leitura do CSV
crimes <- read.csv("crime.csv", sep=",", na.strings = c('','NA','na','N/A','n/a','NaN','nan'))
crimes

# ----------------------------------- Exploração do dataset
nrow(crimes)
ncol(crimes)
dim(crimes)

View(head(crimes, n=5))
str(crimes)

# Após a visalização de uma amostra do dataset com 10 linhas 
# foi possivel perceber valores vazios para coluna SHOOTING, 
# então foi realizado um exploração para entender o numero de dados vazios nas features.
# Desta forma SHOTTING apresentou o maior numero de dados vazios com 318054 registros NA.

View(sapply(crimes, function(x) sum(is.na(x))))

# Retorna a menor e maior data de ocorrencia de crime presente no dataset
min(crimes$OCCURRED_ON_DATE)
max(crimes$OCCURRED_ON_DATE)


# ------------------- Exploração inicial com medidas estatísticas
head(crimes$YEAR, 10)
tail(crimes$YEAR, 10)
data_2015_2010<- filter(crimes, YEAR%in%c(2015, 2010))


# Retornar o menor e o maior indice de latitude e longitude para realizar a escala do mapa de crimes de boston
maior_lat = max(crimes$Lat, na.rm=T)
menor_lat = min(crimes$Lat, na.rm=T)

maior_long = max(crimes$Long, na.rm=T)
menor_long = min(crimes$Long, na.rm=T)


# Mapa dos crimes agrupado por distrito de Boston
ggplot(crimes, aes(x = Long, y = Lat, group=DISTRICT, colour = DISTRICT))+
  geom_point(size = 1, alpha = 1) + 
  xlim(-71.15,-70.90) +
  ylim(42.2,42.4) +
  ggtitle("Crimes ocorridos na região de Boston separado por distritos")

# Mapa dos crimes agrupados por tipo de ofensa
ggplot(crimes, aes(x = Long, y = Lat, group=OFFENSE_CODE, colour = OFFENSE_CODE))+
  geom_point(size = 1, alpha = 1) + 
  xlim(-71.15,-70.90) +
  ylim(42.2,42.4) +
  ggtitle("Crimes ocorridos na região de Boston separado por tipos ofensa de crimes")
