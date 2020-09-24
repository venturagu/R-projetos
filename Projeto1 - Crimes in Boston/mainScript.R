library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2) # melt a data.frame

# setwd("~/Documents/R aulas/projeto1")

# ------------------------------------ Leitura do CSV --------------------------
crimes <- read.csv("crime.csv", sep=",", na.strings = c('','NA','na','N/A','n/a','NaN','nan'))

# ----------------------------------- Exploração do dataset --------------------
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

# ---------------------------------- Exploração inicial com medidas estatísticas
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

#Nesse decidimos aplicar algo para calcular frequencia de dados nominais/ordinais
#Chamados com mais frequencia
categorias_de_crimes <- factor(crimes$OFFENSE_CODE_GROUP)
categorias_de_crimes
categoryTable <- table(categorias_de_crimes)
converted <- as.data.frame(categoryTable)

crimes_mais_cometidos <- filter(converted, Freq > 15000)
crimes_mais_cometidos

ggplot(crimes_mais_cometidos ,aes(x = categorias_de_crimes, y = Freq)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Distritos com mais ocorrencias 
distritos <- factor(crimes$DISTRICT)
distritosTable <- table(distritos)
convertedDistrict <- as.data.frame(distritosTable)
convertedDistrict

ggplot(convertedDistrict, aes(x = distritos, y = Freq)) +
  geom_point() 

# ------------------------------------ Análises --------------------------------
# Insight buscado -> Quando os crimes ocorrem?

# Identificar todos os tipos de crimes presentes  
unique(crimes$OFFENSE_CODE_GROUP)

# Filtro de todos os crimes relacionado a homicidio, drogas e roubo
homicideFilter <- filter(crimes, OFFENSE_CODE_GROUP == "Homicide")
drugFilter <- filter(crimes, OFFENSE_CODE_GROUP == "Drug Violation")
larcenyFilter <- filter(crimes, OFFENSE_CODE_GROUP == "Larceny")

# Tabela de quantidade de crimes de homicidio, drogas e roubo por hora do dia
homicide_hour<-as.data.frame(table(unlist(homicideFilter$HOUR)))
drug_hour<-as.data.frame(table(unlist(drugFilter$HOUR)))
larceny_hour<-as.data.frame(table(unlist(larcenyFilter$HOUR)))

# Tabela de quantidade de crimes de homicidio, drogas e roubo por dia da semana
homicide_day_of_week<-as.data.frame(table(unlist(homicideFilter$DAY_OF_WEEK)))
drug_day_of_week<-as.data.frame(table(unlist(drugFilter$DAY_OF_WEEK)))
larceny_day_of_week<-as.data.frame(table(unlist(larcenyFilter$DAY_OF_WEEK)))

# Criando dataframes estruturado por horas e dias da semana para os crimes filtrados 
df_crimes_hour <- data.frame(Hour = homicide_hour$Var1, Homocide = homicide_hour$Freq, Drug_Violation = drug_hour$Freq, Larceny = larceny_hour$Freq )
df_crimes_day_of_week <- data.frame(Hour = homicide_day_of_week$Var1, Homocide = homicide_day_of_week$Freq, Drug_Violation = drug_day_of_week$Freq, Larceny = larceny_day_of_week$Freq )

View(df_crimes)

#when you melt essentially you create only one column with the value
#and one column with the variable i.e. your others columns
df_hour <- melt(df_crimes_hour, id.vars='Hour')
df_day_of_week <- melt(df_crimes_day_of_week, id.vars='Hour')

# Grafico de barras crimes filtrados por hora e por dia da semana
ggplot(df_hour, aes(x=Hour, y=value, fill=variable)) + 
  geom_bar(stat='identity', position='dodge')

ggplot(df_day_of_week, aes(x=Hour, y=value, fill=variable)) + 
  geom_bar(stat='identity', position='dodge')


################## Backup da versão anteiror: ##################################

# is.na(crimes)  # há registros que contem NA nas features Reporting area Lat e Long

# TODO verificar os registros que não possuem valores em algumas features (NA != vazio)

# Verificando sentido em medidas estatisticas ----
# vandalism <- filter(crimes, OFFENSE_DESCRIPTION == "VANDALISM")
# vandalism

# year2015 <- filter(crimes, YEAR == 2015)

# ggplot(year2015, aes(OFFENSE_CODE_GROUP, STREET)) + 
#   geom_point()
# 
# plot(crimes$OFFENSE_CODE, crimes$REPORTING_AREA)

#Quando os crimes ocorrreram? - Para o crime de assassinato, drogas(drug violation)
#e furto (larceny),  plote um gráfico que apresente índices de acordo com os horários do dia (independente do ano)
