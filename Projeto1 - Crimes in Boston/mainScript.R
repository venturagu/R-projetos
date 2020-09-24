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

