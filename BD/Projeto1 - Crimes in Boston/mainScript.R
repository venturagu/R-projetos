library("tidyr")
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2) # melt a data.frame

# setwd("~/Documents/topicos_BD/Projeto1 - Crimes in Boston")

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
  xlim(-71.15, -70.90) +
  ylim(42.2,42.4) +
  ggtitle("Crimes ocorridos na região de Boston separado por distritos")

# Mapa dos crimes agrupados por tipo de ofensa
ggplot(crimes, aes(x = Long, y = Lat, group=OFFENSE_CODE, colour = OFFENSE_CODE))+
  geom_point(size = 1, alpha = 1) + 
  xlim(-71.15, -70.90) +
  ylim(42.2, 42.4) +
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

#Histogramas de tipos de ocorrencia
barplot(crimes_mais_cometidos$Freq, names.arg = crimes_mais_cometidos$categorias_de_crimes)


#Distritos com mais ocorrencias 
=======
# Histograma de tipos de ocorrencia com rotação na label x para melhor visualização
ggplot(crimes_mais_cometidos,aes(x = categorias_de_crimes, y = Freq)) +
  geom_bar(stat='identity') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Crimes com mais ocorrencias", x="Categoria de crimes", y="Nº de ocorrencias")

# Distritos com mais ocorrencias 
>>>>>>> 40f9c48d1387217ad67c17b496392fdf1b079999:Projeto1 - Crimes in Boston/mainScript.R
distritos <- factor(crimes$DISTRICT)
distritosTable <- table(distritos)
convertedDistrict <- as.data.frame(distritosTable)
convertedDistrict


ggplot(convertedDistrict ,aes(x = distritos, y = Freq)) +
  geom_point()


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

View(df_crimes_hour)

#when you melt essentially you create only one column with the value
#and one column with the variable i.e. your others columns
df_hour <- melt(df_crimes_hour, id.vars='Hour')
df_day_of_week <- melt(df_crimes_day_of_week, id.vars='Hour')

View(df_hour)

# Grafico de barras crimes filtrados por hora e por dia da semana
ggplot(df_hour, aes(x=Hour, y=value, fill=variable)) + 
  geom_bar(stat='identity', position='dodge')

ggplot(df_day_of_week, aes(x=Hour, y=value, fill=variable)) + 
  geom_bar(stat='identity', position='dodge')


# ------------------------------------------------------------------------------
# Insight procurado -> Onde os crimes ocorrem?

# Filtro de todos os crimes relacionado a homicidio, drogas e roubo
homicideFilter <- filter(crimes, OFFENSE_CODE_GROUP == "Homicide")
drugFilter <- filter(crimes, OFFENSE_CODE_GROUP == "Drug Violation")
larcenyFilter <- filter(crimes, OFFENSE_CODE_GROUP == "Larceny")
geralFilter <- filter(crimes, OFFENSE_CODE_GROUP == "Homicide" | OFFENSE_CODE_GROUP == "Drug Violation" | OFFENSE_CODE_GROUP == "Larceny")

# Mapa dos crimes agrupados por crimes de roubo, assasinato e drogas
# apresentação de forma unida no mesmo ambiente 
grafico <- ggplot(geralFilter, aes(x = Long, y = Lat, group=OFFENSE_CODE_GROUP, color = OFFENSE_CODE_GROUP))+
  geom_point(size = 1.2, alpha = 0.9) + 
  xlim(-71.15, -70.90) +
  ylim(42.2, 42.4)

grafico + scale_color_manual(values=c("#E69F00", "red", "#56B4E9"))


# Mapa de crimes de roubo, assasinato e drogas plotadas de forma separada
grafico <- ggplot(geralFilter, aes(x = Long, y = Lat, group=OFFENSE_CODE_GROUP, color = OFFENSE_CODE_GROUP))+
  geom_point(size = 1.2, alpha = 0.9) + 
  xlim(-71.15, -70.90) +
  ylim(42.2, 42.4) +
  facet_grid(.~OFFENSE_CODE_GROUP)

grafico + scale_color_manual(values=c("#E69F00", "red", "#56B4E9"))
barplot(convertedDistrict$Freq, names.arg = convertedDistrict$distritos)

#Apresente este mesmo gráfico com os pontos coloridos pela região.
ggplot(geralFilter, aes(x = Long, y = Lat, group=DISTRICT, colour = DISTRICT))+
  geom_point(size = 1, alpha = 1) + 
  xlim(-71.15, -70.90) +
  ylim(42.2,42.4) +
  facet_grid(.~OFFENSE_CODE_GROUP)
  
# ------------------------------------------------------------------------------
# Explorando os dados temporais

#Para os crimes tratados anteriormente, plote séries temporais que
#apresentem como estes crimes tem evoluído ao longo dos anos, no geral.

homicide_year<-as.data.frame(table(unlist(homicideFilter$YEAR)))
drug_year<-as.data.frame(table(unlist(drugFilter$YEAR)))
larceny_year<-as.data.frame(table(unlist(larcenyFilter$YEAR)))

crimesYears <- data.frame(Year = homicide_year$Var1, Homocide = homicide_year$Freq, Drug_Violation = drug_year$Freq, Larceny = larceny_year$Freq )
View(crimesYears)
df_years <- melt(crimesYears, id.vars='Year')
View(df_years)
ggplot(df_years, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat='identity', position='dodge')


ggplot(data=df_years, aes(x=Year, y=value, group=variable, color=variable)) +
  geom_line()+
  geom_point()

#Para os crimes tratados anteriormente, plote séries temporais, considerando
#apenas as 3 regiões mais violentas.

#Preparação dos dados para todos os crimes
geral_distric<-as.data.frame(table(unlist(geralFilter$DISTRICT)))
distOrder <- geral_distric[order(- geral_distric$Freq), ]

#3 Distritos mais violentos
firstViolent <- distOrder$Var1[1]
secondViolent <- distOrder[2,1]
thirdViolent <- distOrder[3,1]
print(thirdViolent)

#Função que gera o grafico temporal dado um distrito
plot_temporal_grap <- function(dist, geralFilter) {

  #Gerando os filtros especificos
  lacerny_filter <- filter(geralFilter, DISTRICT == dist & OFFENSE_CODE_GROUP == "Larceny")
  drug_filter <- filter(geralFilter, DISTRICT == dist & OFFENSE_CODE_GROUP == "Drug Violation")
  homicide_filter <- filter(geralFilter, DISTRICT == dist & OFFENSE_CODE_GROUP == "Homicide")
  
  #Gerando tabelas
  lacerny_year <- as.data.frame(table(unlist(lacerny_filter$YEAR)))

  drug_year <- as.data.frame(table(unlist(drug_filter$YEAR)))
  homicide_year <- as.data.frame(table(unlist(homicide_filter$YEAR)))
  
  #Gerando a tabela principal que sera usada para gerar os graficos
  main_table <- data.frame(YEAR = lacerny_year$Var1, LACERNY = lacerny_year$Freq, DRUG = drug_year$Freq, 
                              HOMICIDE = homicide_year$Freq)
  
  #Gerando o grafico temporal que sera retornado
  df_main <- melt(main_table, id.vars='YEAR')
  
  result <- ggplot(df_main, aes(x=YEAR, y=value, fill=variable)) + 
    geom_bar(stat='identity', position='dodge') + 
    labs(subtitle = "Ocorrencias ao longos do anos na regiao", title = dist) +
    xlab("Anos") +
    ylab("Ocorrências")
  
  return(result)
}

firstViolent_graph <- plot_temporal_grap(firstViolent, geralFilter)
secondViolent_graph <- plot_temporal_grap(secondViolent, geralFilter)
thirdViolent_graph <- plot_temporal_grap(thirdViolent, geralFilter)


firstViolent_graph
secondViolent_graph
thirdViolent_graph

# Implementação adicional onde é criado um data frame onde relaciona crimes tratados anteriormente,
# para plote de séries temporais com gráfico de linha, seprando por grupos das 3 regiões mais violentas

# Preparação dos dados para todos os crimes
geral_distric <- as.data.frame(table(unlist(geralFilter$DISTRICT)))
View(geral_distric)

# D4, A1 e B2 são os 3 distritos com mais ocorrencias
more_dist <- filter(geralFilter, DISTRICT == "D4" | DISTRICT == "A1"| DISTRICT == "B2")

distric_D4_homicide <- filter(homicideFilter, DISTRICT == "D4")
distric_D4_drug <- filter(drugFilter, DISTRICT == "D4")
distric_D4_larceny <- filter(larcenyFilter, DISTRICT == "D4")

distric_C11_homicide <- filter(homicideFilter, DISTRICT == "A1")
distric_C11_drug <- filter(drugFilter, DISTRICT == "A1")
distric_C11_larceny <- filter(larcenyFilter, DISTRICT == "A1")

distric_B2_homicide <- filter(homicideFilter, DISTRICT == "B2")
distric_B2_drug <- filter(drugFilter, DISTRICT == "B2")
distric_B2_larceny <- filter(larcenyFilter, DISTRICT == "B2")

# Tabela de quantidade de crimes de homicidio, drogas e roubo por ano
more_distric <- as.data.frame(table(unlist(more_dist$YEAR)))
View(more_distric)

# Tabela de quantidade de crimes de homicidio, drogas e roubo por ano separado nos 3 distritos mais violentos
dist_D4_homicide_year <- as.data.frame(table(unlist(distric_D4_homicide$YEAR)))
dist_D4_drug_year <- as.data.frame(table(unlist(distric_D4_drug$YEAR)))
dist_D4_larceny_year <- as.data.frame(table(unlist(distric_D4_larceny$YEAR)))

dist_A1_homicide_year <- as.data.frame(table(unlist(distric_A1_homicide$YEAR)))
dist_A1_drug_year <- as.data.frame(table(unlist(distric_A1_drug$YEAR)))
dist_A1_larceny_year <- as.data.frame(table(unlist(distric_A1_larceny$YEAR)))

dist_B2_homicide_year <- as.data.frame(table(unlist(distric_B2_homicide$YEAR)))
dist_B2_drug_year <- as.data.frame(table(unlist(distric_B2_drug$YEAR)))
dist_B2_larceny_year <- as.data.frame(table(unlist(distric_B2_larceny$YEAR)))

# Criando dataframes estruturado por ano para os crimes filtrados de acordo com o distrito especificado
df_crimes_D4 <- data.frame(Year = dist_D4_homicide_year$Var1, Homocide = dist_D4_homicide_year$Freq, Drug_Violation = dist_D4_drug_year$Freq, Larceny = dist_D4_larceny_year$Freq )
df_crimes_A1 <- data.frame(Year = dist_A1_homicide_year$Var1, Homocide = dist_A1_homicide_year$Freq, Drug_Violation = dist_A1_drug_year$Freq, Larceny = dist_A1_larceny_year$Freq )
df_crimes_B2 <- data.frame(Year = dist_B2_homicide_year$Var1, Homocide = dist_B2_homicide_year$Freq, Drug_Violation = dist_B2_drug_year$Freq, Larceny = dist_B2_larceny_year$Freq )

# Adicionando clouna referente ao distritos aos data frames
nrow(df_crimes_D4)

total <- rbind(df_crimes_D4, df_crimes_A1)
total <- rbind(total, df_crimes_B2)

new_df <- melt(total, id.vars='Year')

new_df$District <- c('D4','D4','D4','D4','A1','A1','A1','A1','B2','B2','B2','B2',
                     'D4','D4','D4','D4','A1','A1','A1','A1','B2','B2','B2','B2',
                     'D4','D4','D4','D4','A1','A1','A1','A1','B2','B2','B2','B2')
View(new_df)

# Gráfico de série temporal do tipo linha seprado em grid dos 3 distritos mais violentos
ggplot(data=new_df, aes(x=Year, y=value, group=variable, color=variable)) +
  geom_line() +
  geom_point() +
  facet_grid(.~District)

