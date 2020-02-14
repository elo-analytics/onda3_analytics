#######################################################################################
#-----------------------Introducao ao R ------------------------------------------#####
#-----------------------Analytics -2020-02-14-------------------------------------#####
#######################################################################################




# Introdução 

## O que é o R

sqrt(25)


5%%2

## Utilização de Pacotes

### Instalar pacotes

#install.packages("readxl")

library(readxl)
library(tidyverse)

# Import Data

## Diferentes tipos de dados

## csv - padrão internacional


library(readr)

df <- read_csv(readr_example("mtcars.csv"))
df


## csv - padrão Brasil

library(readr)

df <- read_csv2("a; b\n1,0 ;2,0")
df

## Excel

library(readxl)

sus_do_df_2013 <- read_excel("data/sus_do_df_2013.xlsx")
sus_do_df_2013

sus_sih_df_2013 <- read_excel("data/sus_sih_df_2013.xlsx")
sus_sih_df_2013

# Sanity Check e Análise Exploratória Inicial

## Resumo dos dados

sus_do_df_2013 %>% 
  summary()


# Conhecendo a Base mais detalhadamente


## Categoria das Variáveis

sus_do_df_2013 %>% 
  glimpse()

## Pequenas amostras

sus_do_df_2013 %>% 
  head()

sus_do_df_2013 %>% 
  tail()

## DataExplorer

library(DataExplorer)

### Sumário da base de dados - Tabela

introduce(sus_do_df_2013)

### Sumário da base de dados - Gráfico

plot_intro(sus_do_df_2013)

### Análise dados Faltantes



plot_missing(sus_do_df_2013)

### Análise das variáveis categóricas

plot_bar(sus_do_df_2013)

### Análise das variáveis Numéricas


plot_histogram(sus_do_df_2013)


### Relatório completo da base de dados


create_report(sus_sih_df_2013,
              output_file = "report_sus_sih.html",
              output_dir = 'C:/Users/matha/Desktop/')


create_report(sus_do_df_2013,
              output_file = "report_sus_do.html",
              output_dir = 'C:/Users/matha/Desktop/')

# Wrangling Data

# Muitas vezes, a gente precisa alterar dados na tabela, seja:
#   
# - filtrando valores (`filter`);
# - selecionar colunas (`select`);
# - agrupando (`group_by`);
# - ordenando (`arrange`);
# - criando novas colunas (variáveis) (`mutate`);
# - recodificando valores (`case_when`).

## Filter


mulheres <- sus_do_df_2013 %>% 
  filter(SEXO=="Feminino")

mulheres

## Seleção de Variáveis

sus_do <- sus_do_df_2013 %>%
  select(DTOBITO, SEXO, ESC, LOCOCOR, IDADEanos)

head(sus_do)


## group_by

### Contagem

agrupado_edu <-  sus_do_df_2013 %>% 
  group_by(ESC) %>% 
  count()

agrupado_edu

### Demais medidas


agrupado_edu <- sus_do_df_2013 %>% 
  group_by(ESC) %>%
  summarise(qtt_pessoas = n(),
            mediana_idade = median(IDADEanos),
            media_idade = mean(IDADEanos))

agrupado_edu



## Ordenação

### Crescente



ordena_idade <- sus_do_df_2013 %>% 
  arrange(IDADEanos)

ordena_idade




### Decrescente



ordena_desc_idade <- sus_do_df_2013 %>% 
  arrange(desc(IDADEanos))

ordena_desc_idade



## Criando Variáveis


### Numérica 


sus_do_df_2013 <- sus_do_df_2013 %>% 
  mutate(DIAS_VIDA = IDADEanos*365)

sus_do_df_2013 %>% 
  glimpse()



### Categórica



sus_do_df_2013 <- sus_do_df_2013 %>% 
  mutate(IDADE_CAT = case_when(IDADEanos < 10 ~ "criança", 
                               IDADEanos >= 10 & IDADEanos <= 19 ~ "adolescente", 
                               IDADEanos >= 20 ~ "adulto",
                               TRUE ~ NA_character_))

sus_do_df_2013




# Visualização



#instal.packages(ggplot2)
library(ggplot2)



## Variáveis Numéricas



p <- ggplot(sus_do_df_2013, aes(x = IDADEanos, y = DIAS_VIDA)) +
  geom_point()

p

p <- ggplot(sus_do_df_2013, aes(x = DTNASC, y = DIAS_VIDA )) +
  geom_line()

p


p <- ggplot(data = agrupado_edu, aes(x = ESC, y = media_idade)) +
  geom_bar(stat = "identity")
p


library(esquisse)

esquisser(sus_sih_df_2013)

# location ----------------------------------------------------------------
# install.packages("reticulate")
# install.packages("berryFunctions")
# devtools::install_github(repo = 'rCarto/photon') 
#devtools::source_url("https://raw.githubusercontent.com/th1460/CEP2COO/master/CEP2COO.R")

# aplicar a função ao conjunto de ceps

# coo <- tibble(lat = numeric(), lon = numeric())
# for(i in 1:1000){
#   coo <- bind_rows(coo, cep2coo(sus_sih_df_2013[i, "CEP"] %>% pull))
# }
# dados <- bind_cols(sus_sih_df_2013[1:1000,], coo)
#write_csv(dados, "data/dados_geo_sus_do.csv")

dados_geo_sus_do <- read_csv("data/dados_geo_sus_do.csv")
require(leaflet)

leaflet(data = dados_geo_sus_do) %>% 
  addTiles() %>% 
  addMarkers(lng = ~lon, lat = ~lat, popup = ~CEP)
