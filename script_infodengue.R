# Instalando os pacotes necessários
if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse) 
if(!require(lubridate)) install.packages("lubridate"); library(lubridate) 
if(!require(foreign)) install.packages("foreign"); library(foreign) 
if(!require(xts)) install.packages("xts"); library(xts)
if(!require(flexdashboard)) install.packages("flexdashboard") 
if(!require(plotly)) install.packages("plotly") 
if(!require(knitr)) install.packages("knitr") 
if(!require(DT)) install.packages("DT")

# Atribuição de dados para consultar a API do InfoDengue
url <- "https://info.dengue.mat.br/api/alertcity?" # inserimos o endereço do InfoDengue 
geocode <- 3548807  # indicamos código do IBGE de SCS
disease <- "dengue" # selecionamos a doença
format <- "csv"     # indicamos o formato do arquivo
ew_start <- 1       # indicamos o início da semana epidemiológica
ew_end <- 53        # indicamos o final da semana epidemiológica
ey_start <- 2012    # indicamos o início do ano a ser exportado
ey_end <- 2023      # indicamos o final do ano a ser exportado

# Programando o RStudio para ele trazer os dados diretamente da internet
consulta <- paste0(url,
                   "geocode=", geocode,
                   "&disease=", disease, 
                   "&format=", format, 
                   "&ew_start=", ew_start, 
                   "&ew_end=", ew_end, 
                   "&ey_start=", ey_start, 
                   "&ey_end=", ey_end)

# Visualizando o link armazenado no objeto {consulta}
consulta

# OBS: Copiar e colar o link no navegador para fazer download do arquivo .csv

dengue_scs <- read_csv("dengue_2012-2023.csv")

# Criando uma série temporal 

scs_ts <- xts(
  # selecionando a variável com os dados da incidência de dengue
  x = dengue_scs$p_inc100k,
  # selecionando a variável que contém as datas correspondentes
  order.by = dengue_scs$data_iniSE)

# Plotando gráfico de serie temporal
plot(
  # indicando a série temporal
  x = scs_ts,
  # colocando o título do gráfico
  main = 'Distribuição da incidência de dengue em São Caetano do Sul/SP',
  # escrevendo o título do eixo y
  ylab = 'Incidência por 100.000 habitantes' )

# Analisando no console as estatísticas básicas do banco de dengue para avaliação de média e mediana
summary(dengue_scs$p_inc100k)

# Critério estabelecidos pelo Programa Nacional de Controle de Dengue para avaliação da incidência de casos:
# Baixa incidência: até 100 casos por 100 mil habitantes (linha amarela do gráfico)
# Média incidência: entre 101 e 299 casos por 100 mil habitantes, média de 200 (linha laranja do gráfico)
# Alta incidência: 300 casos ou mais por 100 mil habitantes ou mais (linha vermelha do gráfico)


# Plotando gráfico boxplot com incidência por (`~`) ano
boxplot(dengue_scs$p_inc100k ~ year(dengue_scs$data_iniSE), 
        ylab = 'Incidência por 100.000 habitantes',
        xlab = "Ano de início da semana epidemiológica",
        
        # Inserindo o título do boxplot
        main = "Distribuição da incidência anual de dengue em São Caetano do Sul/SP entre 2012-2023")

# Criando linhas de análise a partir dos parâmetros definidos
abline(
  h = c(300, 200, 100), 
  lty = 2,
  lwd = 2,
  col = c('red', 'orange', 'blue')
) 

nao_epidemic <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

# Criando o objeto {`dengue_ano`} com ano selecionado
dengue_ano <- dengue_scs |>
  filter(year(data_iniSE) == 2023) |>
  mutate(sem_epi = epiweek(data_iniSE))

# Criando o gráfico com o diagrama de controle
dengue_stat <- dengue_scs |>
  filter(year(data_iniSE) %in% nao_epidemic) |>
  mutate(sem_epi = epiweek(data_iniSE)) |>
  group_by(sem_epi) |>
  summarise(
    n = n(),
    media = mean(p_inc100k, na.rm = TRUE), 
    desvio = sd(p_inc100k, na.rm = TRUE) , 
    sup = media + 2 * desvio,
    inf = media - 2 * desvio
  )

head(dengue_stat)

ggplot(data = dengue_stat) +
  aes(x = sem_epi, y = media) +
  geom_line(aes(color = 'cor_media_casos'), size = 1.2) +
  geom_line(aes(y = sup, color = 'cor_limite'), size = 1.2) +
  geom_col(data = dengue_ano,
           aes(y = p_inc100k, fill = 'cor_incidencia'), alpha = 0.4) +
  scale_x_continuous(breaks = 1:53, expand = c(0, 0)) +
  labs(x = 'Semana Epidemiológica',
       y = 'Incidência por 100 mil habitantes',
       title = 'Diagrama de controle de dengue em São Caetano do Sul/SP no ano de 2022') +
  theme_classic() +
  scale_color_manual(
    name = "",
    values = c('cor_media_casos' = 'darkblue', 'cor_limite' = 'red'), labels = c("Limite superior", "Incidência média")
  ) +
  scale_fill_manual(
    name = "",
    values = c('cor_incidencia' = 'deepskyblue'),
    labels = "Incidência de dengue em 2022" 
  )
  

# Diagrama com linha suavizada

ggplot(data = dengue_stat) +
  aes(x = sem_epi, y = media) +
  stat_smooth(
    aes(color = 'cor_incidencia_media'), size = 1.2,
    se = FALSE,
    span = 0.2
  ) +

  stat_smooth(
    aes(y = sup, color = 'cor_limite'), size = 1.2,
    se = FALSE,
    span = 0.2
  ) +
  
  geom_col(data = dengue_ano,
    aes(y = p_inc100k, fill = 'cor_incidencia'), 
    alpha = 0.4) +
  scale_x_continuous(breaks = 1:53, expand = c(0, 0)) +
  labs(x = 'Semana Epidemiológica',
       y = 'Incidência por 100 mil habitantes',
       title = 'Diagrama de controle de dengue em São Caetano do Sul no ano de 2023') +
  theme_classic() +
  scale_color_manual(
    name = "",
    values = c('cor_incidencia_media' = 'darkblue', 'cor_limite' = 'red'), 
    labels = c("Incidência média de casos", "Limite superior")
  ) +
  scale_fill_manual(
    name = "",
    values = c('cor_incidencia' = 'deepskyblue'), labels = "Incidência de dengue em 2023"
  )

