
# Códigos para Covid-19 ---------------------------------------------------
#
# O Script abaixo usa nomes dos campos comuns nas bases do SIVEP 
# sugerimos uma consulta ao dicionario de dados.
#

## Chamando as bibliotecas a serem utilizadas 
library(tidyverse)
library(lubridate)
library(foreign)
library(aweek)

# Importando os dados do SIVEP do Estado da Amazonia 
# para 2020,2021 e 2022 (até Semana 34)
# Assim você tem 3 bancos em formato DBF , um para cada ano
# Vamos importar e depois unir todos em um único objeto  

AM_2020 <- read.dbf('SIVEP_AM_2020.dbf')
AM_2021 <- read.dbf('SIVEP_AM_2021.dbf')
AM_2022 <- read.dbf('SIVEP_AM_2022.dbf')

# 
# o objeto UF_AM contem todos os dados e além de unir a base para cada ano 
# vamos criar 3 variáveis:  a semana e o ano  epidemiológico e uma variável 
# que indica se o caso é covid ou não.
# É considerado um caso de COVID quando CLASSI_FIN = 5 OU PCR_SARS2 = 1 OU AN_SARS2 = 1

UF_AM <- AM_2020 |> 
  bind_rows(AM_2021) |> 
  bind_rows(AM_2022) |> 
  mutate(
    SEM_EPI = epiweek(DT_SIN_PRI),
    ANO_EPI = epiyear(DT_SIN_PRI),
    COVID = if_else(CLASSI_FIN == 5 | PCR_SARS2 == 1 | AN_SARS2 == 1,TRUE,FALSE,FALSE) 
    )


# Tabela de população -----------------------------------------------------


pop_AM <- read_csv2('pop_AM.csv')

pop_TOT <- sum(pop_AM$Total)

# 
#
semanas_epi <- tibble(
  inisem = seq(ymd('2020-03-01'),ymd("2022-08-28"),by='week') ) |> 
  mutate(EPIANO = epiyear(inisem),
         EPISEM =epiweek(inisem),
         AWEEK = date2week(inisem,week_start = 7) )
