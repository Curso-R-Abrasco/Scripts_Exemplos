# Tuberculose ----

library(tidyverse)
library(lubridate)
library(foreign)
library(descr)
library(ggpubr)



# variaveis para tabulação TUBERCULOSE -----

vartb <- c("NU_ANO","ID_MUNICIP","DT_DIAG","NU_IDADE_N","CS_SEXO",
           "CS_GESTANT","CS_RACA","CS_ESCOL_N","ID_MN_RESI","TRATAMENTO",
           "RAIOX_TORA","FORMA","EXTRAPU1_N","AGRAVAIDS","AGRAVALCOO",
           "AGRAVDIABE","AGRAVDOENC","BACILOSC_E","CULTURA_ES","HIV",
           "DT_INIC_TR","TRAT_SUPER","SITUA_ENCE","BENEF_GOV","AGRAVDROGA",
           "AGRAVTABAC","TEST_MOLEC","NU_CONTATO","NU_COMU_EX","ANT_RETRO",
           "TEST_SENSI")

### geral ----
tb <- read.dbf("TUBENET.dbf",as.is = TRUE) |> 
  select(all_of(vartb))

head(tb)

# separando ano inicio tratamento
tb$anotrat <- year(tb$DT_INIC_TR)
tb$anotrat <- as.character(tb$anotrat)

# separando anodiag
tb$anodiag <- year((tb$DT_DIAG))

# residentes
tb <- filter(tb,tb$ID_MN_RESI=="330455")

# a partir de 2012
tb <- filter(tb, tb$anotrat > 2011)

#### Mudança diagnostico - retirada -----
tb <- replace_na(tb,list(SITUA_ENCE = '99'))
tb <- filter(tb, SITUA_ENCE !="6" )

# correcoes de conteudo de variaveis
