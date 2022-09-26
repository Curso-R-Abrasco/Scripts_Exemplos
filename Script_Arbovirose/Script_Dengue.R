#  dados de Dengue

# Pacotes ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(dygraphs)


# Dados de Dengue ----------------------------------------------------


# Os Arquivos em DBF com os dados obtidos de dengue para os diversos anos
#  devem ser colocados em um subdiretório chamado "bases" (com b minusculo)
# certifique-se de não exita nenhum outro arquivo com a extensão .dbf que não
# sejam os de dengue neste diretório e que não haja nenhum arquivo duplicado.

onde <- 'bases'

arqs <- dir(onde,pattern = '*.[dbf|DBF]$',full.names = TRUE)

if (length(arqs) == 0) stop('Não existem arquivos DBF nesse diretorio')
arqs

## 

le_dengue <- function(x) {
  tmp <-   foreign::read.dbf(x,as.is = TRUE) %>% 
    mutate(across(where(is.integer), as.character))
  return(tmp)  
}

dengue.full <- arqs %>%  map_df(le_dengue)

manter <- c("NU_NOTIFIC","ID_AGRAVO" , "DT_NOTIFIC", "SEM_NOT" , "NU_ANO" ,  "SG_UF_NOT" , "ID_MUNICIP" ,"ID_MN_RESI",
            "DT_SIN_PRI","DT_NASC", "NU_IDADE_N" ,"CS_SEXO", "CLASSI_FIN" ,"CRITERIO","SOROTIPO" ,"EVOLUCAO",'ID_BAIRRO',
            "sem_sint","ano_sint") 

# para tabulaçao usar apenas week e year (lubridate), ao inves de epiweek e epiyear
dengue <- dengue.full %>% 
  mutate(CLASSI_FIN = ifelse(is.na(CLASSI_FIN),8,CLASSI_FIN),
         CHAV_NOTIF = paste0(NU_NOTIFIC,ID_AGRAVO,DT_NOTIFIC,ID_MUNICIP),
         ano_sint = year(DT_SIN_PRI),
         sem_sint = week(DT_SIN_PRI)) %>% # inclusao da coluna ano_sint (ano do sintoma) e sem_sint (semana de sintoma)
  filter(ID_MN_RESI == '330455' & CLASSI_FIN != 5 & ID_AGRAVO == 'A90') %>%
  tibble() %>% 
  select(any_of(manter))
