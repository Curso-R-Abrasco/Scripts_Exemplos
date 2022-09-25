
# Códigos para Covid-19 ---------------------------------------------------
#
# O Script abaixo usa nomes dos campos comuns nas bases do SIVEP 
# sugerimos uma consulta ao dicionario de dados.
#

## Chamando as bibliotecas a serem utilizadas 
library(tidyverse)
library(lubridate)
library(foreign)
library(zoo)
library(ggpubr)

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
# logica (TRUE ou FALSE) que indica se o caso é covid ou não.
# Neste exemplo é  considerado um caso de COVID quando 
# CLASSI_FIN = 5 OU PCR_SARS2 = 1 OU AN_SARS2 = 1

UF_AM <- AM_2020 |> 
  bind_rows(AM_2021) |> 
  bind_rows(AM_2022) |> 
  mutate(
    SEM_EPI = epiweek(DT_SIN_PRI),
    ANO_EPI = epiyear(DT_SIN_PRI),
    COVID = if_else(CLASSI_FIN == 5 | PCR_SARS2 == 1 | 
                      AN_SARS2 == 1,TRUE,FALSE,FALSE) 
    )


# Tabela de população -----------------------------------------------------

# Vamos importar os dados de população  para projetados para 2020 
# por sexo e faixa etária disponível no tabnet do datasus 
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/projpopuf.def

pop_AM <- read_csv2('pop_AM.csv')

pop_TOT <- sum(pop_AM$Total)


# Vamos criar um objeto semana_epi que contem todas as datas do início das 
# semanas epidemiológicas desde a semana 10/2020 até a semana 35/2022.

# Isto é importante para garantirmos que todas as semanas esteja presentes 
# no objeto que usaremos para as subsequentes análises exploratórias 
# construindo  tabelas e gráficos. 

semanas_epi <- tibble(
  inisem = seq(ymd('2020-03-01'),ymd("2022-08-28"),by='week') ) |> 
  mutate(ANO_EPI = epiyear(inisem),
         SEM_EPI = epiweek(inisem))

# Neste momento vamos criar um objeto que contem a contagem de casos de 
# COVID-19 por Semana Epidemiológica. Note que a linha filter(COVID)  
# garante que só os casos de COVID-19 serão contados 
# Observe que sendo a variável COVID  uma variável do tipo lógico 
# filter(COVID) é o mesmo que filter(COVID == TRUE) 

conta_covid <- UF_AM %>% 
  filter(COVID) %>% 
  group_by(ANO_EPI,SEM_EPI) %>% 
  count(name='covid')


# Agora vamos criar o próximo objeto com a contagem de óbitos registrados 
# no SIVEP  por Semana Epidemiológica 
# Note que nosso filtro agora é ser COVID e  EVOLUCAO == 2

conta_obt <- UF_AM %>% 
  filter(COVID & EVOLUCAO == 2) %>% 
  group_by(ANO_EPI,SEM_EPI) %>% 
  count(name='obitos')


# Por Fim, vamos juntar esses objetos com a contagem dos casos e óbitos  
# a partir deles  criar as taxas de  incidência e de mortalidade por covid-19 
# em um único objeto chamado tab_covid

tab_covid <- semanas_epi %>% 
  left_join(conta_covid,by=c('ANO_EPI','SEM_EPI')) %>% 
  left_join(conta_obt,by=c('ANO_EPI','SEM_EPI')) |> 
  filter(inisem >="2020-03-08" & inisem <= "2022-08-06") |> 
  mutate(tx_covid = covid/pop_TOT * 100000,
         tx_mort = obitos/pop_TOT * 100000,
         semana  = fct_inorder(sprintf('%02d-%4d',SEM_EPI,ANO_EPI)) )

# o comando a seguir vai mostra as primeiras 10 linhas dessa tabela 
# Observe que temos nessa tabela as seguintes variáveis :
# "inisem" (data de início da semana epidemiológica), 
# "ANO_EPI" e "SEM_EPI" (Ano e Semana epidemiológica) , 
# "covid" e "obitos" (número de casos e óbitos na semana) , 
# "tx_covid" e "tx_mort" (taxa de incidência e mortalidade por covid-19 
# por  semana por 100.000 hab) ,
# por último  "semana" que é um rotulo para semana epidemiológica a ser exibida 
# nos gráficos  


head(tab_covid,10)

# Vamos salvar a tabela tab_covid em formato CSV caso queria usá-la em outros 
# softwares como o excel 

write_csv2(tab_covid,file = 'tabela_covid.csv')


# Usando a tabela criada vamos fazer alguns gráficos para  o número de casos e
# óbitos usando um gráfico de barras e sobre eles uma media móvel de 4 semanas. 
# Vamos armazenar cada gráfico para poder salvar depois.

# Aqui temos o número de casos por semana epidemiologia e a linha com a 
# media móvel de 4 semanas. 
# Caso queria fazer um gráfico com a taxa de incidência por 100.000h basta 
# substituir “covid” por “tx_covid” nas duas linhas (1ª e 3ª) no código abaixo. 


pcaso <- ggplot(tab_covid,aes(semana ,covid,group=1)) +
  geom_col(fill = 'dodgerblue4') +
  geom_line(aes(semana,y = rollmean(covid,4,fill = T,align = 'center') ),
            size = 1.5,color = 'orange') +
  labs(x = "Semana epidemiológica",y = "Casos")+
  theme_classic() +
  theme(axis.text.x = element_text(size = 8, angle = 90))

pcaso


# 
pobt <- ggplot(tab_covid,aes(semana ,obitos,group=1)) +
  geom_col(fill = 'darkslategray') +
  geom_line(aes(semana,y = rollmean(obitos,4,fill = T,align = 'center') ),
            size = 1.5,color = 'tomato1') +
  labs(x = "Semana epidemiológica",y = "Óbitos")+
  theme_classic() +
  theme(axis.text.x = element_text(size = 8, angle = 90))

pobt

##

ggarrange(pcaso,pobt,nrow=2)

## grafico espelho

ggplot(tab_covid,aes(x=inisem)) +
  geom_area(aes(y=covid,fill="Casos")) +
  geom_area(aes(y=(obitos),fill= "Óbitos")) +  
  scale_fill_manual(values=c(  "Casos"="#EB9EAE"  ,"Óbitos" = "#CB3243"),name='') +
  scale_x_date(date_breaks = '1 months',date_labels = '%b/%y') +
  scale_y_continuous(labels = abs,breaks = seq(-1500,2500,by=500)) + 
  labs(x="Data",y="Casos e òbitos Covid-19")+
  theme_classic() 


### comorbidades

lista_co <-   c("Diabetes","Obesidade","Imunodepressão","Cardiopatia","Doença Renal",
                "Pneumopatia","Outra comorbidade","Doença Neurológica",
                "Asma","Doença Hepática","Doença Hematológica")
tmp <- UF_AM %>% 
  filter(COVID) %>% 
  dplyr::select(ANO_EPI,DIABETES,OBESIDADE,IMUNODEPRE,CARDIOPATI,RENAL,PNEUMOPATI,OUT_MORBI,
                NEUROLOGIC,ASMA,HEPATICA,HEMATOLOGI) %>% 
  group_by(ANO_EPI) |> 
  summarise(across(.cols = everything(), function(x) {sum(x == '1',na.rm = TRUE)/n()} ))

comorb_ano <- tmp %>% 
  pivot_longer(cols = DIABETES:HEMATOLOGI,names_to = 'MORBIDADE',values_to = 'VALOR') |> 
  mutate(MORBIDADE = factor(MORBIDADE,levels = unique(MORBIDADE), labels = lista_co)) %>%  
  filter(!is.na(VALOR )) 

#tmp2 |> arrange(ANO_EPI,desc(VALOR))


ggplot(comorb_ano,aes(fct_reorder(MORBIDADE,VALOR),VALOR))  + 
  geom_col(fill='tomato4') +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L),limits=c(NA,0.27),breaks = seq(0,0.25,by=0.05),expand = c(0,0)) +
  xlab('Comorbidades') + ylab( 'Percentual') +
  facet_wrap(~ANO_EPI) +
  theme_minimal() +
  theme(legend.position = 'right',legend.key.size = unit(0.6, 'cm'),legend.text = element_text(size=12)) +
  theme(axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = 0, face = "plain"),
        strip.background =   element_rect(colour = 'darkblue',fill='gray76'),
        strip.text = element_text(color = "grey20", size = 18))



## Piramide

covid_fx <- UF_AM %>%
  filter(COVID ) %>% 
  filter (CS_SEXO != 'I' ) %>% 
  mutate (fetar = cut(as.numeric(NU_IDADE_N),
                      breaks = c(0,5,10,15,20,30,40,50,60,70,80,120),
                      labels=c("0-4","5-9","10-14","15-19","20-29","30-39",
                               "40-49","50-59","60-69","70-79","80+"),
                      include.lowest = TRUE),
          sexo = factor(CS_SEXO,labels = c('Feminino','Masculino') ),
          EVOLUCAO = ifelse(is.na(EVOLUCAO) | EVOLUCAO == 3 |EVOLUCAO == 9,1,EVOLUCAO )   ) %>% 
  select(sexo,fetar,EVOLUCAO,ANO_EPI,SEM_EPI)

