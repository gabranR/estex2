## Bibliotécas -----------------
library(tidyverse)
library(read.dbc) #biblioteca necessária para abrir o formato de banco de dados do Ministério da Saúde
set.seed(1234) #como determinado nas instruções

## Lendo os arquivos iniciais ---------------

base_raw <- bind_rows(AC = read.dbc(file = "Dados/DNAC2016.dbc"),
                    AL = read.dbc(file = "Dados/DNAL2016.dbc"),
                    AP = read.dbc(file = "Dados/DNAP2016.dbc"),
                    AM = read.dbc(file = "Dados/DNAM2016.dbc"),
                    BA = read.dbc(file = "Dados/DNBA2016.dbc"),
                    CE = read.dbc(file = "Dados/DNCE2016.dbc"),
                    DF = read.dbc(file = "Dados/DNDF2016.dbc"),
                    ES = read.dbc(file = "Dados/DNES2016.dbc"),
                    GO = read.dbc(file = "Dados/DNGO2016.dbc"),
                    MA = read.dbc(file = "Dados/DNMA2016.dbc"),
                    MT = read.dbc(file = "Dados/DNMT2016.dbc"),
                    MS = read.dbc(file = "Dados/DNMS2016.dbc"),
                    MG = read.dbc(file = "Dados/DNMG2016.dbc"),
                    PA = read.dbc(file = "Dados/DNPA2016.dbc"),
                    PB = read.dbc(file = "Dados/DNPB2016.dbc"),
                    PR = read.dbc(file = "Dados/DNPR2016.dbc"),
                    PE = read.dbc(file = "Dados/DNPE2016.dbc"),
                    PI = read.dbc(file = "Dados/DNPI2016.dbc"),
                    RJ = read.dbc(file = "Dados/DNRJ2016.dbc"),
                    RN = read.dbc(file = "Dados/DNRN2016.dbc"),
                    RS = read.dbc(file = "Dados/DNRS2016.dbc"),
                    RO = read.dbc(file = "Dados/DNRO2016.dbc"),
                    RR = read.dbc(file = "Dados/DNRR2016.dbc"),
                    SC = read.dbc(file = "Dados/DNSC2016.dbc"),
                    SP = read.dbc(file = "Dados/DNSP2016.dbc"),
                    SE = read.dbc(file = "Dados/DNSE2016.dbc"),
                    TO = read.dbc(file = "Dados/DNTO2016.dbc"),
                    .id = "UF"
          )

base1 <- select(base_raw, -APGAR1, -APGAR5, -VERSAOSIST)

## Selecionando apenas os nascidos em Hospitais -------

base1 <- filter(base1, LOCNASC == 1)


## Fazendo algumas transformações e limpezas
base1$CODESTAB <- as.numeric(as.character(base1$CODESTAB))

base1 <- base1[!is.na(base1$CODESTAB), ]
#

## Etapa 1: Selecionar aleatroriamente no máximo 20 casos de todos os conglomerados ---------------

base2 <- base1  %>%
        group_by(
          CODESTAB
        ) %>%
        slice_sample(n = 20)

base2 <- ungroup(base2)
#

## Etapa 2: Selecionar aleatoriamente 200 conglomerados (hospitais) -----------------

base3 <- base2 %>% filter(CODESTAB %in% sample(unique(CODESTAB),200))
#

## Etapa3: Estabelecendo a "Fração de Amostragem" -----

FA <- 2000/nrow(base3)


base3 <- base3 %>%
            group_by(
              CODESTAB
            ) %>%
            mutate(
              Fra_Grupo = ceiling(length(CODESTAB)*FA)
            )

base3 <- ungroup(base3)

##Etapa 4: Selecionando amostras aleatorias dos aglomerados---------
# respeitando a Fração de Amostragem

base_fin <- base3 %>%
  group_by(
    CODESTAB
  ) %>% 
  sample_n(
    Fra_Grupo[1]
  )
  
# Devido ao arredondamento das frequências de cada grupo, dada pela
#função ceiling(), a quantidade de casos ultrapassou os 2000
#sendo necessária uma nova seleção aleatória


base_fin <- base_fin[sample(nrow(base_fin), size = 2000),]

##







