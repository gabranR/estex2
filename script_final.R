## Bibliotécas -----------------
library(tidyverse)
library(ggplot2)
library(vcd)
library(read.dbc) #biblioteca necessária para abrir o formato de banco de dados do Ministério da Saúde
###

##Lendo o arquivo de dados pronto -----

base_fin <- readRDS("Dados/base_fin.rds")

#Script das Questões ------------

## Sugestão de paleta de cores para os gráficos

eq_col1 <- c("#003f5c",
             "#2f4b7c",
             "#665191",
             "#a05195",
             "#d45087",
             "#f95d6a",
             "#ff7c43",
             "#ffa600")


##Questão 1

#Transformando a data de nascimento em dia da semana
base_fin$DTNASC2 = as.Date(base_fin$DTNASC,
                           format = "%d%m%y")

base_fin$DTNASC2 = weekdays(as.Date(base_fin$DTNASC2))

#categorizando variavel parto
base_fin$PARTO1 = base_fin$PARTO

base_fin$PARTO1 = factor(base_fin$PARTO1, labels = c("Vaginal","Cesáreo"),
                         levels = c(1,2))

#Fazendo tabela numero de partos x dias de semana
base_fin$DTNASC2 = factor(base_fin$DTNASC2,
                          labels = c('Domingo',
                                     'Segunda-feira',
                                     'Terça-feira',
                                     'Quarta-feira',
                                     'Quinta-feira',
                                     'Sexta-feira',
                                     'Sábado'),
                          levels = c('domingo',
                                     'segunda-feira',
                                     'terça-feira',
                                     'quarta-feira',
                                     'quinta-feira',
                                     'sexta-feira',
                                     'sábado'))

tab.nnasc = table(base_fin$DTNASC2)
tab.nnasc

#Fazendo grafico numero de partos x dias de semana
barplot(tab.nnasc,
        hor=F,
        main="Gráfico de frequência dos partos em determinados dias das semana",
        ylab="Frequência",
        xlab="Dias da semana",
        border = FALSE,
        col= eq_col1,
        ylim=c(0,400))

#tabela de tipos de parto x dias da semana
tab_dia_parto = table(base_fin$PARTO1, base_fin$DTNASC2)


#barplot tipos de parto x dias da semana
barplot(tab_dia_parto, col = eq_col1[c(1,8)], horiz = FALSE,
        xlab = "Dias da semana",
        ylab = "Frequência", ylim = c(0, 250), 
        beside=TRUE, legend.text = TRUE, border = FALSE)

##Questão 2

#Transformando estado civil em fator com a classificação do dícionario
#criando novo dataframe para utilizar somenta na questão 2
#sem alterar o dataframe principal
dados = base_fin[!is.na(base_fin$ESTCIVMAE),]


dados$ESTCIVMAE = factor(dados$ESTCIVMAE,
                         labels = c('Solteira',
                                    'Casada',
                                    'Viúva',
                                    'Separada judicialmente/divorcia',
                                    'União Estável',
                                    'Ignorado'),
                         levels = c(1,2,3,4,5,9))

dados$ESTCIVMAE = factor(dados$ESTCIVMAE,
                         labels = c('Solteira',
                                    'Casada',
                                    'União Estável',
                                    'Outros',
                                    'Outros',
                                    'Outros'),
                         levels = c('Solteira',
                                    'Casada',
                                    'União Estável',
                                    'Viúva',
                                    'Separada judicialmente/divorcia',
                                    'Ignorado'))
summary(dados$ESTCIVMAE)

#Aqui fizemos a junção de viúva, separada judicialmente/divorciada e ignorado
#em uma unica categoria "Outros" 



#Obtendo dados e tabelas para comparações                            
summary(base_fin$ESTCIVMAE)

tabela_mae = table(dados$ESTCIVMAE)
tabela_mae
tabela_percentual_mae = prop.table(tabela_mae)
tabela_percentual_mae

#fazendo grafico para as comparações

grafico_estciv = ggplot(data = dados, aes(x = "", fill = ESTCIVMAE)) + 
  geom_bar(position = "fill", width = 1) + 
  coord_polar(theta = "y") + 
  xlab("") + 
  ylab("") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(values = eq_col1[c(2, 4, 6, 8)]) +
  labs(fill = "Est. Civil da Mãe")
grafico_estciv 

#-------------------------------------------------#
#possível barplot

barplot(tabela_mae,
        main="Gráfico de frequência do estado civil da mãe",
        ylab="Frequência",
        xlab="Estado civil",
        col=eq_col1[c(2, 4, 6, 8)],
        ylim=c(0,800))

##Questão 3

## Transformando a variável em numeric e padronizando em KG

base_fin$PESO = as.numeric(as.character(base_fin$PESO))

base_fin$PESO = base_fin$PESO/1000



# 3.1) Cálculo de Medidas Resumo e box-plot


# A) Medidas de Posição

summary(base_fin$PESO)

media = mean(base_fin$PESO)
media

mediana = median(base_fin$PESO)
mediana

quartis = quantile(base_fin$PESO)
quartis

## A partir das informações acima foi possível observar a variação
## dos dados em torno das medidas de tendência central. 
## Um ponto importante a ser observado é a próximidade entre
## a média e a mediana.



# B) Medidas de Dispersão

Amplitude_Total = max(base_fin$PESO) - min(base_fin$PESO)
Amplitude_Total

variancia = var(base_fin$PESO)
variancia

desvio_padrao = sd(base_fin$PESO)
desvio_padrao

coeficiente_variacao = sd(base_fin$PESO)/mean(base_fin$PESO)
coeficiente_variacao

Q3 = 3.555
Q1 = 2.930

Amplitude_Interquartilica = Q3 - Q1
Amplitude_Interquartilica

## As medidas de dispersão retornam informações importantes sobre
## a variabilidade do conjunto de dados.Nessa perspectiva, observamos
## um desvio padrão em torno de 0.52, o que representa uma maior distribuição
## dos pesos entre 2.7 KG e 3.72. Além disso, temos um coeficiente
## de variação em cerca de 0.16 ou 16%, o que caracteriza uma variação
## que já não é baixa, mas também não chega a ser um número alto.

## A amplitude interquartilica pode ser usada para verificar a dispersão
## de 50 % dos valores centrais. Ela ajuda em casos em que a média pode ser
## bastante influenciada por valores extremos (outliers).



# C) Medidas de Formas - Assimetria e Achatamento

Assimetria = ((Q3 - median(base_fin$PESO)) - (median(base_fin$PESO) - Q1))/(Q3-Q1)
Assimetria

## Assimetria negativa - distribuição assimétrica à esquerda, mas
## muito próxima de se tornar simétrica por ter valor próximo a 0


D1 = quantile(base_fin$PESO, probs = 0.10)
D1
D9 = quantile(base_fin$PESO, probs = 0.90)
D9

Coef_achatamento = (Q3 - Q1)/(2*(3.81 - 2.65))
Coef_achatamento

## Achatamento > 0.263 - distribuição platicúrtica, mas
## muito próxima de se tornar mesocúrtica por ter valor próximo a 0.263.

## Considerações: a variável peso se aproxima bastante de uma
## distribuição normal.



# D) Desenho esquemático (esquema dos cinco números)

median(base_fin$PESO)
min(base_fin$PESO)
max(base_fin$PESO)
Q3
Q1

boxplot(base_fin$PESO, 
        main = "", xlab= "Peso em Kg dos Recém-nascidos",
        ylab= "", horizontal = TRUE)

boxplot(base_fin$PESO, 
        main = "", xlab= "Peso dos Recém-nascidos",
        ylab= "Peso em Kg")



# 3.2) Tabelas e Gráficos da Variável Peso

# A) Atribuindo Classes para a Distribuição de Frequência

base_fin$PESO2 = base_fin$PESO

Amplitude_Total
min(base_fin$PESO)
max(base_fin$PESO)

stem(base_fin$PESO2) ## não ficou interessante para dimensionar as classes
sqrt(2000) ## aplicável para dimensionar as classes no histograma, mas não na
## na apresentação tabular.

## Intervalos para a tabela: 0 |--- 1 ; 1 |--- 2 ; 2 |--- 3 ; 3 |--- 4 ; 4 |--- 5
## Intervalos para o histograma: de 0.1 em 0.1



# B) Tabela - Distribuição de Frequência por Classes

base_fin$PESO2 = cut(base_fin$PESO2, breaks=c(0, 1, 2, 3, 4, 5), 
                     labels=c("0 |--- 1", "1 |--- 2","2 |--- 3", 
                              "3 |--- 4", "4 |--- 5"),
                     right=FALSE)

tabela_peso = addmargins(table(base_fin$PESO2))
tabela_peso



# C) Gráficos: Histograma e Dispersão

# Gráfico 01 - Histograma

hist(base_fin$PESO, xlab = "Faixas de Peso em Kg", 
     ylab = "Densidade", col = eq_col1[1],
     main = " ", ylim = c(0, 1), breaks = 44,border=FALSE, freq = FALSE)


# Gráfico 02 - Histograma com Gráfico de linhas

hist(base_fin$PESO, xlab = "Faixas de Peso em Kg", 
     ylab = "Densidade", col = eq_col1[1],
     main = " ", ylim = c(0, 1), breaks = 44,border=FALSE, freq = FALSE)

h1 = density(base_fin$PESO)
lines(h1, lwd = 2, col = eq_col1[7])


# Gráfico 03 - Histograma com Gráfico de Polígonos

h=hist(base_fin$PESO, xlab = "Faixas de Peso em Kg", 
       ylab = "Densidade", col = eq_col1[1],
       main = " ", ylim = c(0, 1), breaks = 44,border=FALSE, freq = FALSE)

lines(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts, 0), 
      type = "l", col = eq_col1[7], lwd = 2)
#------------------------------------------------------------------------#

h=hist(base_fin$PESO, xlab = "Faixas de Peso em Kg", 
       ylab = "Frenquência", col = eq_col1[1],
       main = " ", ylim = c(0, 200), breaks = 44,border=FALSE)

lines(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts, 0), 
      type = "l", col = eq_col1[7], lwd = 2)


# Gráfico 04 - Diagrama de dispersão

plot(base_fin$PESO, xlab = "Quantidade de Recém-nascidos", 
     ylab = "Peso em Kg", col = eq_col1[1])

##Questão 04 ----------

#transformando de factor para numerico
base_fin$IDADEMAE = as.numeric(as.character(base_fin$IDADEMAE))


#calculando o coeficiente de correlação de Pearson
cor(base_fin$IDADEMAE,base_fin$PESO)

#tabela peso do recém nascido x idade da mãe
tabela_peso_idademae = data.frame(IDADEMAE = base_fin$IDADEMAE,
                                  PESO = base_fin$PESO,
                                  GESTACAO = base_fin$GESTACAO)
tabela_peso_idademae$GESTACAO <- as.numeric(as.character(tabela_peso_idademae$GESTACAO)) 

#gráfico peso do recém nascido x idade da mãe

plot(base_fin$IDADEMAE, base_fin$PESO,
                          main = "Relação entre a Idade da Mãe\n e o Peso dos Recém-Nascidos no Brasil em 2016",
                          xlab = 'Idade da mãe',
                          ylab = 'Peso do recém nascido',
                          xlim = c(10,50),
                          ylim = c(0, 6),
                          col = eq_col1[1])
#abline(h = median(base_fin$PESO), col = eq_col1[7], lwd = 2, lty = 2)

# Análise controlada para nascimentos prematuros

tab_s_prematuros <- filter(tabela_peso_idademae, GESTACAO >= 5)


plot(tab_s_prematuros$IDADEMAE, tab_s_prematuros$PESO,
     main = "Relação entre a Idade da Mãe e o Peso dos Recém-Nascidos\n em tempo de Gestação Normal",
     xlab = 'Idade da mãe',
     ylab = 'Peso do recém nascido',
     xlim = c(10,50),
     ylim = c(0, 6),
     col = eq_col1[1])
#abline(h = median(tab_s_prematuros$PESO), col = eq_col1[7], lwd = 2, lty = 2)

cor(tab_s_prematuros$IDADEMAE, tab_s_prematuros$PESO)






## Questão 05

# 5.1) Associação entre Tipo de Parto e Idade da Mãe

# A) Categorização das Variáveis Parto e Idade da Mãe

base_fin$IDADEMAE2 = base_fin$IDADEMAE

base_fin$IDADEMAE2 = as.numeric(as.character(base_fin$IDADEMAE2))

base_fin$IDADEMAE2 <- cut(base_fin$IDADEMAE2, breaks=c(10, 15, 20, 25,30, 35, 40, 45, 50), 
                          labels=c("10 |--- 15", "15 |--- 20","20 |--- 25", 
                                   "25 |--- 30", "30 |--- 35", "35 |--- 40",
                                   "40 |--- 45","45 |--- 50"),
                          right=FALSE)

base_fin$IDADEMAE2 = factor(base_fin$IDADEMAE2, labels = c("10 |--- 15", "15 |--- 20","20 |--- 25", 
                                                           "25 |--- 30", "30 |--- 35", "35 |--- 40",
                                                           "40 |--- 45","45 |--- 50"),
                            levels = c("10 |--- 15", "15 |--- 20","20 |--- 25", 
                                       "25 |--- 30", "30 |--- 35", "35 |--- 40",
                                       "40 |--- 45","45 |--- 50"))
####variavel parto já estava categorizada (questao 1)


# B) Tabela Principal (total e relativa)
# Variável Resposta - Tipo de Parto
# Variável Explicativa - Idade da Mãe

tab_Idade_Parto = table(base_fin$IDADEMAE2, base_fin$PARTO1)
addmargins(tab_Idade_Parto)
tab_Idade_Parto = prop.table(tab_Idade_Parto, 1)
tab_Idade_Parto


# C) Coeficiente de Contingência - Idade da Mãe e Parto

tab = xtabs(~ base_fin$IDADEMAE2 + base_fin$PARTO1, data = base_fin)
assocstats(tab)

associação1 = (0.142)/((2-1)/2)**(1/2)
associação1

# Pegamos o coeficiente de contigência e dividimos
# pelo valor do limite superior: 0 < C < ((t-1)/t)**(1/2)
# Nesse caso, t representa o mínimo entre o número de colunas e linhas 
# t = min(2,8)
# Existe relação fraca: [C*<0.3]


## D) Gráfico - Idade e Tipo de Parto

tab_Idade_Parto = table(base_fin$PARTO1, base_fin$IDADEMAE2)
tab_Idade_Parto
tab_Idade_Parto = prop.table(tab_Idade_Parto, 2)
tab_Idade_Parto

barplot(tab_Idade_Parto, col = eq_col1[c(1, 8)], horiz = FALSE,
        xlab = "Faixas de Idade",
        ylab = "Frequência Relativa", ylim = c(0, 1), 
        beside=FALSE, legend.text = TRUE, border = FALSE)


barplot(tab_Idade_Parto, col = eq_col1[c(1, 8)], horiz = FALSE,
        xlab = "Faixas de Idade",
        ylab = "Frequência Relativa", ylim = c(0, 1.3), 
        beside=TRUE, border = FALSE)
legend(x = 2, y = 1.3, legend = c("Vaginal", "Cesário"), cex = 0.8,fill = eq_col1[c(1, 8)])

###################################################################


# 5.2) Associação entre Tipo de Parto e Escolaridade

# A) Categorização das Variável Escolaridade

base_fin$ESCMAE2 = base_fin$ESCMAE


base_fin$ESCMAE2 = as.numeric(as.character(base_fin$ESCMAE2))


base_fin$ESCMAE2 = factor(base_fin$ESCMAE2, labels = c('Nenhuma','1 a 3 anos', '4 a 7 anos',
                                                       '8 a 11 anos', '12 anos ou mais'),
                          levels = c(1,2,3,4,5))


# B) Tabela Principal (total e relativa)
# Variável Resposta - Tipo de Parto
# Variável Explicativa - Escolaridade da Mãe

tab_Esc_Parto = table(base_fin$ESCMAE2, base_fin$PARTO1)
addmargins(tab_Esc_Parto)
tab_Esc_Parto = prop.table(tab_Esc_Parto, 1)
tab_Esc_Parto


## C) Coeficiente de Contingência - Escolaridade da Mãe e Parto

tab2 = xtabs(~ base_fin$ESCMAE2 + base_fin$PARTO1, data = base_fin)
assocstats(tab2)

associação2 = (0.321)/((2-1)/2)**(1/2)
associação2

# Pegamos o coeficiente de contigência e dividimos
# pelo valor do limite superior: 0 < C < ((t-1)/t)**(1/2)
# Nesse caso, t representa o mínimo entre o número de colunas e linhas 
# t = min(2,5)
# Existe relação moderada [0.3 < C* < 0.7]


## D) Gráfico - Escolaridade e Tipo de Parto

tab_Esc_Parto = table(base_fin$PARTO1, base_fin$ESCMAE2)
tab_Esc_Parto
tab_Esc_Parto = prop.table(tab_Esc_Parto, 2)
tab_Esc_Parto

barplot(tab_Esc_Parto, col = eq_col1[c(1,8)], horiz = FALSE,
        xlab = "Escolaridade em Anos Concluídos",
        ylab = "Frequência Relativa", ylim = c(0, 1.0), 
        beside=FALSE, legend.text = TRUE, border = FALSE)


barplot(tab_Esc_Parto, col = eq_col1[c(1,8)], horiz = FALSE,
        xlab = "Escolaridade em Anos Concluídos",
        ylab = "Frequência Relativa", ylim = c(0, 1.2), 
        beside=TRUE, legend.text = TRUE, border = FALSE)

###################################################################


# 5.3) Associação entre Tipo de Parto e Raça da Mãe

# A) Categorização da Variável Raça da Mãe

base_fin$RACACORMAE2 = base_fin$RACACORMAE


base_fin$RACACORMAE2 = factor(
  base_fin$RACACORMAE2,
  labels = c('Branca', 'Amarela', 'Parda', 'Preta', 'Indígena'),
  levels = c(1, 3, 4, 2, 5)
)


# B) Tabela Principal (total e relativa)
# Variável Resposta - Tipo de Parto
# Variável Explicativa - Raça da Mãe

tab_Cor_Parto = table(base_fin$RACACORMAE2, base_fin$PARTO1)
addmargins(tab_Cor_Parto)
tab_Cor_Parto = prop.table(tab_Cor_Parto, 1)
tab_Cor_Parto


## C) Coeficiente de Contingência - Raça da Mãe e Parto

tab3 = xtabs(~ RACACORMAE2 + base_fin$PARTO1, data = base_fin)
assocstats(tab3)

associação3 = (0.234)/((2-1)/2)**(1/2)
associação3

# Pegamos o coeficiente de contigência e dividimos
# pelo valor do limite superior: 0 < C < ((t-1)/t)**(1/2)
# Nesse caso, t representa o mínimo entre o número de colunas e linhas 
# t = min(2,5)
# Existe relação moderada [0.3 < C* < 0.7]



## D) Gráfico - Raça da Mãe e Tipo de Parto

tab_Cor_Parto = table(base_fin$PARTO1, base_fin$RACACORMAE2)
tab_Cor_Parto
tab_Cor_Parto = prop.table(tab_Cor_Parto, 2)
tab_Cor_Parto

barplot(tab_Cor_Parto, col = eq_col1[c(1,8)], horiz = FALSE,
        xlab = "Raça da Mãe",
        ylab = "Frequência Relativa", ylim = c(0, 1.0), 
        beside=FALSE, legend.text = TRUE, border = FALSE)


barplot(tab_Cor_Parto, col = eq_col1[c(1,8)], horiz = FALSE,
        xlab = "Raça da Mãe",
        ylab = "Frequência Relativa", ylim = c(0, 1.2), 
        beside=TRUE, legend.text = TRUE, border = FALSE)

###################################################################






