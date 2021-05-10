load("~/estex/.RData")

###puxando pacotes
library(ggplot2)
library(vcd)

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
        col=c(1,6,2,4,3,7,8),
        ylim=c(0,400))

#tabela de tipos de parto x dias da semana
tab_dia_parto = table(base_fin$PARTO1, base_fin$DTNASC2)


#barplot tipos de parto x dias da semana
barplot(tab_dia_parto, col = c(2,4), horiz = FALSE,
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
        panel.border = element_blank())
grafico_estciv 

#-------------------------------------------------#
#possível barplot

barplot(tabela_mae,
        main="Gráfico de frequência do estado civil da mãe",
        ylab="Frequência",
        xlab="Estado civil",
        col=c(6,2,4,3),
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
     ylab = "Densidade", col = c(4),
     main = " ", ylim = c(0, 1), breaks = 44,border=FALSE, freq = FALSE)


# Gráfico 02 - Histograma com Gráfico de linhas

hist(base_fin$PESO, xlab = "Faixas de Peso em Kg", 
     ylab = "Densidade", col = c(4),
     main = " ", ylim = c(0, 1), breaks = 44,border=FALSE, freq = FALSE)

h1 = density(base_fin$PESO)
lines(h1)


# Gráfico 03 - Histograma com Gráfico de Polígonos

h=hist(base_fin$PESO, xlab = "Faixas de Peso em Kg", 
       ylab = "Densidade", col = c(4),
       main = " ", ylim = c(0, 1), breaks = 44,border=FALSE, freq = FALSE)

lines(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts, 0), 
      type = "l", col = c(1))
#------------------------------------------------------------------------#

h=hist(base_fin$PESO, xlab = "Faixas de Peso em Kg", 
       ylab = "Frenquência", col = c(4),
       main = " ", ylim = c(0, 200), breaks = 44,border=FALSE)

lines(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts, 0), 
      type = "l", col = c(1))


# Gráfico 04 - Diagrama de dispersão

plot(base_fin$PESO, xlab = "Quantidade de Recém-nascidos", 
     ylab = "Peso em Kg", col = c(1))

##Questão 04

#transformando de factor para numerico
base_fin$IDADEMAE = as.numeric(as.character(base_fin$IDADEMAE))

#calculando o coeficiente de correlação de Pearson
cor(base_fin$IDADEMAE,base_fin$PESO)

#tabela peso do recém nascido x idade da mãe
tabela_peso_idademae = table(base_fin$IDADEMAE, base_fin$PESO)
tabela_peso_idademae

#gráfico peso do recém nascido x idade da mãe
graf_peso_idadamae = plot(base_fin$IDADEMAE, base_fin$PESO,
                          xlab = 'Idade da mãe',
                          ylab = 'Peso do recém nascido',
                          xlim = c(10,50),
                          ylim = c(0, 6),
                          col = 'black')


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


## C) Gráfico - Idade e Tipo de Parto

tab_Idade_Parto = table(base_fin$PARTO1, base_fin$IDADEMAE2)
tab_Idade_Parto
tab_Idade_Parto = prop.table(tab_Idade_Parto, 2)
tab_Idade_Parto

barplot(tab_Idade_Parto, col = c(2,3), horiz = FALSE,
        xlab = "Faixas de Idade",
        ylab = "Frequência Relativa", ylim = c(0, 1), 
        beside=FALSE, legend.text = TRUE, border = FALSE)


barplot(tab_Idade_Parto, col = c(2,3), horiz = FALSE,
        xlab = "Faixas de Idade",
        ylab = "Frequência Relativa", ylim = c(0, 1.3), 
        beside=TRUE, legend.text = TRUE, border = FALSE)

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


## C) Gráfico - Escolaridade e Tipo de Parto

tab_Esc_Parto = table(base_fin$PARTO1, base_fin$ESCMAE2)
tab_Esc_Parto
tab_Esc_Parto = prop.table(tab_Esc_Parto, 2)
tab_Esc_Parto

barplot(tab_Esc_Parto, col = c(6,4), horiz = FALSE,
        xlab = "Escolaridade em Anos Concluídos",
        ylab = "Frequência Relativa", ylim = c(0, 1.0), 
        beside=FALSE, legend.text = TRUE, border = FALSE)


barplot(tab_Esc_Parto, col = c(6,4), horiz = FALSE,
        xlab = "Escolaridade em Anos Concluídos",
        ylab = "Frequência Relativa", ylim = c(0, 1.2), 
        beside=TRUE, legend.text = TRUE, border = FALSE)

###################################################################


# 5.3) Associação entre Tipo de Parto e Raça da Mãe

# A) Categorização da Variável Raça da Mãe

base_fin$RACACORMAE2 = base_fin$RACACORMAE


base_fin$RACACORMAE2 = factor(base_fin$RACACORMAE2,
                              labels = c('Branca','Preta', 'Amarela','Parda', 'Indígena'),
                              levels = c(1,2,3,4,5))


# B) Tabela Principal (total e relativa)
# Variável Resposta - Tipo de Parto
# Variável Explicativa - Raça da Mãe

tab_Cor_Parto = table(base_fin$RACACORMAE2, base_fin$PARTO1)
addmargins(tab_Cor_Parto)
tab_Cor_Parto = prop.table(tab_Cor_Parto, 1)
tab_Cor_Parto


## C) Gráfico - Raça da Mãe e Tipo de Parto

tab_Cor_Parto = table(base_fin$PARTO1, base_fin$RACACORMAE2)
tab_Cor_Parto
tab_Cor_Parto = prop.table(tab_Cor_Parto, 2)
tab_Cor_Parto

barplot(tab_Cor_Parto, col = c(2,4), horiz = FALSE,
        xlab = "Raça da Mãe",
        ylab = "Frequência Relativa", ylim = c(0, 1.0), 
        beside=FALSE, legend.text = TRUE, border = FALSE)


barplot(tab_Cor_Parto, col = c(2,4), horiz = FALSE,
        xlab = "Raça da Mãe",
        ylab = "Frequência Relativa", ylim = c(0, 1.2), 
        beside=TRUE, legend.text = TRUE, border = FALSE)

###################################################################
rm(base_raw)
rm(base1)
rm(base2)
rm(base3)

save.image(file = '.RData') 

