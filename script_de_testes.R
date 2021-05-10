#Jogar todos os tipos de testes aqui, para deixar o script principal mais limpo


#Testando o método da "fração de amostragem" ----------------
teste_n <- data.frame(Id = sample(c("A", "B", "C", "D", "E"), size = 50, replace = TRUE), 
           Something = sample(100, size = 50, replace = TRUE))

sum(teste_n$Something)


somas <- teste_n  %>%
  group_by(
    Id
  ) %>% 
  summarise(
    freqs = sum(Something)
  )

Fn <- 1000/sum(teste_n$Something)



somas <- mutate(somas, AAA = ceiling(freqs*Fn))
sum(somas$AAA)



teste_n %>% group_by(Id) %>% sample_n(somas$freqs[1])







rm(list = c("teste_n", "somas", "Fn"))
##

#Testando o método da seleção aleatória dos grupos -------

temp <- base2 %>% filter(CODESTAB %in% sample(unique(CODESTAB),2))

unique(temp)

rm(temp)
