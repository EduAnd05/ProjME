######################################################
#### Trabalho - Metodos Estatisticos - 2023-2024 ####
######################################################
library(ggplot2)
nrow(DadosMarkDig)

### Teste interdependencia qui-quadrado entre plataforma e anuncio ###

(plataforma <- c(DadosMarkDig$plataforma)) # Qualitativa Nominal
(anuncio <- c(DadosMarkDig$anuncio)) # Qualitativa Nominal
(mercado <- c(DadosMarkDig$mercado)) # Qualitativa Nominal
(investimento <- c(DadosMarkDig$investimento)) # Quantitativa Continua
(cliques <- c(DadosMarkDig$cliques)) # Quantitativa Discretas
(conversoes <- c(DadosMarkDig$conversoes)) # Quantitativa Discretas


### Transformar dados dos cliques em qualitativos
intervalos_cliq <- c(139, 390, 641, 892)
labels_cliques <- c("Poucos", "Médio", "Muitos")
cliques_quali <- cut(cliq, breaks = intervalos_cliq, labels = labels_cliques, include.lowest = TRUE);

###### Tabela de contingencias e grafico de dispersao entre plataforma e cliques ######
tabela_cont_plat_cliques <- table(plat, cliques_quali)
addmargins(tabela_cont_plat_cliques)

# Com o objetivo de ver se a plataforma influencia nos cliques 
# H0 - as variaveis nao estao relacionadas
# contra
# H1 - as variaveis estao relacionadas
# 2 variaveis qualitativas, r = 3, c = 3, nivel de significancia = 0.01

chisq.test(x = plat, y = cliques_quali)

# como p-value = 0.9146 >= 0.01 entao nao se rejeita h0

###### 

### Grafico de dispersao entre investimento e cliques
graf_disp_inv_cliq <- ggplot(DadosMarkDig, aes(x=inv, y=cliq, color=plat)) +
  geom_point() +
  labs(subtitle = "Grafico de dispersao entre investimento e cliques",
       y = "cliques", x = "investimento")  
plot(graf_disp_inv_cliq)
# correlacao linear negativa

### Grafico de dispersao entre investimento e conversoes
graf_disp_inv_conv <- ggplot(DadosMarkDig, aes(x=inv, y=conv, color=plat)) +
  geom_point() +
  labs(subtitle = "Grafico de dispersao entre investimento e conversoes",
       y = "conversoes", x = "investimento")  
plot(graf_disp_inv_conv)
# correlacao linear positiva

### Grafico de dispersao entre cliques e conversoes
graf_disp_cliq_conv <- ggplot(DadosMarkDig, aes(x=cliq, y=conv, color=plat)) +
  geom_point() +
  labs(subtitle = "Grafico de dispersao entre cliques e conversoes",
       y = "conversoes", x = "cliques")  
plot(graf_disp_cliq_conv)
# correlacao linear negativa

