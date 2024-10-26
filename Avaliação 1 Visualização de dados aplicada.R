#devtools::install_github("pabsantos/roadtrafficdeaths")
# install.packages("tidyverse")
# install.packages("geobr")

library(tidyverse)
library(roadtrafficdeaths)
library(geobr)

dados <- rtdeaths

# Gráfico 1 ----

dados %>% 
  group_by(ano_ocorrencia, sexo_vitima) %>% 
  summarise(total = n()) %>%
  na.omit() %>% 
  ggplot(aes( x = ano_ocorrencia, y = total, colour = sexo_vitima))+
  geom_point()+
  geom_line()+
  scale_y_continuous(
    limits = c(0,40000),
    breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000)
  )+
  scale_color_manual(values = c("Masculino" = "blue", "Feminino" = "orange"))+
  labs(
    title = "Avanço das Mortes por Sexo ao Longo do Tempo",
    x = "Ano de ocorrência",
    y = "Total de mortes",
    colour = "Sexo da Vítima"
  )+
  theme_bw()

# Gráfico 2 ----
 
dados %>% 
  ggplot(aes(x = idade_vitima))+
  geom_histogram(fill = "orange2")+
  labs(title = "Histograma das idades das vítimas" ,
       x = "Idade", y = "Quantidade de vítimas")+
  geom_vline(xintercept = mean(dados$idade_vitima, na.rm = TRUE), color = "blue", linetype = 2, size = 0.8)+
  annotate("text", 
           x = mean(dados$idade_vitima, na.rm = TRUE) + 12, 
           y = 115000, 
           label = "Média das idades", 
           size = 4)+
  theme_bw()


# Gráfico 3 ----
dados_estado <- dados %>% 
  filter(ano_ocorrencia == 2022) %>% 
  group_by(nome_uf_ocor) %>% 
  summarise(total = n()) %>% 
  na.omit()

estados <- read_state(code_state = "all")

dados_estado <- dados_estado %>% 
  mutate(nome_uf_ocor = recode(nome_uf_ocor,
                               "Rio Grande do Sul" = "Rio Grande Do Sul",
                               "Rio de Janeiro" = "Rio De Janeiro",
                               "Mato Grosso do Sul" = "Mato Grosso Do Sul",
                               "Espírito Santo" = "Espirito Santo",
                               "Rio Grande do Norte" = "Rio Grande Do Norte"))

grafico_estado <- full_join(estados, dados_estado, by = c("name_state" = "nome_uf_ocor"))

grafico_estado %>% 
  ggplot()+
  geom_sf(aes(fill = total))+
  theme_bw()+
  scale_fill_gradient(low = "yellow", high = "red")+
  labs(
    title = "Heatmap sobre a Quantidade de mortes no Trânsito por estado no ano de 2022"
  )
     

# Gráfico 4 ----

g4 <- dados %>% 
  filter(ano_ocorrencia == 2022) %>% 
  group_by(sexo_vitima) %>% 
  summarise(total = n()) %>% 
  na.omit 

g4 <- g4 %>% 
  mutate(prop = total / sum(total))

pie(g4$total,
    labels = c("Feminino","Masculino"),
    main = "Gráfico de Setores - Tipo de Sexo (2022)",
    col = c("white", "gray"))

text(x = 0.5, y = 0.25, labels = paste0((round(g4[1,3],2))*100,"%"), cex = 1.1)
text(x = -0.25, y = -0.25, labels = paste0((round(g4[2,3],2))*100,"%"), cex = 1.1)
