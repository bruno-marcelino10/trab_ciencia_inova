# Criação de Gráficos

# - Gráfico de barras do percentual de cada assunto geral
graf_1 <- top_n(assunto, n = 15, n) %>%
    mutate(assuntoGeral = tolower(assuntoGeral)) %>% 
    group_by(assuntoGeral) %>% 
    summarise("n" = sum(n)) %>% 
    ggplot(., aes(x = assuntoGeral, y = n)) +
    geom_bar(stat = 'identity', fill = "lightblue", color = "blue") +
    labs(x = "Assunto Geral", y = "", title = "Ocorrência de cada assunto") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

# - Gráfico de barras do percentual de proposições por partido
graf_2 <- partido_ciencia %>% 
    mutate("ratio" = n/sum(partido_ciencia$n)) %>% 
    arrange(desc(ratio)) %>%
    filter(!(partido == "COMISSÃO PARTICIPAÇÃO POPULAR")) %>% 
    top_n(15) %>% 
    ggplot(aes(x = partido, y = ratio)) +
    geom_bar(stat = 'identity', fill = "darkgreen") + 
    labs(x = "Partido", y = "", title = "Percentual de proposições por partido") +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

# - Gráfico de linha do percentual de propostas na área de ciencia e tecnologia ao longo dos anos
graf_3 <- ano_ciencia %>% 
    group_by(ano) %>% 
    summarise("pl_ciencia" = sum(n)) %>% 
    ggplot(aes(x = ano, y = pl_ciencia)) +
    geom_bar(stat = 'identity', color = "green") +
    geom_abline() +
    labs(x = "Ano", y = "",
         title = "Quantidade de propostas na área de Ciência e Tecnologia ao longo dos anos") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

# - Gráfico de barras do percentual de propostas na área de ciencia e tecnologia por partido
graf_4 <- partido_ciencia %>% 
    filter(!(partido == "COMISSÃO PARTICIPAÇÃO POPULAR")) %>% 
    ggplot(aes(x = partido, y = n)) +
    geom_bar(stat = 'identity', fill = "brown1", color = "red") + 
    labs(x = "Partido",
         y = "",
         title = "Quantidade de propostas na área de Ciência e Tecnologia por partido") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 

# - Gráfico de barras com os maiores temas de ciencia e tecnologia
# graf_5 <- tipos_assuntos %>% 
#     ggplot(aes(x = assuntoGeral, y = n)) +
#     geom_bar(stat = 'identity') +
#     labs(x = "Partido", y = "", title = "Temas mais citados de Ciência e Tecnologia") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

# 461 / 48353 ementas contém a palavra "CIÊNCIA"
tmp <- ementas %>% 
    filter(str_detect(ementas$ementa, "CIÊNCIA"))

print(round((461/48353)*100, 2))

# 158 / 48353 ementas contém a palavra "TECNOLOGIA"
tmp <- ementas %>% 
    filter(str_detect(ementas$ementa, "TECNOLOGIA"))

print(round((158/48353)*100, 2))

#### Salvando os gráficos

ggsave("figs/graf1.jpg", plot = graf_1)
ggsave("figs/graf2.jpg", plot = graf_2)
ggsave("figs/graf3.jpg", plot = graf_3)
ggsave("figs/graf4.jpg", plot = graf_4)

