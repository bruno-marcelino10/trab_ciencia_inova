# Manipulação dos Dados
library(tidyverse)

#### Importação de Arquivos
# arquivos <- list.files("dados/", full.names = TRUE)
# dados = read.csv("dados/dados.csv", encoding = "utf-8")

#### Extrair o Partido do Autor
df <- dados %>%
  select(dominio, autor, numero, ano, ementa, assuntoGeral) %>%
  separate_rows(autor, sep = "\n") %>% # Múltipla autoria separada para nova linha
  separate_rows(assuntoGeral, sep = "\n") %>% # Múltiplos assuntos separados para nova linha
  mutate(partido = sub(".*   ", "", autor)) %>%
  mutate(autor = sub("   .*", "", autor))

#### Extrair Temas

# Qtde de cada assunto por partido
partidos_assunto <- df %>%
  count(partido, assuntoGeral) %>%
  group_by(assuntoGeral) %>%
  slice_max(order_by = n, n = 15)

# Qtde de cada assunto por deputado
deputado_assunto <- df %>%
  count(autor, assuntoGeral) %>%
  group_by(assuntoGeral) %>%
  slice_max(order_by = n, n = 15)

# Qtde de assuntos por pessoa
assunto <- df %>%
  count(assuntoGeral, sort = TRUE)

# Ciência ao longo dos anos
# topicos <- c("Ciência e Tecnologia",
#              "Educação",
#              "EDUCAÇÃO",
#              "Estabelecimento de Ensino",
#              "ESTABELECIMENTO DE ENSINO",
#              "Ensino Superior",
#              "ENSINO SUPERIOR",
#              "UNIVERSIDADE DO ESTADO DE MINAS GERAIS (UEMG)",
#              "UNIVERSIDADE ESTADUAL DE MONTES CLAROS (UNIMONTES)",
#              "Ensino Público Estadual",
#              "ENSINO PÚBLICO ESTADUAL")

topicos <- c("ciência e tecnologia",
             "educação",
             "estabelecimento de ensino",
             "ensino superior",
             "universidade do estado de minas gerais (uemg)",
             "universidade estadual de montes claros (unimontes)",
             "ensino público estadual")

ano_ciencia <- df %>% 
  mutate(assuntoGeral = tolower(assuntoGeral)) %>% 
  count(ano, assuntoGeral) %>% 
  filter(assuntoGeral %in% topicos) %>% 
  group_by(ano) %>% 
  summarise("n" = sum(n))
  
tipos_assuntos <- df %>% 
  mutate(assuntoGeral = tolower(assuntoGeral)) %>% 
  count(ano, assuntoGeral) %>% 
  filter(assuntoGeral %in% topicos) %>% 
  group_by(assuntoGeral) %>% 
  summarise("n" = sum(n)) %>% 
  arrange(desc(n))
  
partido_ciencia <- df %>%
  mutate(assuntoGeral = tolower(assuntoGeral),
         partido = toupper(partido)) %>% 
  filter(!str_detect(`partido`, "GOVERNADOR")) %>% 
  count(partido, assuntoGeral) %>% 
  filter(assuntoGeral %in% topicos) %>% 
  group_by(partido) %>% 
  summarise("n" = sum(n))

ementas <- dados %>%
  select(ano, autor, ementa, assuntoGeral) %>%
  separate_rows(autor, sep = "\n") %>% # Múltipla autoria separada para nova linha
  separate_rows(assuntoGeral, sep = "\n") %>% # Múltiplos assuntos separados para nova linha
  mutate(partido = sub(".*   ", "", autor)) %>%
  mutate(autor = sub("   .*", "", autor))

#### Escreve os dataframes 
# write_csv(partidos_assunto, "dados/partidos_assunto.csv")
# write_csv(deputado_assunto, "dados/deputado_assunto.csv")
# write_csv(assunto, "dados/assunto.csv")
# library("xlsx")
# write.xlsx(partidos_assunto, "dados/partidos_assunto.xlsx")
# write.xlsx(deputado_assunto, "dados/deputado_assunto.xlsx")
# write.xlsx(assunto, "dados/assunto.xlsx")
# write.xlsx(ano_ciencia, "dados/ano_ciencia.xlsx")
# 
