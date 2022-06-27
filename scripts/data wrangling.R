# Manipulação dos Dados

library(tidyverse)

arquivos <- list.files("./dados/dados_brutos/PL", full.names = TRUE)

arquivos <- arquivos[grepl("2019|2020|2021", arquivos)]

PL <- map(arquivos, read_csv) %>%
  bind_rows()

#### extrair partido do autor

PL <- PL %>%
  select(dominio, autor, numero, ano, ementa, assuntoGeral) %>%
  separate_rows(autor, sep = "\n") %>% # Múltipla autoria separada para nova linha
  separate_rows(assuntoGeral, sep = "\n") %>% # Múltiplos assuntos separados para nova linha
  mutate(partido = sub(".*   ", "", autor)) %>%
  mutate(autor = sub("   .*", "", autor))


#### extrair temas

# Partidos por assunto

partidos_assunto <- PL %>%
  count(partido, assuntoGeral) %>%
  group_by(assuntoGeral) %>%
  slice_max(order_by = n, n = 15)

deputado_assunto <- PL %>%
  count(autor, assuntoGeral) %>%
  group_by(assuntoGeral) %>%
  slice_max(order_by = n, n = 15)

assunto <- PL %>%
  count(assuntoGeral, sort = TRUE)

write_csv(partidos_assunto, "./aplicativos/assuntos/dados/partidos_assunto.csv")

write_csv(deputado_assunto, "./aplicativos/assuntos/dados/deputado_assunto.csv")

write_csv(assunto, "./aplicativos/assuntos/dados/assunto.csv")
