# Extração de Dados

library(httr)
library(jsonlite)
library(tidyverse)

# Maximo tamanho de pagina = 100

# Definir o tipo de proposição a ser extraída conforme nomenclatura disponível em http://dadosabertos.almg.gov.br/ws/proposicoes/ajuda#tiposProposicao
tipo <- "PL"

# Fazer para cada ano
baixar_base <- function(ano) {
    
    #### listar numero de resultados
    
    url <- paste0("http://dadosabertos.almg.gov.br/ws/proposicoes/pesquisa/direcionada?ano=", ano, "&sitTram=2&tipo=", tipo, "&formato=json&p=1") # endere?o de lista de projetos de lei
    
    nr_resultados <- GET(url)
    
    nr_resultados <- rawToChar(nr_resultados$content) %>%
        fromJSON(flatten=TRUE) %>%
        .$resultado %>%
        .$noDocumentos %>%
        as.numeric()
    
    nr_paginas <- ceiling(nr_resultados/100)
    
    
    #### extrair dados para cada página 
    
    if (length(nr_paginas) != 0) {
        
        extrair_paginas <- function(pagina) {
            
            pagina <- 1
            
            url_pagina <- paste0("http://dadosabertos.almg.gov.br/ws/proposicoes/pesquisa/direcionada?ano=", ano, "&sitTram=2&tipo=", tipo, "&formato=json&tp=100&p=", pagina) # endereçoo de lista de projetos de lei
            
            dados_json <- GET(url_pagina)
            
            if(dados_json$status_code == 200) {
                
                
                tabela <- content(dados_json, "text", encoding = "UTF-8") %>%
                    fromJSON(flatten=TRUE)
                
                tabela <- as_tibble(tabela$resultado$listaItem) %>%
                    mutate_all(as.character) %>%
                    select(-horario)
                
                Sys.sleep(5)
                
            } else {
                
                tabela <- tibble(dominio = as.character(pagina), ano = as.character(ano))
                
            }
            
            Sys.sleep(3)
            
            return(tabela)
            
        }
        
        tabela <- map_dfr(1:nr_paginas, extrair_paginas) %>%
            bind_rows()
        
        write_csv(tabela, paste0("dados/", ano, ".csv"))
        
        return(tabela)
        
    }
}

# Definir a quantidade de anos pesquisados (desde 1959)
periodo = 1990:2021 
raw_data <- map(periodo, baixar_base) 

#### União dos dados de todos os anos

# Lista os arquivos na pasta
arquivos <- list.files("dados/", full.names = TRUE)[-c(33:42)]
dados <- map(arquivos, read_csv, col_types = cols(.default = "c")) %>% bind_rows()

### Escreve .csv com os arquivos
#write_csv(dados, "dados/dados.csv")

### Escreve .xlsx com os arquivos
#library("xlsx")
#write.xlsx(dados, "dados/dados.xlsx")
