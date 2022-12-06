#install.packages("stringdist")

library('stringdist')
library('dplyr')
library('readxl')
library('stringr')
library("stringi")
library("httr")
library("jsonlite")

# FUNÇÕES

## Função que recebe os dados da API e entrega um arquivo JSON
extrai_api <- function(APIURL){
  #Recebdno as informações da API
  get_budget <- GET(APIURL, type = "basic")
  
  if(get_budget$status_code == 200){
    #transformando as informações em texto
    get_budget_text <- content(get_budget, "text")
    
    #Transformando o texto em JSON, pois assim transforma o texto (.txt) em um arquivo que pode ser lido em forma "tabela"
    #O arquivo .JSON é criado com dois objetos, avisos de hoje e do futuro
    get_budget_json <- fromJSON(get_budget_text, flatten = TRUE)
    
    get_budget_df <- as.data.frame(get_budget_json)
    
    return(get_budget_df)
  } else {
    print("Erro na conexão da api")
  }
  
}

#************************************************#

# URL da api
urlapimuni <- "https://servicodados.ibge.gov.br/api/v1/localidades/estados/MG/distritos"
#busca a lista de municipios para MG, para outros tipos de acesso conferir em https://servicodados.ibge.gov.br/api/docs/localidades


#Recebendo o DF com os municipios de MG
municipios_mg <- extrai_api(urlapimuni)

municipios_mg <- municipios_mg %>% 
                  select(municipio.id, municipio.nome, municipio.microrregiao.id, municipio.microrregiao.nome,
                         municipio.microrregiao.mesorregiao.id, municipio.microrregiao.mesorregiao.nome,
                         ) %>% 
                    distinct(municipio.id, .keep_all = TRUE)
                      
#criando uma coluna com os nomes maiusculos e sem espaço
municipios_mg <- municipios_mg %>%
                    mutate(MUNICIPIO = str_replace_all(str_to_upper(stri_trans_general(municipios_mg$municipio.nome, 'latin-ascii')), " ", ""))
                    

exemplo <- data.frame(muni = c("Presidnte Bernardes", "Tre Corações", "Piedade do Reio Grande", "Silverania"))
                         
exemplo <- exemplo %>%
              mutate(MUNICIPIO = str_replace_all(str_to_upper(stri_trans_general(exemplo$muni, 'latin-ascii')), " ", ""))
                         

linha_match <- amatch(exemplo$MUNICIPIO, municipios_mg$MUNICIPIO, maxDist = 10)
length(exemplo)

for(x in 1:length(linha_match)){
  linha <- municipios_mg[linha_match[x], 2]
  exemplo[x, 1] <- linha
}
