#install.packages("stringdist")

library('stringdist')
library('dplyr')
library('readxl')
library('stringr')
library("stringi")


municipios_mg <- read_excel("C:/Users/Gbrie/Workspace/fuzzy_matching/base/municipios_mg.xlsx", 
                            col_types = c("numeric", "text", "text", 
                                          "text", "text"))



#criando uma coluna com os nomes maiusculos e sem espaço
municipios_mg <- municipios_mg %>%
                    mutate(MUNICIPIO = str_replace_all(str_to_upper(stri_trans_general(municipios_mg$municipio, 'latin-ascii')), " ", ""))
                    

exemplo <- data.frame(muni = c("Presidnte Bernardes", "Tre Corações", "Piedade do Reio Grande", "Silverania"))
                         
exemplo <- exemplo %>%
              mutate(MUNICIPIO = str_replace_all(str_to_upper(stri_trans_general(exemplo$muni, 'latin-ascii')), " ", ""))
                         

linha_match <- amatch(exemplo$MUNICIPIO, municipios_mg$MUNICIPIO, maxDist = 10)
length(exemplo)

for(x in 1:length(linha_match)){
  linha <- municipios_mg[linha_match[x], 2]
  exemplo[x, 1] <- linha
}

