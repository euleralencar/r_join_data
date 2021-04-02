# APRESENTACAO #####
#' Project: Join data #####
#'
#' Author: Beatriz Milz
#' Vídeo: https://www.youtube.com/watch?v=xnUo25VRH70
#' Fonte dados: http://www.imp.seade.gov.br/frontend/#/tabelas
#' ##########

# INSTALAR PACOTES #####

install.packages("janitor") #para limpeza do código
install.packages("geobr")
install.packages("abjutils")

# CARREGAR PACOTES #####

library(readr)
library(janitor)
library(dplyr)
library(geobr)
library(ggplot2)
library(stringr)  #para usar o minusculo
library(abjutils)

# IMPORTAR DADOS #####

seade_bruto <- read_delim(
  "dados/imp_2021-04-01_20-00_orig.csv",
  ";",
  escape_double = FALSE,
  locale = locale(
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "ISO-8859-1"
  ),
  trim_ws = TRUE
)
View(seade_bruto)

# LIMPEZA DE DADOS - CODIGO IBGE #####
seade_limpo <- seade_bruto %>%
  janitor::clean_names() %>%
  dplyr::rename(
    "esgoto_sanitario" =
      esgoto_sanitario_nivel_de_atendimento_censo_demografico_em_percent,
    "coleta_lixo" =
      coleta_de_lixo_nivel_de_atendimento_censo_demografico_em_percent,
    "abastecimento_agua" =
      abastecimento_de_agua_nivel_de_atendimento_censo_demografico_em_percent
  ) %>%
  dplyr::filter(periodos == 2010) %>%
  # #  Crio esgoto_sanitario_2 para comparar com original, se ok retira o "_2"
  #    dplyr::mutate(
  #      esgoto_sanitario_2 = parse_double(esgoto_sanitario,
  #                                        locale = locale(decimal_mark =  ","))
  dplyr::mutate(#Crio esgoto_sanitario_2 para comparar com original
    esgoto_sanitario = parse_double(esgoto_sanitario,
                                    locale = locale(decimal_mark =  ",")))

glimpse(seade_limpo) #dplyr

# Importar base geobr
municipios_sp <- geobr::read_municipality("SP")

# JOIN COM CODIGO IBGE #####
# Join tabelas
municipios_leftJoin_codigoibge <-
  left_join(x = municipios_sp,
            y = seade_limpo,
            by = c("code_muni" = "cod_ibge"))

# Verificar se houver NA
sum(is.na(municipios_leftJoin_codigoibge$periodos))
# como resultado foi 0 não houve NA como imaginávamos

# VISUALIZACAO
# Visualização dos dados
municipios_leftJoin_codigoibge %>%
  ggplot() +
  geom_sf(aes(fill = esgoto_sanitario)) +
  theme_bw() +
  # scale_fill_viridis_b()
  scale_fill_viridis_b(direction = -1)

# JOIN COM NOME DOS MUNICIPIOS #####
# Join usando os nomes dos munícipios
municipios_leftJoin_nomemunicipio <-
  left_join(x = municipios_sp,
            y = seade_limpo,
            by = c("name_muni" = "localidades"))

# Observe na base a quantidade de NAs pois por diferenças mínimas nos nomes
# pode acontecer de não haver o cruzamento

# Inner join
# Inner Join usando os nomes dos munícipios
municipios_InnerJoin_nomemunicipio <-
  inner_join(x = municipios_sp,
             y = seade_limpo,
             by = c("name_muni" = "localidades"))

municipios_InnerJoin_nomemunicipio %>% nrow()
#rm(municipios_leftJoin_nomemunicipio_IJ)

# Observe que a base original tem 645 linhas e o no inner join houve 552
645 - 552

# antijoin
# Usando antijoin para pegar a base que não bate
municipios_AntiJoin_nomemunicipio <-
  anti_join(x = municipios_sp,
            y = seade_limpo,
            by = c("name_muni" = "localidades"))

#rm(municipios_AntiJoin_nomemunicipio_AJ)

# fuuljoin -> ideal para ver as diferenças
# Usando antijoin para pegar a base que não bate
municipios_FullJoin_nomemunicipio <-
  full_join(x = municipios_sp,
            y = seade_limpo,
            by = c("name_muni" = "localidades"))

#rm(municipios_FullJoin_nomemunicipio_FJ)

# LIMPEZA DE DADOS - NOME MUNICIPIO #####

# 1. deixar os nomes todos minusculos, retirar acentos e retirar hifen
municipios_sp_limpo <- municipios_sp %>%
  mutate(
    nome_muni = stringr::str_to_lower(name_muni),
    nome_muni = abjutils::rm_accent(nome_muni),
    nome_muni = stringr::str_replace_all(nome_muni, "-", " ")
  )

seade_limpo_nomes <- seade_limpo %>%
  mutate(
    nome_muni = stringr::str_to_lower(localidades),
    nome_muni = abjutils::rm_accent(nome_muni),
    nome_muni = stringr::str_replace_all(nome_muni, "-", " ")
  )

municipios_FullJoin_nomemunicipio_2 <-
  full_join(x = municipios_sp_limpo,
            y = seade_limpo_nomes,
            by = c("nome_muni"))

#rm(municipios_leftJoin_nomemunicipio_FJ_2)

# 6 casos não foram iguais
nrow(municipios_leftJoin_codigoibge)  #Referencia
nrow(municipios_FullJoin_nomemunicipio_2)  #Goal

# Verificar quantos casos restantes
NAfilter <-
  municipios_FullJoin_nomemunicipio_2 %>%
  filter(is.na(coleta_lixo))

nrow(NAfilter)

# Este casos são remanescentes pq os nomes foram digitados errados
municipios_sp_limpo <- municipios_sp %>%
  mutate(
    nome_muni = stringr::str_to_lower(name_muni),
    nome_muni = abjutils::rm_accent(nome_muni),
    nome_muni = stringr::str_replace_all(nome_muni, "-", " "),
    nome_muni = dplyr::case_when(
      nome_muni == "moji mirim" ~ "mogi mirim",
      nome_muni == "embu" ~ "embu das artes",
      nome_muni == "florinia" ~ "florinea",
      nome_muni == "sao luis do paraitinga" ~ "sao luiz do paraitinga",
      TRUE ~ nome_muni
    )
  )

seade_limpo_nomes <- seade_limpo %>%
  mutate(
    nome_muni = stringr::str_to_lower(localidades),
    nome_muni = abjutils::rm_accent(nome_muni),
    nome_muni = stringr::str_replace_all(nome_muni, "-", " "),
    nome_muni = dplyr::case_when(
      nome_muni == "moji mirim" ~ "mogi mirim",
      nome_muni == "embu" ~ "embu das artes",
      nome_muni == "florinia" ~ "florinea",
      nome_muni == "sao luis do paraitinga" ~ "sao luiz do paraitinga",
      TRUE ~ nome_muni
    )
  )

#FullJoin
municipios_FullJoin_nomemunicipio_2 <-
  full_join(x = municipios_sp_limpo,
            y = seade_limpo_nomes,
            by = c("nome_muni"))

# Verificar quantos casos restantes
NAfilter <-
  municipios_FullJoin_nomemunicipio_2 %>%
  filter(is.na(coleta_lixo))

nrow(NAfilter)

# VISUALIZACAO
# Visualização dos dados
municipios_FullJoin_nomemunicipio_2 %>%
  ggplot() +
  geom_sf(aes(fill = abastecimento_agua)) +
  theme_bw() +
  # scale_fill_viridis_b()
  scale_fill_viridis_b(direction = -1)

# Selecione o código todo e use CTRL+SHIFT+A para identar totalmente

# Estudar o pacote Fuzzyjoin para bases onde há Matching inexatos, como este caso.
# Ideal para bases de dados maiores

# Fonte 1: https://cran.r-project.org/web/packages/fuzzyjoin/index.html
# Fonte 2: https://github.com/dgrtwo/fuzzyjoin
