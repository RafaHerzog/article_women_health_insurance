# Carregando os pacotes necessários
library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(tidyr)
library(microdatasus)


# Criando alguns objetos auxiliares ---------------------------------------
## Lendo uma base auxiliar de municípios
df_info_muni <- read.csv("data_download/databases/df_aux_municipios.csv")

## Criando um objeto com os códigos dos municípios
codigos_municipios <- df_info_muni |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(
  codmunres = rep(codigos_municipios, each = length(2015:2025)),
  ano = 2015:2025
) |>
  mutate_if(is.character, as.numeric) |>
  left_join(df_info_muni |> select(codmunres, municipio, uf, regiao), by = "codmunres")


# Baixando dados de beneficiários de planos de saúde no Tabnet -----------
## Importando as funções utilizadas para baixar os dados do Tabnet
source("data_download/funcoes_auxiliares.R")

## Baixando os dados de estimativas populacionais de mulheres entre 10 a 49 anos
df_est_pop_fem_10_a_49 <- est_pop_tabnet(periodo = 15:25, filtro_sexo = 2, idade_min = 10, idade_max = 49)

### Verificando se existem NAs
if (any(is.na(df_est_pop_fem_10_a_49))) {
  print("existem NAs")
} else {
  print("não existem NAs")
}

## Baixando os dados de estimativas populacionais de ambos os sexos de 15 a 64 anos
df_est_pop_15_a_64 <- est_pop_tabnet(periodo = 15:25, filtro_sexo = c(1, 2), idade_min = 15, idade_max = 64) |>
  rename(populacao_15_a_64 = populacao_feminina_15_a_64)

### Verificando se existem NAs
if (any(is.na(df_est_pop_15_a_64))) {
  print("existem NAs")
} else {
  print("não existem NAs")
}


## Baixando os dados de mulheres beneficiárias de planos de saúde entre 10 a 49 anos
df_beneficiarias_aux1 <- pop_com_plano_saude_tabnet(
  periodo = 2015:2019
) |>
  select(!municipio)

df_beneficiarias_aux2 <- pop_com_plano_saude_tabnet(
  periodo = 2020:2025
) |>
  select(!municipio)

df_beneficiarias_aux <- df_beneficiarias_aux1 |>
  left_join(df_beneficiarias_aux2, by = "codmunres")

### Verificando se existem NAs
if (any(is.na(df_beneficiarias_aux))) {
  print("existem NAs")
} else {
  print("não existem NAs")
}

#### Aconteceram NAs porque tive que baixar os períodos separadamente
df_beneficiarias_aux[is.na(df_beneficiarias_aux)] <- 0

rm(df_beneficiarias_aux1, df_beneficiarias_aux2)

### Passando o data.frame para o formato long
df_beneficiarias <- df_beneficiarias_aux |>
  mutate(codmunres = as.character(codmunres)) |>
  pivot_longer(
    !codmunres,
    names_to = "mes_ano",
    values_to = paste0("beneficiarias_10_a_49")
  ) |>
  mutate(
    mes = substr(mes_ano, start = 1, stop = 3),
    ano = as.numeric(paste0("20", substr(mes_ano, start = 5, stop = 6))),
    .after = mes_ano,
    .keep = "unused"
  ) |>
  arrange(codmunres, ano) |>
  filter(codmunres %in% df_aux_municipios$codmunres) |>
  left_join(df_est_pop_fem_10_a_49 |> mutate(ano = as.numeric(ano))) |>
  filter(beneficiarias_10_a_49 < populacao_feminina_10_a_49) |>
  group_by(codmunres, ano) |>
  summarise(
    beneficiarias_10_a_49 = round(median(beneficiarias_10_a_49))
  ) |>
  ungroup()

### Juntando com os dados de estimativas populacionais
df_beneficiarias_pop <- left_join(
  df_est_pop_fem_10_a_49 |> mutate(ano = as.numeric(ano)),
  df_beneficiarias
) |>
  left_join(df_est_pop_15_a_64 |> mutate(ano = as.numeric(ano))) |>
  mutate(beneficiarias_10_a_49 = ifelse(is.na(beneficiarias_10_a_49), 0, beneficiarias_10_a_49))

### Calculando a cobertura suplementar, os limites inferiores e superiores para a consideração de outliers e inputando caso necessário
df_cob_suplementar <- df_beneficiarias_pop |>
  mutate(
    cob_suplementar = round(
      beneficiarias_10_a_49 / populacao_feminina_10_a_49,
      3
    )
  ) |>
  group_by(codmunres) |>
  mutate(
    q1 = round(
      quantile(
        cob_suplementar[which(cob_suplementar < 1)],
        0.25
      ),
      3
    ),
    q3 = round(
      quantile(
        cob_suplementar[which(cob_suplementar < 1)],
        0.75
      ),
      3
    ),
    iiq = q3 - q1,
    lim_inf = round(q1 - 1.5 * iiq, 3),
    lim_sup = round(q3 + 1.5 * iiq, 3),
    outlier = ifelse(
      (cob_suplementar > 1) |
        (cob_suplementar < lim_inf | cob_suplementar > lim_sup) |
        (is.na(q1) & is.na(q3)) |
        (is.na(cob_suplementar)),
      1,
      0
    ),
    outlier_superior = ifelse(
      (cob_suplementar > 1) |
        (cob_suplementar > lim_sup) |
        (is.na(q1) & is.na(q3)) |
        (is.na(cob_suplementar)),
      1,
      0
    ),
    outlier_inferior = ifelse(
      (cob_suplementar < lim_inf) |
        (is.na(q1) & is.na(q3)) |
        (is.na(cob_suplementar)),
      1,
      0
    ),
    novo_cob_suplementar = ifelse(
      outlier == 0,
      cob_suplementar,
      round(
        median(cob_suplementar[which(outlier == 0)]),
        3
      )
    ),
    novo_beneficiarias_10_a_49 = round(
      novo_cob_suplementar * populacao_feminina_10_a_49
    )
  ) |>
  ungroup() |>
  select(
    codmunres,
    ano,
    cob_suplementar = novo_cob_suplementar,
    beneficiarias_10_a_49,
    novo_beneficiarias_10_a_49 = novo_beneficiarias_10_a_49,
    populacao_feminina_10_a_49,
    populacao_15_a_64
  ) |>
  mutate(
    cob_suplementar = round(cob_suplementar * 100, 1)
  )

### Juntando com a base auxiliar de municípios
df_cob_suplementar_final <- left_join(
  df_aux_municipios,
  df_cob_suplementar |> mutate(codmunres = as.numeric(codmunres))
) |>
  arrange(codmunres, ano) |>
  mutate(across(cob_suplementar:populacao_15_a_64, ~replace_na(., 0)))

### Salvando a base final
write.csv(df_cob_suplementar_final, "data_download/databases/df_cob_suplementar_2015_2025.csv", row.names = FALSE)
