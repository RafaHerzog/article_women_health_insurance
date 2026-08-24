library(dplyr)
library(janitor)
library(tidyr)
library(readxl)

# Criando uma função auxiliar para padronizar o nome dos municípios
normalize_municipio <- function(x) {
  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Lendo uma base auxiliar de municípios
df_aux_municipios <- read.csv("data_download/databases/df_aux_municipios.csv") |>
  drop_na() |>
  select(codmunres, municipio, uf, sigla_uf, regiao) |>
  mutate(municipio_padronizado = normalize_municipio(municipio))


# Lendo a base com os indicadores obtidos a partir do Tabnet
df_indicadores_tabnet <- read.csv("data_download/databases/df_cob_suplementar_2015_2025.csv") |>
  select(codmunres, municipio, uf, regiao, ano, cob_suplementar, populacao_15_a_64)


# Lendo a base com os indicadores obtidos a partir do Atlas Brasil
df_indicadores_atlas_aux <- read_excel("data_download/databases/df_indicadores_atlas_brasil.xlsx", sheet = 1) |>
  mutate(
    municipio = sub(" \\([A-Z]{2}\\)$", "", Territorialidades),
    sigla_uf = sub(".*\\(([A-Z]{2})\\)$", "\\1", Territorialidades),
    municipio = case_when(
      municipio == "Eldorado dos Carajás" ~ "Eldorado do Carajás",
      municipio == "Itapagé" ~ "Itapajé",
      municipio == "Iguaraci" ~ "Iguaracy",
      municipio == "Brasópolis" ~ "Brazópolis",
      municipio == "Poxoréo" ~ "Poxoréu",
      TRUE ~ municipio
    ),
    municipio_padronizado = normalize_municipio(municipio)
  ) |> 
  mutate(
    porc_populacao_urbana = round(populacao_urbana / populacao_total * 100, 3),
    .before = "populacao_urbana"
  ) |>
  select(-c(Territorialidades, populacao_urbana)) |>
  rename(municipio_atlas = municipio)

## Juntando-a com a base auxiliar de municípios usando nomes padronizados
df_indicadores_atlas <- left_join(
  df_aux_municipios,
  df_indicadores_atlas_aux,
  by = c("municipio_padronizado", "sigla_uf")
) |>
  select(-c(municipio_padronizado, municipio_atlas)) |>
  arrange(codmunres)

rm(df_indicadores_atlas_aux)


# Lendo a base com os indicadores obtidos a partir da Base dos Dados
df_indicadores_base_dos_dados <- read.csv("data_download/databases/df_indicadores_base_dos_dados.csv") |>
  # Criando o indicador da razão de emprego formal / população de 15 a 64 anos
  left_join(df_indicadores_tabnet |> filter(ano == 2024) |> select(!c(ano, cob_suplementar))) |>
  mutate(
    razao_emprego_formal_pop_15_a_64 = round(n_vinculos / populacao_15_a_64, 3),
    .after = "n_vinculos",
    .keep = "unused"
  )


# Juntando todas as bases
df_indicadores <- df_aux_municipios |>
  left_join(df_indicadores_tabnet |> filter(ano == 2025) |> select(!c(populacao_15_a_64, ano))) |>
  left_join(df_indicadores_atlas) |>
  left_join(df_indicadores_base_dos_dados) |>
  select(
    codmunres:regiao,
    cob_suplementar,
    # 1. Mercado de Trabalho Formal
    razao_emprego_formal_pop_15_a_64, porc_vinculos_setor_agropecuaria:porc_vinculos_setor_servicos, massa_salarial_media,
    
    # 2. Renda e Produto Interno Bruto
    pib_per_capita, renda_per_capita, porc_individuos_quinto_mais_pobre:porc_individuos_quinto_mais_ricos, porc_vulneraveis_pobreza, 
    
    # 3. Desenvolvimento Humano e Escolaridade
    idhm, idhm_educacao, porc_ensino_superior_completo, taxa_de_analfabetismo,
    
    # 4. Estrutura Produtiva e Dinâmica Econômica Local
    porc_empresas_porte_micro, porc_empresas_porte_pequeno, porc_empresas_porte_medio, porc_empresas_porte_grande,
    va_agropecuaria:va_adespss, porc_va_adespss,
    
    # 5. Desigualdade de Renda
    gini, razao_renda_10_mais_ricos_40_mais_pobres,
    
    # 6. Urbanização e Características Demográficas
    porc_populacao_urbana, populacao_total,
    
    # 7. Transferências Governamentais e Proteção Social
    porc_beneficiarios_bolsa_familia
  )


# Exportando a base final
write.csv(df_indicadores, "data_download/databases/df_indicadores.csv", row.names = FALSE)


