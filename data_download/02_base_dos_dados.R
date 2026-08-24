library(basedosdados)
library(dplyr)
library(glue)
library(readr)
library(janitor)
library(tidyr)

# Lendo uma base auxiliar de municípios
df_aux_municipios <- read.csv("data_download/databases/df_aux_municipios.csv") |>
  drop_na() |>
  select(codmunres, municipio, uf, sigla_uf, regiao)

# =========================================================
# CONFIGURAÇÕES
# =========================================================

set_billing_id("dados-artigo-cobertura")

ano_rais <- 2024
ano_pib <- 2023
ano_va <- 2021
ano_pop <- 2023

dir.create(
  "data_download/databases/base_dos_dados",
  showWarnings = FALSE,
  recursive = TRUE
)

# =======================================================
# 1) RAIS - VÍNCULOS (2024)
# =======================================================

query_vinculos <- glue("
  SELECT
    ano,
    sigla_uf,
    id_municipio,
    cnae_2_subclasse,
    valor_remuneracao_media,
    vinculo_ativo_3112
  FROM `basedosdados.br_me_rais.microdados_vinculos`
  WHERE ano = {ano_rais}
    AND vinculo_ativo_3112 = '1'
")

vinculos <- read_sql(query_vinculos)

# =======================================================
# SETORES (vínculos)
# =======================================================

vinculos <- vinculos |>
  mutate(
    codmunres = substr(id_municipio, 1, 6),
    
    cnae2 = substr(cnae_2_subclasse, 1, 2),
    
    setor = case_when(
      cnae2 %in% c("01","02","03") ~ "Agropecuaria",
      
      cnae2 %in% c(
        "05","06","07","08","09",
        "10","11","12","13","14",
        "15","16","17","18","19",
        "20","21","22","23","24",
        "25","26","27","28","29",
        "30","31","32","33"
      ) ~ "Industria",
      
      cnae2 %in% c("45","46","47") ~ "Comercio",
      
      TRUE ~ "Servicos"
    )
  )

# =======================================================
# INDICADORES VÍNCULOS
# =======================================================

indicadores <- vinculos |>
  group_by(codmunres) |>
  summarise(
    n_vinculos = n(),
    massa_salarial_media =
      mean(valor_remuneracao_media, na.rm = TRUE),
    .groups = "drop"
  )

# =======================================================
# DISTRIBUIÇÃO SETORIAL
# =======================================================

setores <- vinculos |>
  group_by(codmunres, setor) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(codmunres) |>
  mutate(porc_setor = round(n / sum(n) * 100, 3)) |>
  ungroup() |>
  select(codmunres, setor, porc_setor) |>
  pivot_wider(
    names_from = setor,
    values_from = porc_setor,
    names_prefix = "porc_vinculos_setor_",
    values_fill = 0
  ) |>
  clean_names()

# =======================================================
# 2) RAIS - ESTABELECIMENTOS (2024)
# =======================================================

query_estab <- glue("
  SELECT
    ano,
    id_municipio,
    cnae_2_subclasse,
    quantidade_vinculos_ativos
  FROM `basedosdados.br_me_rais.microdados_estabelecimentos`
  WHERE ano = {ano_rais}
")

estab <- read_sql(query_estab)

estab <- estab |>
  mutate(
    codmunres = substr(id_municipio, 1, 6),
    
    empregados = quantidade_vinculos_ativos,
    
    cnae2 = substr(cnae_2_subclasse, 1, 2),
    
    setor = case_when(
      cnae2 %in% c(
        "05","06","07","08","09",
        "10","11","12","13","14",
        "15","16","17","18","19",
        "20","21","22","23","24",
        "25","26","27","28","29",
        "30","31","32","33"
      ) ~ "Industria",
      
      TRUE ~ "Outros"
    ),
    
    porte = case_when(
      setor == "Industria" & empregados <= 19 ~ "micro",
      setor == "Industria" & empregados <= 99 ~ "pequeno",
      setor == "Industria" & empregados <= 499 ~ "medio",
      setor == "Industria" & empregados >= 500 ~ "grande",
      
      empregados <= 9 ~ "micro",
      empregados <= 49 ~ "pequeno",
      empregados <= 99 ~ "medio",
      empregados >= 100 ~ "grande",
      
      TRUE ~ NA_character_
    )
  )

# =======================================================
# PORTE DAS EMPRESAS
# =======================================================

porte_empresas <- estab |>
  group_by(codmunres, porte) |>
  summarise(
    n_empresas = n(),
    .groups = "drop"
  ) |>
  group_by(codmunres) |>
  mutate(
    porc_empresas = round(n_empresas / sum(n_empresas) * 100, 3)
  ) |>
  ungroup() |>
  select(
    codmunres,
    porte,
    porc_empresas
  ) |>
  pivot_wider(
    names_prefix = "porc_empresas_porte_",
    names_from = porte,
    values_from = porc_empresas,
    values_fill = 0
  ) |>
  clean_names()

# =======================================================
# PIB + VA (2023/2021)
# =======================================================

query_pib_va <- glue("
  SELECT
    ano,
    id_municipio,
    pib,
    va,
    va_agropecuaria,
    va_industria,
    va_servicos,
    va_adespss
  FROM `basedosdados.br_ibge_pib.municipio`
  WHERE ano IN ({ano_pib}, {ano_va})
")

pib_va <- read_sql(query_pib_va)

# =======================================================
# PIB (2023)
# =======================================================

pib <- pib_va |>
  filter(ano == ano_pib) |>
  mutate(
    codmunres = substr(id_municipio, 1, 6)
  ) |>
  select(
    codmunres,
    pib
  )

# =======================================================
# VA (2021)
# =======================================================

va <- pib_va |>
  filter(ano == ano_va) |>
  mutate(
    codmunres = substr(id_municipio, 1, 6),
    porc_va_adespss = round(va_adespss / va * 100, 3)
  ) |>
  select(
    codmunres,
    va,
    va_agropecuaria,
    va_industria,
    va_servicos,
    va_adespss,
    porc_va_adespss
  )

# =======================================================
# POPULAÇÃO (2023)
# =======================================================

query_pop <- glue("
  SELECT
    ano,
    id_municipio,
    populacao
  FROM `basedosdados.br_ibge_populacao.municipio`
  WHERE ano = {ano_pop}
")

pop <- read_sql(query_pop)

pop <- pop |>
  mutate(
    codmunres = substr(id_municipio, 1, 6)
  ) |>
  select(
    codmunres,
    populacao
  )

# =======================================================
# JUNÇÃO FINAL
# =======================================================

resultado_final <- df_aux_municipios |> mutate(codmunres = as.character(codmunres)) |>
  left_join(indicadores, by = "codmunres") |>
  left_join(setores, by = "codmunres") |>
  left_join(porte_empresas, by = "codmunres") |>
  left_join(pib, by = "codmunres") |>
  left_join(va, by = "codmunres") |>
  left_join(pop, by = "codmunres") |>
  mutate(
    pib_per_capita = pib / populacao,
    .keep = "unused"
  ) |>
  clean_names()

# =======================================================
# EXPORTAÇÃO
# =======================================================

write_csv(
  resultado_final,
  "data_download/databases/df_indicadores_base_dos_dados.csv"
)