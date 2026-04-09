# Loading the necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)

# Reading the data ------------------------------------------------------------
df_beneficiarias <- read.csv("databases/df_beneficiarias_10_a_49.csv") |>
  mutate(regiao = factor(regiao, levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")))

## Aggregating the data for the hole country
df_beneficiarias_br <- df_beneficiarias |>
  group_by(ano) |>
  summarise(
    porc_dependentes_sus = round((sum(populacao_feminina_10_a_49) - sum(beneficiarias_10_a_49))/sum(populacao_feminina_10_a_49) * 100, 1)
  ) |>
  ungroup() 

## Aggregating the data for each state
df_beneficiarias_ufs <- df_beneficiarias |>
  group_by(ano, uf) |>
  summarise(
    porc_dependentes_sus = round(sum(beneficiarias_10_a_49) / sum(populacao_feminina_10_a_49) * 100, 1)
  ) |>
  ungroup() |>
  mutate(
    uf = factor(uf, levels = df_beneficiarias |> dplyr::select(uf, regiao) |> arrange(regiao, uf) |> pull(uf) |> unique())
  ) |>
  arrange(uf)


# Ploting Brazil's time series -----------------------------------------------
## Ploting the time series
plot_br_time_series <- ggplot(
  data = df_beneficiarias_br,
  mapping = aes(x = ano, y = porc_dependentes_sus)
) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_point(color = "#2C7FB8", size = 2.5) +
  geom_label(
    aes(label = round(porc_dependentes_sus, 1)),
    vjust = -0.5,
    size = 3.5,
    label.size = 0.2,
    fill = "white"
  ) +
  scale_x_continuous(
    breaks = unique(df_beneficiarias_br$ano),
    guide = guide_axis(angle = 45)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(1, 1))
  ) +
  theme_bw(base_size = 14) +
  labs(
    x = "Ano",
    y = "%"
  )

plot_br_time_series

## Exporting the plot
ggsave(
  "figures/Fig1.tiff", plot_br_time_series,
  width = 8, height = 6, units = "in", 
  dpi = 600, compression = "lzw"
)


# Mann-Kendall tests for trend analysis ---------------------------------------
## Joining the country and the states data
df_beneficiarias_tendencia <- full_join(
  df_beneficiarias_br |> mutate(local = "Brasil", .before = "ano"),
  df_beneficiarias_ufs |> rename(local = uf)
)

## Selecting the desired variables
variaveis <- c("porc_dependentes_sus")

## Creating a data.frame with the Mann-Kendall tests results
results_table_completa <- data.frame()

for (i in 1:length(unique(df_beneficiarias_tendencia$local))) {
  localidade <- unique(df_beneficiarias_tendencia$local)[i]
  
  mann_kendall_results <- lapply(df_beneficiarias_tendencia |> filter(local == localidade) |> dplyr::select(all_of(variaveis)), function(x) {
    mann_kendall_test <- mk.test(x)
    return(c(local = localidade, p_value = mann_kendall_test$p.value, z_value = mann_kendall_test$statistic))
  })
  
  p_value <- round(as.numeric(unlist(lapply(mann_kendall_results, `[[`, "p_value"))), 5)
  
  results_table <- data.frame(
    local = as.vector(unlist(lapply(mann_kendall_results, `[[`, "local"))),
    valor_2015 = lapply(variaveis, function(variavel) df_beneficiarias_tendencia |> filter(local == localidade, ano == 2015) |> pull(variavel)) |> as.numeric(),
    valor_2024 = lapply(variaveis, function(variavel) df_beneficiarias_tendencia |> filter(local == localidade, ano == 2024) |> pull(variavel)) |> as.numeric(),
    Mann_Kendall_z = round(as.numeric(unlist(lapply(mann_kendall_results, `[[`, "z_value.z"))), 3),
    Mann_Kendall_p = p_value
  )
  
  results_table_completa <- bind_rows(results_table_completa, results_table)
}

results_table_completa

## Exportando the final table
write.csv(results_table_completa, "databases/tabela_mann_kendall_completa.csv", row.names = FALSE)
