# Loading the necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)
library(segmented)
library(lmtest)
library(gridExtra)

options(scipen = 999) # Avoid scientific notation

# Reading the data ------------------------------------------------------------
df_cob_suplementar <- read.csv("data_download/databases/df_cob_suplementar_2015_2025.csv") |>
  mutate(regiao = factor(regiao, levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")))

## Aggregating the data for the hole country
df_cob_suplementar_br <- df_cob_suplementar |>
  group_by(ano) |>
  summarise(
    cob_suplementar = round(sum(beneficiarias_10_a_49)/sum(populacao_feminina_10_a_49) * 100, 1)
  ) |>
  ungroup()

## Aggregating the data for each state
df_cob_suplementar_ufs <- df_cob_suplementar |>
  group_by(ano, uf) |>
  summarise(
    cob_suplementar = round(sum(beneficiarias_10_a_49)/sum(populacao_feminina_10_a_49) * 100, 1)
  ) |>
  ungroup() |>
  mutate(
    uf = factor(uf, levels = df_cob_suplementar |> dplyr::select(uf, regiao) |> arrange(regiao, uf) |> pull(uf) |> unique())
  ) |>
  arrange(uf)

## Joining the country and the states data for tendency analysis
df_cob_suplementar_tendencia <- full_join(
  df_cob_suplementar_br |> mutate(local = "Brazil", .before = "ano"),
  df_cob_suplementar_ufs |> rename(local = uf)
)

# Ploting Brazil's time series -----------------------------------------------
font_theme <- theme(
  text = element_text(size = 10),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  plot.title = element_text(size = 11),
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 10)
)

## Ploting the time series
plot_br_time_series <- ggplot(
  data = df_cob_suplementar_br,
  mapping = aes(x = ano, y = cob_suplementar)
) +
  geom_line(
    color = "#2C7FB8",
    linewidth = 1.2
  ) +
  geom_point(
    color = "#2C7FB8",
    size = 2.5
  ) +
  geom_label(
    aes(label = paste0(round(cob_suplementar, 1), "%")),
    vjust = -0.5,
    size = 2.82,
    label.size = 0.2,
    fill = "white"
  ) +
  scale_x_continuous(
    breaks = unique(df_cob_suplementar_br$ano),
    guide = guide_axis(angle = 45)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(1, 1))
  ) +
  theme_bw() +
  font_theme +
  labs(
    x = "Year",
    y = "%"
  )

plot_br_time_series

## Exporting the plot
ggsave(
  "figures/Fig1.tiff",
  plot_br_time_series,
  width = 7.5,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "figures/Fig1.png",
  plot_br_time_series,
  width = 7.5,
  height = 6,
  units = "in",
  dpi = 600
)

# Ploting time series by UF --------------------------------------------------
font_theme <- theme(
  text = element_text(size = 8),
  legend.text = element_text(size = 8),
  legend.title = element_text(size = 8),
  plot.title = element_text(size = 8),
  axis.text = element_text(size = 8),
  axis.title = element_text(size = 8)
)

plot_ufs_time_series <- ggplot(
  data = df_cob_suplementar_ufs,
  mapping = aes(x = ano, y = cob_suplementar)
) +
  geom_line(
    color = "#2C7FB8",
    linewidth = 0.7
  ) +
  geom_point(
    color = "#2C7FB8",
    size = 1.5
  ) +
  facet_wrap(
    ~ uf,
    ncol = 4,
    scales = "free"
  ) +
  scale_x_continuous(
    breaks = seq(2015, 2025, by = 2),
    guide = guide_axis(angle = 45)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  theme_bw() +
  font_theme +
  theme(
    strip.background = element_rect(
      fill = "#f0f0f0",
      color = "#d9d9d9"
    ),
    strip.text = element_text(
      face = "bold",
      size = 8
    ),
    panel.grid.major = element_line(
      color = "#e5e5e5"
    ),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Year",
    y = "%"
  )

plot_ufs_time_series

## Exporting the UF plot
ggsave(
  "figures/Fig2.tiff", plot_ufs_time_series,
  width = 7.5, height = 8.75, units = "in",
  dpi = 600, compression = "lzw"
)

ggsave(
  "figures/Fig2.png", plot_ufs_time_series,
  width = 7.5, height = 8.75, units = "in",
  dpi = 600
)


# Mann-Kendall tests for trend analysis ---------------------------------------
## Selecting the desired variables
variaveis <- c("cob_suplementar")

## Creating a data.frame with the Mann-Kendall tests results
results_table_completa <- data.frame()

for (i in 1:length(unique(df_cob_suplementar_tendencia$local))) {
  localidade <- unique(df_cob_suplementar_tendencia$local)[i]

  mann_kendall_results <- lapply(df_cob_suplementar_tendencia |> filter(local == localidade) |> dplyr::select(all_of(variaveis)), function(x) {
    mann_kendall_test <- mk.test(x)
    return(c(local = localidade, p_value = mann_kendall_test$p.value, z_value = mann_kendall_test$statistic))
  })

  p_value <- round(as.numeric(unlist(lapply(mann_kendall_results, `[[`, "p_value"))), 5)

  results_table <- data.frame(
    local = as.vector(unlist(lapply(mann_kendall_results, `[[`, "local"))),
    valor_2015 = lapply(variaveis, function(variavel) df_cob_suplementar_tendencia |> filter(local == localidade, ano == 2015) |> pull(variavel)) |> as.numeric(),
    valor_2025 = lapply(variaveis, function(variavel) df_cob_suplementar_tendencia |> filter(local == localidade, ano == 2025) |> pull(variavel)) |> as.numeric(),
    Mann_Kendall_z = round(as.numeric(unlist(lapply(mann_kendall_results, `[[`, "z_value.z"))), 3),
    Mann_Kendall_p = p_value
  )

  results_table_completa <- bind_rows(results_table_completa, results_table)
}

results_table_completa

## Exportando the final table
write.csv(results_table_completa, "r_objects/tabela_mann_kendall_completa.csv", row.names = FALSE)


# Segmented regression analysis ----------------------------------------------
## Function to fit a segmented regression model for a given series
ajustar_modelo_segmentado <- function(serie) {
  
  ## Linear model on log scale
  lm_fit <- lm(
    log(cob_suplementar) ~ ano,
    data = serie
  )
  
  ## Davies' test
  davies_test <- tryCatch(
    davies.test(lm_fit, seg.Z = ~ano),
    error = function(e) NULL
  )
  
  davies_p <- if (!is.null(davies_test)) {
    davies_test$p.value
  } else {
    NA_real_
  }
  
  ## Segmented model
  seg_fit <- NULL
  
  if (!is.null(davies_test) && davies_p < 0.05) {
    seg_fit <- tryCatch(
      segmented(
        lm_fit,
        seg.Z = ~ano,
        npsi = 1,
        control = seg.control(display = FALSE)
      ),
      error = function(e) NULL
    )
  }
  
  ## Model used for diagnostics
  modelo_diag <- if (!is.null(seg_fit)) seg_fit else lm_fit
  
  list(
    lm_fit = lm_fit,
    seg_fit = seg_fit,
    modelo_diag = modelo_diag,
    davies_p = davies_p
  )
}


## Function to extract results from the segmented regression model
extrair_resultados_segmentado <- function(
    serie,
    ajuste,
    localidade = NULL
) {
  
  seg_fit <- ajuste$seg_fit
  
  if (!is.null(seg_fit)) {
    
    ## Breakpoint
    segmented_ano <- round(
      seg_fit$psi[1, "Est."]
    )
    
    ## Slopes
    slopes <- slope(seg_fit)[[1]]
    df_resid <- df.residual(seg_fit)
    
    ## APC
    apc_1 <- round(
      (exp(slopes[1, "Est."]) - 1) * 100,
      2
    )
    
    apc_2 <- round(
      (exp(slopes[2, "Est."]) - 1) * 100,
      2
    )
    
    ## p-values
    tendencia_1_p <- round(
      2 * pt(
        -abs(slopes[1, "t value"]),
        df_resid
      ),
      5
    )
    
    tendencia_2_p <- round(
      2 * pt(
        -abs(slopes[2, "t value"]),
        df_resid
      ),
      5
    )
    
    ## Periods
    tendencia_1_anos <- paste0(
      min(serie$ano),
      "-",
      segmented_ano
    )
    
    tendencia_2_anos <- paste0(
      segmented_ano,
      "-",
      max(serie$ano)
    )
    
  } else {
    
    segmented_ano <- NA
    apc_1 <- NA
    apc_2 <- NA
    tendencia_1_p <- NA
    tendencia_2_p <- NA
    tendencia_1_anos <- NA
    tendencia_2_anos <- NA
  }
  
  data.frame(
    local = localidade,
    davies_p = round(ajuste$davies_p, 5),
    breakpoint = segmented_ano,
    tendencia_1_anos = tendencia_1_anos,
    tendencia_1_apc = apc_1,
    tendencia_1_p = tendencia_1_p,
    tendencia_2_anos = tendencia_2_anos,
    tendencia_2_apc = apc_2,
    tendencia_2_p = tendencia_2_p
  )
}


## Function to generate diagnostic plots and tests for a given series and model
gerar_diagnosticos <- function(
    serie,
    ajuste,
    arquivo,
    titulo
) {
  
  modelo_diag <- ajuste$modelo_diag
  residuos <- residuals(modelo_diag)
  
  ## Diagnostic tests
  shapiro_p <- tryCatch(
    shapiro.test(residuos)$p.value,
    error = function(e) NA_real_
  )
  
  bp_p <- tryCatch(
    bptest(modelo_diag)$p.value,
    error = function(e) NA_real_
  )
  
  dw_p <- tryCatch(
    dwtest(modelo_diag)$p.value,
    error = function(e) NA_real_
  )
  
  ## PDF
  pdf(
    file = arquivo,
    width = 8,
    height = 10
  )
  
  par(mfrow = c(3, 2))
  
  ## Fitted model
  plot(
    serie$ano,
    log(serie$cob_suplementar),
    pch = 19,
    xlab = "Ano",
    ylab = "log(Cobertura)",
    main = titulo
  )
  
  if (!is.null(ajuste$seg_fit)) {
    plot(
      ajuste$seg_fit,
      add = TRUE,
      col = 2,
      lwd = 2
    )
    
  } else {
    abline(
      ajuste$lm_fit,
      col = 2,
      lwd = 2
    )
  }
  
  ## Studentized residuals x fitted
  plot(
    fitted(modelo_diag),
    rstudent(modelo_diag),
    pch = 19,
    xlab = "Valores ajustados",
    ylab = "Resíduos studentizados",
    main = "Resíduos studentizados vs Ajustados"
  )
  
  abline(h = 0, lty = 2)
  abline(h = c(-2, 2), lty = 3)
  abline(h = c(-3, 3), lty = 3)
  
  ## QQ-plot
  qqnorm(
    residuos,
    main = "QQ-plot dos resíduos"
  )
  
  qqline(
    residuos,
    lty = 2
  )
  
  ## Histogram
  hist(
    residuos,
    main = "Histograma dos resíduos",
    xlab = "Resíduos"
  )
  
  ## Residuals x time
  plot(
    serie$ano,
    residuos,
    type = "p",
    pch = 19,
    xlab = "Ano",
    ylab = "Resíduos",
    main = "Resíduos vs Tempo"
  )
  
  abline(h = 0, lty = 2)
  
  ## Cook's distance
  plot(
    cooks.distance(modelo_diag),
    type = "h",
    main = "Cook's distance",
    xlab = "Índice da observação",
    ylab = "Distância"
  )
  
  abline(
    h = 4 / length(residuos),
    col = 2,
    lty = 2
  )
  
  par(mfrow = c(1, 1))
  
  ## Diagnostic tests page
  plot.new()
  
  texto <- c(
    titulo,
    "",
    paste(
      "Davies p =",
      round(ajuste$davies_p, 4)
    ),
    paste(
      "Shapiro-Wilk p =",
      round(shapiro_p, 4)
    ),
    paste(
      "Breusch-Pagan p =",
      round(bp_p, 4)
    ),
    paste(
      "Durbin-Watson p =",
      round(dw_p, 4)
    )
  )
  
  text(
    0.05,
    0.95,
    paste(texto, collapse = "\n"),
    adj = c(0, 1),
    cex = 1
  )
  
  ## Diagnostic dataframe
  diag <- data.frame(
    Ano = serie$ano,
    `Resíduo` = residuos,
    `Resíduo padronizado` = rstandard(modelo_diag),
    `Resíduo studentizado` = rstudent(modelo_diag),
    `Cook's distance` = cooks.distance(modelo_diag),
    check.names = FALSE
  )
  
  ## Rounding numeric columns
  diag[-1] <- lapply(
    diag[-1],
    round,
    digits = 4
  )
  
  grid.table(
    diag,
    rows = NULL
  )
  
  dev.off()
}


## Running the segmented regression for each local (1 breakpoint at most)
resultados <- list()

for (localidade in unique(df_cob_suplementar_tendencia$local)) {
  
  ## Selecting the series
  serie <- df_cob_suplementar_tendencia |>
    filter(local == localidade) |>
    arrange(ano)
  
  ## Fitting model
  ajuste <- ajustar_modelo_segmentado(serie)
  
  ## Diagnostic PDF
  nome_local <- gsub(
    "[^[:alnum:]_]",
    "_",
    localidade
  )
  
  gerar_diagnosticos(
    serie = serie,
    ajuste = ajuste,
    arquivo = file.path(
      "diagnostics",
      paste0("diagnostic_", nome_local, ".pdf")
    ),
    titulo = localidade
  )
  
  ## Extracting results
  resultados[[localidade]] <- extrair_resultados_segmentado(
    serie = serie,
    ajuste = ajuste,
    localidade = localidade
  )
}

## Combining results
results_segmented_completa <- bind_rows(resultados)

results_segmented_completa

## Exporting
write.csv(
  results_segmented_completa,
  "r_objects/tabela_segmented_completa.csv",
  row.names = FALSE
)


## Additional diagnostics
### Function to perform sensitivity analysis by removing a specific year from the series
analise_sensibilidade <- function(
    dados = df_cob_suplementar_tendencia,
    localidade,
    ano_remover
) {
  
  ## Selecting the series
  serie <- dados |>
    filter(local == localidade) |>
    arrange(ano)
  
  ## Removing the selected year
  serie_sem_ano <- serie |>
    filter(ano != ano_remover)
  
  ## Original model
  ajuste_original <- ajustar_modelo_segmentado(
    serie
  )
  
  ## Sensitivity model
  ajuste_sem_ano <- ajustar_modelo_segmentado(
    serie_sem_ano
  )
  
  ## Safe name for the output file
  nome_local <- gsub(
    "[^[:alnum:]_]",
    "_",
    localidade
  )
  
  ## Diagnostic plots
  gerar_diagnosticos(
    serie = serie_sem_ano,
    ajuste = ajuste_sem_ano,
    arquivo = file.path(
      "diagnostics",
      paste0(
        "diagnostic_",
        nome_local,
        "_sem_",
        ano_remover,
        ".pdf"
      )
    ),
    titulo = paste0(
      localidade,
      " — sem ",
      ano_remover
    )
  )
  
  ## Extracting results
  resultado_original <- extrair_resultados_segmentado(
    serie = serie,
    ajuste = ajuste_original,
    localidade = localidade
  )
  
  resultado_sem_ano <- extrair_resultados_segmentado(
    serie = serie_sem_ano,
    ajuste = ajuste_sem_ano,
    localidade = localidade
  )
  
  ## Comparison
  comparacao <- data.frame(
    parametro = names(resultado_original)[-1],
    modelo_original = as.character(
      resultado_original[1, -1]
    ),
    sem_ano = as.character(
      resultado_sem_ano[1, -1]
    ),
    check.names = FALSE
  )
  
  return(comparacao)
}

### Bahia, 2019
analise_sensibilidade(localidade = "Bahia", ano_remover = 2019)

### Maranhão, 2021
analise_sensibilidade(localidade = "Maranhão", ano_remover = 2021)
