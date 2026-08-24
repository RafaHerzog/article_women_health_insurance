# Loading the necessary libraries
library(knitr)
library(dplyr)
library(tidyr)
library(factoextra)
library(clusterSim)
library(clValid)
library(htmltools)
library(gridExtra)
library(grid)
library(rstatix)
library(sf)
library(geobr)

# Reading and manipulating the data -------------------------------------------
df_cob_suplementar <- read.csv("data_download/databases/df_cob_suplementar_2015_2025.csv") |>
  mutate(regiao = factor(regiao, levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")))

## Filtering and calculating the fertility rates for each period
df_cob_suplementar_19 <- df_cob_suplementar |>
  filter(ano %in% c(2019)) |>
  group_by(codmunres) |>
  summarise(
    cob_suplementar = round(sum(beneficiarias_10_a_49)/sum(populacao_feminina_10_a_49) * 100, 1)
  ) |>
  ungroup()

# For the cluster analysis ----------------------------------------------------
## Standardizing the data for each of the four cluster analysis
df_cluster <- df_cob_suplementar_19 |> select(cob_suplementar) |> scale()

## Calculating the distance matrices
df_cluster_dist <- dist(df_cluster, method = "euclidean")


## K-means --------------------------------------------------------------------
### Ploting the k-means knee-plot
fviz_nbclust(df_cluster, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo", title = ""
  ) # Candidates: K = 3 and K = 4.

#### Adjusting the k-means method with the chosen numbers of clusters 
set.seed(0408)
d1_kmeans3 <- kmeans(df_cluster, 3)

set.seed(0408)
d1_kmeans4 <- kmeans(df_cluster, 4)


### K-medians
#### Ploting the k-medians knee-plot
fviz_nbclust(df_cluster, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo", title = ""
  ) # Candidates: K = 3 and K = 4.

#### Adjusting the k-medians method with the chosen numbers of clusters 
set.seed(0408)
d1_pam3 <- cluster::pam(df_cluster, k = 3)

set.seed(0408)
d1_pam4 <- cluster::pam(df_cluster, k = 4)


### Ward's method
#### Ploting the drodrograms
set.seed(0408)
d1_ward <- hclust(df_cluster_dist, method = "ward.D2")
plot(as.dendrogram(d1_ward), ylab = "Altura") 
rect.hclust(d1_ward, k = 2, border = 2:5) 
rect.hclust(d1_ward, k = 3, border = 2:5) # Candidates: K = 3 and K = 4.

#### Adjusting the Ward's method with the chosen numbers of clusters 
d1_ward3_class <- cutree(d1_ward, k = 3)
d1_ward4_class <- cutree(d1_ward, k = 4)


### Single linkage
#### Ploting the dendrograms
set.seed(0408)
d1_single <- hclust(df_cluster_dist, method = "single")
plot(as.dendrogram(d1_single), ylab = "Altura") # No candidates. Bad fit.


### Complete linkage
#### Ploting the dendrograms
set.seed(0408)
d1_complete <- hclust(df_cluster_dist, method = "complete")
plot(as.dendrogram(d1_complete), ylab = "Altura") # No candidates. Bad fit.


### Average linkage
#### Ploting the dendrograms
set.seed(0408)
d1_average <- hclust(df_cluster_dist, method = "average")
plot(as.dendrogram(d1_average), ylab = "Altura") # No candidates. Bad fit.


### Choosing the best clustering method
d1_kmeans_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("K-means, k = ", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_kmeans", i, ""))$cluster)$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_kmeans", i, ""))$cluster, d = df_cluster_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = df_cluster_dist, get(paste0("d1_kmeans", i, ""))$cluster))),
  silh_index = unlist(lapply(3:4, function(i) index.S(df_cluster_dist, get(paste0("d1_kmeans", i, ""))$cluster))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_kmeans", i, ""))$cluster))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_kmeans", i, ""))$cluster, d = df_cluster_dist, centrotypes = "medoids")))
)

d1_pam_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("K-medoids, k = ", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_pam", i, ""))$cluster)$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_pam", i, ""))$cluster, d = df_cluster_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = df_cluster_dist, get(paste0("d1_pam", i, ""))$cluster))),
  silh_index = unlist(lapply(3:4, function(i) index.S(df_cluster_dist, get(paste0("d1_pam", i, ""))$cluster))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_pam", i, ""))$cluster))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_pam", i, ""))$cluster, d = df_cluster_dist, centrotypes = "medoids")))
)

d1_ward_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("Ward, k = ", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_ward", i, "", "_class")))$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_ward", i, "", "_class")), d = df_cluster_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = df_cluster_dist, get(paste0("d1_ward", i, "", "_class"))))),
  silh_index = unlist(lapply(3:4, function(i) index.S(df_cluster_dist, get(paste0("d1_ward", i, "", "_class"))))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_ward", i, "", "_class"))))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_ward", i, "", "_class")), d = df_cluster_dist, centrotypes = "medoids")))
)

d1_avaliacao <- rbind(
  d1_pam_index,
  d1_kmeans_index, 
  d1_ward_index
) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))
d1_avaliacao

write.csv(d1_avaliacao, "r_objects/df_avaliacao_cluster_2019.csv", row.names = FALSE)

#### Comments:
##### db_index_cent: the lower the better (ward3, followed by kmeans3 and kmeans4)
##### db_index_med: the lower the better (kmeans3, followed by ward3 and kmeans4)
##### dunn_index: the higher the better (kmeans4, followed by ward3 and ward4)
##### silh_index: the higher the better (kmeans3, followed by ward3 and kmeans4)
##### ch_index_cent: the higher the better (kmeans4, followed by ward4 and pam4)
##### ch_index_med: the higher the better (kmeans4, followed by ward4 and pam4)

#### Choice: K-means with K = 4
d1_kmeans4$size

### Adding the columns with the clusters informations to the original data.frames
df_cob_suplementar_19$cluster <- d1_kmeans4$cluster

ordem_taxa <- df_cob_suplementar_19 |>
  group_by(cluster) |>
  summarise(cob_suplementar = mean(cob_suplementar)) |>
  ungroup() |>
  arrange(cob_suplementar) |>
  pull(cluster)

df_cob_suplementar_19$cluster <- factor(case_when(
  df_cob_suplementar_19$cluster == ordem_taxa[1] ~ "1: Lowest percentage of women with private health insurance",
  df_cob_suplementar_19$cluster == ordem_taxa[2] ~ "2: Low percentage of women with private health insurance",
  df_cob_suplementar_19$cluster == ordem_taxa[3] ~ "3: Intermediate percentage of women with private health insurance",
  df_cob_suplementar_19$cluster == ordem_taxa[4] ~ "4: High percentage of women with private health insurance"
))


## Visualizing the clusters in a map ---------------------------------------
### Downloading the geometry data
df_muni_sf <- read_municipality(year = 2020, cache = TRUE) |>
  mutate(codmunres = as.numeric(substr(code_muni, 1, 6)))

df_ufs_sf <- read_state(year = 2020, cache = TRUE)

### Joining the geometry and the clustering data for each period
df_dados_mapa <- left_join(df_cob_suplementar_19, df_muni_sf) |>
  st_as_sf()

### Plotting the maps
plot_clusters <- ggplot() +
  geom_sf(data = df_dados_mapa, aes(fill = cluster), color = NA) +
  scale_fill_viridis_d(
    name = "Groups",
    option = "D",      
    begin = 0,
    end = 0.9,
    alpha = 0.8,
    direction = 1
  ) +
  labs(title = "2019") +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.2, color = "black") +
  theme_bw() +
  theme(legend.title.position = "top", legend.position = "bottom", legend.byrow = TRUE) +
  guides(fill = guide_legend(nrow = 2))

plot_clusters

### Exporting the plot
saveRDS(plot_clusters, "r_objects/plot_clusters_2019.rds")

### Creating a table with the percentage of municipalities in each region
tabela_regioes <- df_cob_suplementar_19 |>
  left_join(df_cob_suplementar |> select(codmunres, regiao)) |>
  mutate(
    cluster = case_when(
      cluster == "1: Lowest percentage of women with private health insurance" ~ "Group 1",
      cluster == "2: Low percentage of women with private health insurance" ~ "Group 2",
      cluster == "3: Intermediate percentage of women with private health insurance" ~ "Group 3",
      cluster == "4: High percentage of women with private health insurance" ~ "Group 4",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(cluster, regiao) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(regiao) |>
  mutate(porc = sprintf("%.2f%%", n / sum(n) * 100)) |>
  ungroup() |>
  select(cluster, regiao, porc) |>
  pivot_wider(names_from = cluster, values_from = porc, values_fill = "0.00%")

write.csv(tabela_regioes, "r_objects/df_regioes_clusters_2019.csv", row.names = FALSE)

## Comparing the groups -------------------------------------------------------
### Reading a dataframe with additional indicators for the municipalities
df_indicadores <- read.csv("data_download/databases/df_indicadores.csv") |>
  select(!cob_suplementar)

### Joining the indicators with the clustering data
df_cob_suplementar_indicadores <- left_join(df_cob_suplementar_19, df_indicadores)

### Creating a function that creates a table with the summary measures
cria_tabelas <- function(variaveis, df, var_grupos, label_tabela, faixa_etaria, periodo) {
  grupos <- levels(df[[var_grupos]])
  
  tabela <- data.frame()
  
  for (variavel in variaveis) {
    
    # Calculating Kruskal-Wallis p-value
    p_valor <- kruskal.test(
      as.formula(paste(variavel, "~", var_grupos)),
      data = df
    )$p.value
    
    p_valor <- ifelse(
      p_valor < 0.001,
      "< 0.001",
      round(p_valor, 3)
    )
    
    tabela_aux <- data.frame(
      variavel = variavel,
      grupo = grupos,
      n = unlist(lapply(grupos, function(grupo) {df[df[[var_grupos]] == grupo, ] |> nrow()})),
      minimo = round(unlist(lapply(grupos, function(grupo) {df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> min(na.rm = TRUE)})), 3),
      primeiro_qt = round(unlist(lapply(grupos, function(grupo) {df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> quantile(0.25, na.rm = TRUE)})), 3),
      media = round(unlist(lapply(grupos, function(grupo) {df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> mean(na.rm = TRUE)})), 3),
      mediana = round(unlist(lapply( grupos, function(grupo) { df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> median(na.rm = TRUE)})), 3),
      dp = round(unlist(lapply(grupos, function(grupo) { df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> sd(na.rm = TRUE) })), 3),
      terceiro_qt = round(unlist(lapply(grupos, function(grupo) { df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> quantile(0.75, na.rm = TRUE)})), 3),
      maximo = round(unlist(lapply(grupos, function(grupo) { df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> max(na.rm = TRUE)})), 3),
      p_valor_kruskal = c(p_valor, rep("", length(grupos) - 1))
    )
    
    tabela <- rbind(tabela, tabela_aux)
  }
  
  tabela
}

### Creating a function that creates a table with the multiple comparisons
cria_tabelas_testes <- function(
    variaveis,
    df,
    var_grupos
) {
  
  tabela <- data.frame()
  
  for (variavel in variaveis) {
    
    formula <- as.formula(
      glue::glue("{variavel} ~ {var_grupos}")
    )
    
    # Dunn test
    resultados_dunn <- dunn_test(
      data = df,
      formula = formula,
      p.adjust.method = "bonferroni"
    )
    
    # Cohen's d
    efeito_d <- cohens_d(
      data = df,
      formula = formula
    )
    
    # Junta resultados
    tabela_aux <- data.frame(
      variavel = variavel,
      `Compared groups` = paste(
        resultados_dunn$group1,
        "vs",
        resultados_dunn$group2
      ),
      `Dunn's statistic` = round(
        -1 * resultados_dunn$statistic,
        3
      ),
      `Adjusted p-value` = ifelse(
        resultados_dunn$p.adj < 0.001,
        "< 0.001",
        round(resultados_dunn$p.adj, 3)
      ),
      `Cohen's d` = round(
        efeito_d$effsize,
        3
      ),
      check.names = FALSE
    )
    
    tabela <- rbind(
      tabela,
      tabela_aux
    )
  }
  
  tabela
}
nomes_variaveis1 <- c("cob_suplementar")

### Creating the summary tables
tabela_resumo_cob <- cria_tabelas(
  df = df_cob_suplementar_indicadores, var_grupos = "cluster", label_tabela = "1", variaveis = nomes_variaveis1
)
tabela_resumo_cob
write.csv(tabela_resumo_cob, "r_objects/tabela_resumo_cluster_2019.csv", row.names = FALSE)

nomes_variaveis2 <- c("razao_emprego_formal_pop_15_a_64", "idhm", "porc_va_adespss", "porc_populacao_urbana", "porc_beneficiarios_bolsa_familia")
tabela_resumo_indicadores <- cria_tabelas(
  df = df_cob_suplementar_indicadores, var_grupos = "cluster", label_tabela = "1", variaveis = nomes_variaveis2
)
tabela_resumo_indicadores
write.csv(tabela_resumo_indicadores, "r_objects/tabela_resumo_indicadores_2019.csv", row.names = FALSE)

### Creating the multiple comparisions tables
tabela_comparacoes_cob <- cria_tabelas_testes(
  df = df_cob_suplementar_19, var_grupos = "cluster", variaveis = nomes_variaveis1
)
tabela_comparacoes_cob
write.csv(tabela_comparacoes_cob, "r_objects/tabela_comparacoes_cluster_2019.csv", row.names = TRUE)

tabela_comparacoes_indicadores <- cria_tabelas_testes(
  df = df_cob_suplementar_indicadores, var_grupos = "cluster", variaveis = nomes_variaveis2
)
tabela_comparacoes_indicadores
write.csv(tabela_comparacoes_indicadores, "r_objects/tabela_comparacoes_indicadores_2019.csv", row.names = TRUE)
