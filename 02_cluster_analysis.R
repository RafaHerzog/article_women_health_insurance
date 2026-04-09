# Loading the necessary libraries
library(knitr)
library(dplyr)
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
df_indicadores <- read.csv("databases/df_beneficiarias_10_a_49.csv")

## Filtering and calculating the fertility rates for each period
df_indicadores_24 <- df_indicadores |>
  filter(ano %in% c(2024)) |>
  group_by(codmunres) |>
  summarise(
    porc_dependentes_sus = round((sum(populacao_feminina_10_a_49) - sum(beneficiarias_10_a_49))/sum(populacao_feminina_10_a_49) * 100, 1)
  ) |>
  ungroup()

## Removing auxiliary objects
rm(df_indicadores)

# For the cluster analysis ----------------------------------------------------
## Standardizing the data for each of the four cluster analysis
df_cluster <- df_indicadores_24 |> select(porc_dependentes_sus) |> scale()

## Calculating the distance matrices
df_cluster_dist <- dist(df_cluster, method = "euclidean")


## K-means --------------------------------------------------------------------
### Ploting the k-means knee-plots
fviz_nbclust(df_cluster, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo", title = ""
  ) # Candidates: K = 3 and K = 4.

#### Adjusting the k-means method with the chosen numbers of clusters 
set.seed(1504)
d1_kmeans3 <- kmeans(df_cluster, 3)

set.seed(1504)
d1_kmeans4 <- kmeans(df_cluster, 4)


### K-medians
#### Ploting the k-medians knee-plots
fviz_nbclust(df_cluster, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo", title = ""
  ) # Candidates: K = 3 and K = 4.

#### Adjusting the k-medians method with the chosen numbers of clusters 
set.seed(1504)
d1_pam3 <- cluster::pam(df_cluster, k = 3)

set.seed(1504)
d1_pam4 <- cluster::pam(df_cluster, k = 4)


### Ward's method
#### Ploting the drodrograms
set.seed(1504)
d1_ward <- hclust(df_cluster_dist, method = "ward.D2")
plot(as.dendrogram(d1_ward), ylab = "Altura") 
rect.hclust(d1_ward, k = 2, border = 2:5) 
rect.hclust(d1_ward, k = 3, border = 2:5) # Candidates: K = 2 and K = 3.

#### Adjusting the Ward's method with the chosen numbers of clusters 
d1_ward2_class <- cutree(d1_ward, k = 2)
d1_ward3_class <- cutree(d1_ward, k = 3)


### Single linkage
#### Ploting the dendrograms
set.seed(1504)
d1_single <- hclust(df_cluster_dist, method = "single")
plot(as.dendrogram(d1_single), ylab = "Altura") # No candidates. Bad fit.


### Complete linkage
#### Ploting the dendrograms
set.seed(1504)
d1_complete <- hclust(df_cluster_dist, method = "complete")
plot(as.dendrogram(d1_complete), ylab = "Altura") # No candidates. Bad fit.


### Average linkage
#### Ploting the dendrograms
set.seed(1504)
d1_average <- hclust(df_cluster_dist, method = "average")
plot(as.dendrogram(d1_average), ylab = "Altura") # No candidates. Bad fit.


### Choosing the best clustering method
#### For the 2018-2019 period 
d1_kmeans_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_kmeans", i, ""))$cluster)$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_kmeans", i, ""))$cluster, d = df_cluster_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = df_cluster_dist, get(paste0("d1_kmeans", i, ""))$cluster))),
  silh_index = unlist(lapply(3:4, function(i) index.S(df_cluster_dist, get(paste0("d1_kmeans", i, ""))$cluster))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_kmeans", i, ""))$cluster))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_kmeans", i, ""))$cluster, d = df_cluster_dist, centrotypes = "medoids")))
)

d1_pam_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_pam", i, ""))$cluster)$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(df_cluster, get(paste0("d1_pam", i, ""))$cluster, d = df_cluster_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = df_cluster_dist, get(paste0("d1_pam", i, ""))$cluster))),
  silh_index = unlist(lapply(3:4, function(i) index.S(df_cluster_dist, get(paste0("d1_pam", i, ""))$cluster))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_pam", i, ""))$cluster))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(df_cluster, get(paste0("d1_pam", i, ""))$cluster, d = df_cluster_dist, centrotypes = "medoids")))
)

d1_ward_index <- data.frame(
  metodo = unlist(lapply(2:3, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(2:3, function(i) index.DB(df_cluster, get(paste0("d1_ward", i, "", "_class")))$DB)),
  db_index_med = unlist(lapply(2:3, function(i) index.DB(df_cluster, get(paste0("d1_ward", i, "", "_class")), d = df_cluster_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:3, function(i) dunn(distance = df_cluster_dist, get(paste0("d1_ward", i, "", "_class"))))),
  silh_index = unlist(lapply(2:3, function(i) index.S(df_cluster_dist, get(paste0("d1_ward", i, "", "_class"))))),
  ch_index_cent = unlist(lapply(2:3, function(i) index.G1(df_cluster, get(paste0("d1_ward", i, "", "_class"))))),
  ch_index_med = unlist(lapply(2:3, function(i) index.G1(df_cluster, get(paste0("d1_ward", i, "", "_class")), d = df_cluster_dist, centrotypes = "medoids")))
)

d1_avaliacao <- rbind(
  d1_pam_index,
  d1_kmeans_index, 
  d1_ward_index
)
d1_avaliacao
#### Comments:
##### db_index_cent: the lower the better (kmeans4, followed by pam4)
##### db_index_med: the lower the better (kmeans3, followed by pam3 and kmeans4)
##### dunn_index: the higher the better (kmeans4, followed by ward3 and kmeans3)
##### silh_index: the higher the better (ward2, followed by pam4, kmeans4 and kmeans3)
##### ch_index_cent: the higher the better (kmeans4, followed by kmeans3)
##### ch_index_med: the higher the better (kmeans4, followed by kmeans3)

#### Choice: despite the better perfomance of k-means with K = 4, one of its clusters contained only a few municipalities (127).
d1_kmeans4$size
d1_kmeans3$size
#### Thus, the chosen method is the k-means with K = 3.

### Adding the columns with the clusters informations to the original data.frames
#### For the 2018-2019 period
df_indicadores_24$cluster_10_a_14 <- d1_kmeans3$cluster

ordem_taxa1 <- df_indicadores_24 |>
  group_by(cluster_10_a_14) |>
  summarise(tx_fecundidade_10_a_14 = mean(tx_fecundidade_10_a_14)) |>
  ungroup() |>
  arrange(tx_fecundidade_10_a_14) |>
  pull(cluster_10_a_14)

df_indicadores_24$cluster_10_a_14 <- factor(case_when(
  df_indicadores_24$cluster_10_a_14 == ordem_taxa1[1] ~ "1: lowest fertility rates",
  df_indicadores_24$cluster_10_a_14 == ordem_taxa1[2] ~ "2: intermediary fertility rates",
  df_indicadores_24$cluster_10_a_14 == ordem_taxa1[3] ~ "3: highest fertility rates"
))


## Visualizing the clusters in a map ---------------------------------------
### Downloading the geometry data
df_muni_sf <- read_municipality(year = 2019, showProgress = FALSE) |>
  mutate(codmunres = as.numeric(substr(code_muni, 1, 6)))

df_ufs_sf <- read_state(year = 2019, showProgress = FALSE)

### Joining the geometry and the clustering data for each period
df_dados_mapa <- left_join(df_indicadores_24, df_muni_sf) |>
  mutate(periodo = "2018-2019") 

df_dados_mapa_20_21 <- left_join(df_indicadores_20_21, df_muni_sf) |>
  mutate(periodo = "2020-2021") 

df_dados_mapa_completo <- full_join(df_dados_mapa, df_dados_mapa_20_21) |>
  st_as_sf()

### Plotting the maps
#### For the 10 to 14 age group
plot_clusters_10_a_14 <- ggplot() +
  geom_sf(data = df_dados_mapa_completo, aes(fill = cluster_10_a_14), color = NA) +
  facet_wrap(vars(periodo)) +
  scale_fill_viridis_d(name = "Groups", end = 0.8, alpha = 0.6, direction = -1) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw() +
  theme(legend.position = "bottom") 

##### Exporting the plot
ggsave(
  "figures/Fig2.tiff", 
  plot_clusters_10_a_14,
  width = 7.5, height = 6, units = "in", 
  dpi = 600, compression = "lzw"
)

#### For the 15 to 19 age group
plot_clusters_15_a_19 <- ggplot() +
  geom_sf(data = df_dados_mapa_completo, aes(fill = cluster_15_a_19), color = NA) +
  facet_wrap(vars(periodo)) +
  scale_fill_viridis_d(name = "Groups", end = 0.8, alpha = 0.6, direction = -1) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw() +
  theme(legend.position = "bottom")

##### Exporting the plot
ggsave(
  "figures/Fig3.tiff", 
  plot_clusters_15_a_19,
  width = 7.5, height = 6, units = "in", 
  dpi = 600, compression = "lzw"
)


## Comparing the groups -------------------------------------------------------
### Creating a function that creates a table with the summary measures 
cria_tabelas <- function(variaveis, df, var_grupos, label_tabela, faixa_etaria, periodo) {
  grupos <- levels(df[[var_grupos]])
  
  for (variavel in variaveis) {
    tabela <- kable(
      data.frame(
        grupo = grupos,
        n = unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> nrow())),
        minimo = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> min(na.rm = TRUE))), 2),
        primeiro_qt = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> quantile(0.25, na.rm = TRUE))), 2),
        media = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> mean(na.rm = TRUE))), 2),
        mediana = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> median(na.rm = TRUE))), 2),
        dp = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> sd(na.rm = TRUE))), 2),
        terceiro_qt = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> quantile(0.75, na.rm = TRUE))), 2),
        maximo = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> max(na.rm = TRUE))), 2)
      ),
      align = "cccccccc",
      col.names = c("Grupo (cluster)", "n", "Mín.", "1º Quartil", "Média", "Mediana", "D.P.", "3º Quartil", "Máx."),
      caption = HTML(ifelse(variavel == variaveis[1], paste0(glue::glue("Medidas resumo das variáveis de interesse para os grupos de municípios agrupados pela taxa de fecundidade de mulheres {faixa_etaria} (por mil) no período de {periodo}. <br><br>"), variavel), variavel)),
      label = ifelse(variavel == variaveis[1], paste0("tabela", label_tabela), NA)
    )
    
    print(tabela)
  }
}

### Creating a function that creates a table with the multiple comparisions
cria_tabelas_testes <- function(variaveis, df, var_grupos, label_tabela) {
  for (variavel in variaveis) {
    
    formula <- as.formula(glue::glue("{variavel} ~ {var_grupos}"))
    resultados_dunn <- dunn_test(data = df, formula = formula, p.adjust.method = "bonferroni")
    
    # Calcula a medida de efeito d de Cohen
    efeito_d <- cohens_d(data = df, formula = formula)
    
    monta_linhas <- function(num_grupo) {
      paste(
        -1 * round(resultados_dunn$statistic[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3),
        "(Z) <br>",
        ifelse(
          round(resultados_dunn$p.adj[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3) < 0.05,
          ifelse(
            round(resultados_dunn$p.adj[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3) == 0,
            "< 0.001*",
            paste0(round(resultados_dunn$p.adj[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3), "*")
          ),
          round(resultados_dunn$p.adj[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3)
        ),
        "(valor-p) <br>",
        round(efeito_d$effsize[efeito_d$group1 == unique(efeito_d$group1)[num_grupo]], 3),
        "(D de Cohen)"
      )
    }
    
    df_teste <- data.frame(
      grupo1 = monta_linhas(1),
      grupo2 = c(rep("", 1), monta_linhas(2)),
      row.names = unique(resultados_dunn$group2)
    )
    
    colnames(df_teste) <- unique(resultados_dunn$group1)
    
    tabela <- kable(
      df_teste,
      align = c("cc"),
      caption = HTML(ifelse(variavel == variaveis[1], paste0(glue::glue("Resultados dos testes de Dunn (com correção de Bonferroni) para as comparações múltiplas entre os pares de grupos de municípios. <br><br>"), variavel), variavel)),
      label = ifelse(variavel == variaveis[1], paste0("tabela-comparacoes-", label_tabela), NA)
    )
    
    print(tabela)
  }
}
nomes_variaveis1 <- c("idhm", "cobertura_ab", "tx_fecundidade_10_a_14")

### Creating the summary tables
cria_tabelas(
  df = df_indicadores_24, var_grupos = "cluster", label_tabela = "1", variaveis = nomes_variaveis1
)

### Creating the multiple comparisions tables
cria_tabelas_testes(
  df = df_indicadores_24, var_grupos = "cluster", label_tabela = "1", variaveis = nomes_variaveis1
)