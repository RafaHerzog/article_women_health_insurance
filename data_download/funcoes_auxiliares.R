# Criando a função que baixa os dados de estimativas populacionais por município, idade e sexo a partir de um endereço FTP
est_pop_tabnet <- function(periodo = 12:25, filtro_sexo = 2, idade_min = 10, idade_max = 49, temp_dir = "data_download/databases/dados_populacao") {
  library(dplyr)
  library(foreign)
  library(RCurl)
  library(utils)
  library(stringr)
  
  # Criar um vetor com os anos de interesse
  anos <- periodo
  
  # URL base
  diretorio <- "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/"
  
  # Criar diretório temporário
  temp_dir <- temp_dir
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Lista para armazenar os dataframes
  lista_dfs <- list()
  
  for (ano in anos) {
    # Nome do arquivo
    arquivo_zip <- paste0("POPSBR", ano, ".zip")
    arquivo_dbf <- paste0("pop", ano, ".dbf")
    
    # Caminho completo do arquivo ZIP
    url_zip <- paste0(diretorio, arquivo_zip)
    
    # Nome temporário para salvar o ZIP
    temp_zip <- tempfile(fileext = ".zip")
    
    # Baixar o arquivo ZIP
    download.file(url_zip, temp_zip, mode = "wb")
    
    # Descompactar o ZIP
    unzip(temp_zip, exdir = temp_dir)
    
    # Ler o arquivo DBF
    caminho_dbf <- file.path(temp_dir, arquivo_dbf)
    df <- read.dbf(caminho_dbf, as.is = TRUE) |>
      janitor::clean_names()
    
    # print(colnames(df))
    
    # Transformar COD_MUN em caracter e renomeá-la
    df <- df |>
      rename(codmunres = cod_mun, ano = ano) |>
      mutate(
        codmunres = as.numeric(codmunres),
        codmunres = substr(codmunres, 1, 6)
      )
    
    # Filtrar SEXO e IDADE
    df <- df |>
      mutate(
        idade = as.numeric(as.character(idade)),
        sexo = as.numeric(sexo)
      ) |>
      filter(sexo %in% as.numeric(filtro_sexo), idade >= idade_min, idade <= idade_max)
    
    # Agrupar e somar POP
    df_resumo <- df |>
      group_by(codmunres, ano) |>
      summarise(!!glue::glue("populacao_feminina_{idade_min}_a_{idade_max}") := sum(pop, na.rm = TRUE), .groups = "drop")
    
    # Adicionar o dataframe à lista
    lista_dfs[[ano]] <- df_resumo
  }
  
  # Juntar todos os dataframes em um só
  df_final <- bind_rows(lista_dfs) |>
    arrange(codmunres, ano)
  
  # Apagar a pasta temporária
  unlink(temp_dir, recursive = TRUE)
  
  return(df_final)
}

# Criando a função que utiliza web scrapping para baixar dados de usuárias de planos de saúde do Tabnet ANS
pop_com_plano_saude_tabnet <- function (linha = "Município",
                                        coluna = "Competência",
                                        conteudo = "Assistência Médica",
                                        periodo = 2012:2021,
                                        sexo = "Feminino",
                                        faixa_etaria = c(
                                          "10 a 14 anos",
                                          "15 a 19 anos",
                                          "20 a 24 anos",
                                          "25 a 29 anos",
                                          "30 a 34 anos",
                                          "35 a 39 anos",
                                          "40 a 44 anos",
                                          "45 a 49 anos"
                                        ),
                                        faixa_etaria_reajuste = "Todas as categorias",
                                        tipo_de_contratacao = "Todas as categorias",
                                        epoca_de_contratacao = "Todas as categorias",
                                        segmentacao = "Todas as categorias",
                                        segmentacao_grupo = c(
                                          "Ambulatorial", "Hospitalar", "Hospitalar e Ambulatorial",
                                          "Referência", "Informado incorretamente", "Não Informado"
                                        ),
                                        uf = "Todas as categorias",
                                        regiao = "Todas as categorias",
                                        capital = "Todas as categorias",
                                        reg_metropolitana = "Todas as categorias",
                                        microrregiao = "Todas as categorias",
                                        municipio = "Todas as categorias"
)

{
  
  page <- xml2::read_html("http://www.ans.gov.br/anstabnet/cgi-bin/dh?dados/tabnet_02.def")
  
  linha.df <- data.frame(
    id = page |>
      rvest::html_elements("#L option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#L option") |>
      rvest::html_attr("value")
  )
  
  coluna.df <- data.frame(
    id = page |>
      rvest::html_elements("#C option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#C option") |>
      rvest::html_attr("value")
  )
  
  conteudo.df <- data.frame(
    id = page |>
      rvest::html_elements("#I option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#I option") |>
      rvest::html_attr("value")
  )
  
  periodo.df <- data.frame(
    id = page |>
      rvest::html_elements("#A option") |>
      rvest::html_text() |>
      substr(start = 1, stop = 8),
    value = page |>
      rvest::html_elements("#A option") |>
      rvest::html_attr("value")
  )
  
  sexo.df <- data.frame(
    id = page |>
      rvest::html_elements("#S1 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S1 option") |>
      rvest::html_attr("value")
  )
  
  faixa_etaria.df <- data.frame(
    id = page |>
      rvest::html_elements("#S2 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S2 option") |>
      rvest::html_attr("value")
  )
  
  faixa_etaria_reajuste.df <- data.frame(
    id = page |>
      rvest::html_elements("#S3 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S3 option") |>
      rvest::html_attr("value")
  )
  
  tipo_de_contratacao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S4 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S4 option") |>
      rvest::html_attr("value")
  )
  
  epoca_de_contratacao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S5 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S5 option") |>
      rvest::html_attr("value")
  )
  
  segmentacao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S6 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S6 option") |>
      rvest::html_attr("value")
  )
  
  segmentacao_grupo.df <- data.frame(
    id = page |>
      rvest::html_elements("#S7 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S7 option") |>
      rvest::html_attr("value")
  )
  
  uf.df <- data.frame(
    id = page |>
      rvest::html_elements("#S8 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S8 option") |>
      rvest::html_attr("value")
  )
  
  regiao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S9 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S9 option") |>
      rvest::html_attr("value")
  )
  
  capital.df <- data.frame(
    id = page |>
      rvest::html_elements("#S10 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S10 option") |>
      rvest::html_attr("value")
  )
  
  reg_metropolitana.df <- data.frame(
    id = page |>
      rvest::html_elements("#S11 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S11 option") |>
      rvest::html_attr("value")
  )
  
  microrregiao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S12 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S12 option") |>
      rvest::html_attr("value")
  )
  
  municipio.df <- data.frame(
    id = page |>
      rvest::html_elements("#S13 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S13 option") |>
      rvest::html_attr("value")
  )
  
  if (is.numeric(periodo)) {
    periodo <- periodo.df |>
      dplyr::filter(
        substr(id, start = 5, stop = 8) %in% as.character(periodo)
      ) |>
      dplyr::pull(id)
  }
  
  argumentos <- c(
    "linha",
    "coluna",
    "conteudo",
    "periodo",
    "sexo",
    "faixa_etaria",
    "faixa_etaria_reajuste",
    "tipo_de_contratacao",
    "epoca_de_contratacao",
    "segmentacao",
    "segmentacao_grupo",
    "uf",
    "regiao",
    "capital",
    "reg_metropolitana",
    "microrregiao",
    "municipio"
  )
  
  invisible(lapply(argumentos, function(argumento) {
    if (!(all(get(argumento) %in% get(glue::glue("{argumento}.df"))$id))) {
      stop(glue::glue("Some element in the '{argumento}' argument is wrong"))
    }
    if (argumento == "periodo") {
      if (length(periodo) > 1 & !(linha == "Competência" | coluna == "Competência")) {
        stop("When more than one period is specified, either the 'linha' or the 'coluna' argument must be equal to 'Competência'")
      }
    }
  }))
  
  argumentos.df <- data.frame(
    argumento = argumentos,
    name =   page |>
      rvest::html_elements("select") |>
      rvest::html_attr("name") |>
      stringi::stri_escape_unicode()
  )
  
  for(argumento in argumentos.df$argumento) {
    assign(
      glue::glue("{argumento}.value"),
      get(glue::glue("{argumento}.df")) |>
        dplyr::filter(id %in% get(argumento)) |>
        dplyr::pull(value)
    )
    
    name <- argumentos.df$name[argumentos.df$argumento == argumento]
    
    assign(
      glue::glue("form_{argumento}"),
      paste0(name, "=",  stringi::stri_escape_unicode(get(glue::glue("{argumento}.value"))), collapse = "&")
    )
  }
  
  form_data <- paste(
    form_linha,
    form_coluna,
    form_conteudo,
    form_periodo,
    form_sexo,
    form_faixa_etaria,
    form_faixa_etaria_reajuste,
    form_tipo_de_contratacao,
    form_epoca_de_contratacao,
    form_segmentacao,
    form_segmentacao_grupo,
    form_uf,
    form_regiao,
    form_capital,
    form_reg_metropolitana,
    form_microrregiao,
    form_municipio,
    "formato=table&mostre=Mostra",
    sep = "&"
  )
  
  form_data <- gsub("\\\\u00", "%", form_data)
  
  #form_data <- "Linha=Munic%edpio&Coluna=--N%E3o-Ativa--&Incremento=Assist%EAncia_M%E9dica&Arquivos=tb_bb_2306.dbf&SSexo=TODAS_AS_CATEGORIAS__&SFaixa_et%E1ria=TODAS_AS_CATEGORIAS__&SFaixa_et%E1ria-Reajuste=TODAS_AS_CATEGORIAS__&STipo_de_contrata%E7%E3o=TODAS_AS_CATEGORIAS__&S%C9poca_de_Contrata%E7%E3o=TODAS_AS_CATEGORIAS__&SSegmenta%E7%E3o=TODAS_AS_CATEGORIAS__&SSegmenta%E7%E3o_grupo=TODAS_AS_CATEGORIAS__&SUF=TODAS_AS_CATEGORIAS__&SGrande_Regi%E3o=TODAS_AS_CATEGORIAS__&SCapital=TODAS_AS_CATEGORIAS__&SReg._Metropolitana=TODAS_AS_CATEGORIAS__&SMicrorregi%E3o=TODAS_AS_CATEGORIAS__&SMunic%EDpio=TODAS_AS_CATEGORIAS__&formato=table&mostre=Mostra"
  
  site <- httr::POST(url = "http://www.ans.gov.br/anstabnet/cgi-bin/tabnet?dados/tabnet_02.def",
                     body = form_data)
  
  tabdados_col1 <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements("tr th") |> rvest::html_text() |>
    trimws()
  tabdados_col1 <- tabdados_col1[-c(1:(which(tabdados_col1 == "TOTAL") - 1))]
  
  tabdados_outras_cols <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements("center td") |> rvest::html_text() |>
    trimws()
  
  col_tabdados <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements("tr:nth-child(1) th") |> rvest::html_text() |> trimws()
  
  f1 <- function(x) x <- gsub("\\.", "", x)
  f2 <- function(x) x <- as.numeric(as.character(x))
  
  tabela_final <- as.data.frame(
    cbind(
      tabdados_col1,
      matrix(tabdados_outras_cols, ncol = length(tabdados_outras_cols)/length(tabdados_col1), byrow = TRUE)
    )
  )
  
  names(tabela_final) <- col_tabdados
  tabela_final[-1] <- lapply(tabela_final[-1], f1)
  tabela_final[-1] <- suppressWarnings(lapply(tabela_final[-1], f2))
  
  if (linha == "Município") {
    tabela_final <- tabela_final[-1, ] |>
      dplyr::mutate(
        codmunres = as.numeric(stringr::str_extract(Município, "\\d+")),
        municipio = stringr::str_replace(Município, "\\d+ ", ""),
        .before = "Município",
        .keep = "unused"
      )
  } else {
    tabela_final <- tabela_final[-1, ]
  }
  
}
