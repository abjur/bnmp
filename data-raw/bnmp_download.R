# variaveis globais -------------------------------------------------------

# cookie para baixar resultados
# pegar de uma requisicao feita no navegador depois de resolver o recaptcha
# não fechar o navegador, pois expira
hh <- httr::add_headers(
  "cookie" = "portalbnmp=eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJndWVzdF9wb3J0YWxibm1wIiwiYXV0aCI6IlJPTEVfQU5PTllNT1VTIiwiZXhwIjoxNjUxNjc3MjExfQ.DXAVm_t7kRVhLK_sXeZSqh3P1jSkVTvhbqtCJUbjMERv5WMvZkzdcYUQ-_8w_9gdLShdszHyQjzuNHBudHw0CQ"
)


# funcoes -----------------------------------------------------------------

# importante para calcular a quantidade de paginas
n_results <- function(size = 10, idEstado = "", idMunicipio = "") {
  u <- "https://portalbnmp.cnj.jus.br/bnmpportal/api/pesquisa-pecas/filter"
  qq <- list(
    page = 0,
    size = size,
    sort = ""
  )
  body <- list(
    buscaOrgaoRecursivo = "false",
    idEstado = idEstado,
    idMunicipio = idMunicipio
  )
  r <- httr::POST(
    u,
    query = qq,
    body = body,
    encode = "json",
    hh
  )
  ct <- httr::content(r)
  npags <- ct$totalPages
  if (npags > 5) usethis::ui_oops("Mais de 10000 resultados.")
  npags
}

# baixa os municipios de um estado
listar_municipios <- function(idEstado) {
  u_estado <- paste0("https://portalbnmp.cnj.jus.br/scaservice/api/municipios/por-uf/", idEstado)
  r <- httr::GET(u_estado, hh)
  aux <- httr::content(r, simplifyDataFrame = TRUE) |>
    tibble::as_tibble() |>
    tidyr::unnest(uf, names_sep = "_")
  aux
}

# baixa os dados de um municipio+estado
baixar_estado_municipio <- function(idEstado = 1,
                                    idMunicipio = 1,
                                    path,
                                    size = 2000,
                                    .progress = NULL) {

  if (!is.null(.progress)) .progress()

  u <- "https://portalbnmp.cnj.jus.br/bnmpportal/api/pesquisa-pecas/filter"
  body <- list(
    buscaOrgaoRecursivo = "false",
    idEstado = idEstado,
    idMunicipio = idMunicipio
  )
  paginas <- n_results(size, idEstado, idMunicipio)

  for (ii in seq_len(paginas)) {

    f <- sprintf(
      "%s/%02d_%05d_%03d.json",
      path,
      as.numeric(idEstado),
      as.numeric(idMunicipio),ii
    )

    # usethis::ui_info(f)
    if (!file.exists(f)) {
      qq <- list(
        page = as.character(ii - 1),
        size = size,
        sort = ""
      )
      r <- httr::POST(
        u,
        query = qq,
        body = body,
        encode = "json",
        hh,
        httr::write_disk(f, TRUE)
      )
    }
  }
}

# download paginas --------------------------------------------------------

todos_municipios <- purrr::map_dfr(1:27, listar_municipios, .id = "idEstado")
path <- "~/Documents/abj/levantamentos/data-raw/gabriela-sa/bnmp"
fs::dir_create(path)

progressr::with_progress({
  p <- progressr::progressor(nrow(todos_municipios))
  with(
    todos_municipios,
    purrr::walk2(
      idEstado, id,
      baixar_estado_municipio,
      path = path, size = 2000, .progress = p
    )
  )
})


# parse paginas -----------------------------------------------------------

todos_arquivos <- fs::dir_ls(path)

parse_arquivo <- function(arq, .progress = NULL) {
  if (!is.null(.progress)) .progress()
  arq |>
    jsonlite::read_json(simplifyDataFrame = TRUE) |>
    purrr::pluck("content") |>
    tibble::as_tibble()
}

progressr::with_progress({
  p <- progressr::progressor(length(todos_arquivos))
  da_paginas <- purrr::map_dfr(
    todos_arquivos,
    parse_arquivo,
    .progress = p,
    .id = "file"
  )
})

354380 - 325746

readr::write_rds(
  da_paginas,
  "~/Documents/abj/levantamentos/data-raw/gabriela-sa/da_paginas.rds",
  compress = "xz"
)


# download processos ------------------------------------------------------

id <- as.character(da_paginas$id[1])
id_tipo <- as.character(da_paginas$idTipoPeca[1])

baixar_processo <- function(id, id_tipo, path, .progress = NULL) {
  if (!is.null(.progress)) .progress()
  f <- stringr::str_glue("{path}/{id}_{id_tipo}.json")
  if (!file.exists(f)) {
    u <- stringr::str_glue(
      "https://portalbnmp.cnj.jus.br/",
      "bnmpportal/api/certidaos/{id}/{id_tipo}"
    )
    r <- httr::GET(u, hh, httr::write_disk(f, TRUE))
  }
  f
}

ids_baixar <- da_paginas |>
  dplyr::transmute(
    id = as.character(id),
    idTipoPeca = as.character(idTipoPeca)
  ) |>
  dplyr::distinct()

path <- "~/Documents/abj/levantamentos/data-raw/gabriela-sa/bnmp_processos"
fs::dir_create(path)

progressr::with_progress({
  p <- progressr::progressor(nrow(ids_baixar))
  purrr::walk2(
    ids_baixar$id,
    ids_baixar$idTipoPeca,
    baixar_processo,
    path = path,
    .progress = p
  )
})

# parse processos ---------------------------------------------------------

parse_processo <- function(json) {
  json |>
    jsonlite::read_json(simplifyDataFrame = TRUE) |>
    unlist() |>
    tibble::enframe() |>
    tidyr::pivot_wider() |>
    dplyr::mutate(file = json)
}

processos <- "~/Downloads/bnmp_processos" |>
  fs::dir_ls() |>
  purrr::discard(~fs::file_size(.x) < 100)
parsed_processos <- purrr::map_dfr(processos, parse_processo)

dplyr::glimpse(parsed_processos)
names(parsed_processos)

parsed_processos |>
  dplyr::select(
    -dplyr::matches("pessoa\\.(sinais|telefone|endereco)"),
    -dplyr::starts_with("outrasPecas")
  ) |>
  dplyr::glimpse()

parsed_processos |>
  dplyr::select(-dplyr::contains("pessoa")) |>
  names()


