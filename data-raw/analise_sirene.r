sirene_shp <- sf::read_sf("~/Downloads/result 2/meioambiente-shape.shp")
base <- sirene_shp |>
  dplyr::mutate(numero = abjutils::clean_cnj(NUMPROCESS)) |>
  tibble::as_tibble() |>
  dplyr::select(-geometry)
dplyr::glimpse(base)

trf1 <- dplyr::filter(base, UF %in% amazonia_legal, TRIBUNAL == "TRF1")
trf1 |>
  dplyr::mutate(NOASSUNTOS = tolower(NOASSUNTOS)) |>
  dplyr::count(NOASSUNTOS, sort = TRUE) |>
  dplyr::top_n(20) |>
  dplyr::pull(NOASSUNTOS) |>
  (\(x) paste0("```", stringr::str_pad(1:20, 2, "left", 0), ". ", x, "```"))() |>
  clipr::write_clip()


trf1_edit <- trf1 |>
  dplyr::mutate(NOASSUNTOS = tolower(NOASSUNTOS)) |>
  dplyr::filter(
    stringr::str_detect(
      CLASSE, "Agravo|Cumprimento|Execução|Apelação|Remessa|Recurso", TRUE
    ),
    ANO > 2010,
    stringr::str_detect(NOASSUNTOS, "(flora|patrom[oô]nio gen[eé]tico)")
  )

trf1_edit |>
  dplyr::count(ANO, FLG_JULGAM) |>
  tidyr::pivot_wider(names_from = FLG_JULGAM, values_from = n) |>
  clipr::write_clip()

poss_dwld <- purrr::possibly(lex::trf1_cpopg_download, "NULL")


dwld <- purrr::map_chr(
  trf1_edit$numero,
  poss_dwld, dir = "tmp"
)

trf1_dir <- fs::dir_ls("tmp", recurse = TRUE, type = "file", regexp = "[0-9]\\.html$") |>
  dplyr::as_tibble() |>
  dplyr::mutate(size = fs::file_size(value)) |>
  dplyr::filter(size > 19000) |>
  dplyr::pull(value) |>
  dirname()


poss_parse <- purrr::possibly(lex::trf1_cpopg_parse, tibble::tibble(erro = "erro"))
trf1_parsed <- purrr::map_dfr(trf1_dir, poss_parse)

readr::write_rds(trf1_parsed, "trf1_parsed.rds")


trf1_parsed |>
  dplyr::filter(is.na(erro)) |>
  dplyr::select(processo, partes) |>
  tidyr::unnest(partes) |>
  clipr::write_clip()

fs::dir_ls("tmp", recurse = TRUE, type = "file", regexp = "[0-9]\\.html$") |>
  dplyr::as_tibble() |>
  dplyr::mutate(size = fs::file_size(value)) |>
  dplyr::filter(size <= 19000) |>
  dplyr::pull(value) |>
  dirname() |>
  fs::dir_delete()

# set.seed(234)
# amostra <- base |>
#   dplyr::filter(stringr::str_detect(
#     CLASSE, "Agravo|Cumprimento|Execução|Apelação|Remessa|Recurso", TRUE
#   )) |>
#   dplyr::sample_n(1000)
# clipr::write_clip(amostra)


trf1_edit <- readr::read_csv("~/Downloads/trf1_desmatamento_2010_2022.csv")

trf1_edit |>
  dplyr::filter(stringr::str_detect(CLASSE, stringr::regex("c[ií]v[ie]l", TRUE))) |>
  dplyr::count(ANO, FLG_JULGAM) |>
  tidyr::pivot_wider(names_from = FLG_JULGAM, values_from = n) |>
  tidyr::replace_na(list(`Concluído` = 0)) |>
  janitor::adorn_totals("col") |>
  clipr::write_clip()
