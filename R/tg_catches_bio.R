#' Catches biology
#'
#' @return A dataframe
#' @export
#'
tg_catches_bio <- function() {
  jsonlite::fromJSON("http://smartfishsvc.hi.no/api/data/BioSampleCatches/mackerel") %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower)
}
