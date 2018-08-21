#' Expeditions
#'
#' @return A dataframe
#' @export
#'
tg_expeditions <- function() {
  jsonlite::fromJSON("http://smartfishsvc.hi.no/api/data/expeditions/mackerel") %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower)
}
