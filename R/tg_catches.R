#' Catches
#'
#' @return A dataframe
#' @export
#'
tg_catches <- function() {
  jsonlite::fromJSON("http://smartfishsvc.hi.no/api/data/Catches/mackerel") %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower)
}
