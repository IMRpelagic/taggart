#' Expeditions biology
#'
#' @return A dataframe
#' @export
#'
tg_expeditions_bio <- function() {
  jsonlite::fromJSON("http://smartfishsvc.hi.no/api/data/BiosampleExpeditions/mackerel") %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower)
}
