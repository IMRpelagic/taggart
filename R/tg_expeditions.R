#' Expeditions
#'
#' @return A dataframe
#' @export
#'
tg_expeditions <- function() {
  jsonlite::fromJSON("http://smartfishsvc.hi.no/api/data/expeditions/mackerel") %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(when = lubridate::ymd_hms(when),
                  relesedate = lubridate::ymd_hms(relesedate),
                  lon = stringr::str_replace(lo, ",", ".") %>% as.numeric(),
                  lat = stringr::str_replace(la, ",", ".") %>% as.numeric())
}
