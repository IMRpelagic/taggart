#' Catches biology
#'
#' @return A dataframe
#' @export
#'
tg_catches_bio <- function() {
  jsonlite::fromJSON("http://smartfishsvc.hi.no/api/data/BioSampleCatches/mackerel") %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(catch_date = lubridate::ymd_hms(catch_date),
                  lon = geo::ir2d(ices_rectangle)$lon,
                  lat = geo::ir2d(ices_rectangle)$lat) %>%
    dplyr::rename(maturity = mauturity)
}
