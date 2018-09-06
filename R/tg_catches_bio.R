#' Catches biology
#'
#' @return A dataframe
#' @export
#'
tg_catches_bio <- function(species = c("mackerel")) {

  # TODO: use purrr::map

  d1 <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/BioSampleCatches/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(catch_date = lubridate::ymd_hms(catch_date),
                  lon = geo::ir2d(ices_rectangle)$lon,
                  lat = geo::ir2d(ices_rectangle)$lat,
                  fish = species[1]) %>%
    dplyr::rename(maturity = mauturity)

  return(d1)
}
