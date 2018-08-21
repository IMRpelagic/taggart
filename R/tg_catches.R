#' Catches
#'
#' @return A dataframe
#' @export
#'
tg_catches <- function() {
  jsonlite::fromJSON("http://smartfishsvc.hi.no/api/data/Catches/mackerel") %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(processing_date = lubridate::ymd_hms(processing_date ),
                  lon = geo::ir2d(ices_rectangle)$lon,
                  lat = geo::ir2d(ices_rectangle)$lat,
                  lonp = geo::ir2d(plant_ices_rectangle)$lon,
                  latp = geo::ir2d(plant_ices_rectangle)$lat)
}
