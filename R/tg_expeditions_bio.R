#' Expeditions biology
#'
#' @param species "mackerel" or "herring"
#' @return A dataframe
#' @export
#'
tg_expeditions_bio <- function(species = "mackerel") {

  if(length(species) > 1) {
    stop(message("Only one species can be specified, 'mackerel' or 'herring'"))
  }

  if(!any(species %in% c("herring", "mackerel"))) {
    stop(message("species has to be either 'mackerel' or 'herring'"))
  }

  jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/BiosampleExpeditions/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(catch_date = lubridate::ymd_hms(catch_date),
                  species = species[1]) %>%
    dplyr::rename(lon = lo,
                  lat = la,
                  maturity = mauturity) %>%
    return()

}
