#' Catches biology
#'
#' @param species "mackerel" or "herring"
#' @param cn.standardized Boolean, if FALSE (default) retains variable names as
#' delivered by the webserver otherwise mri-standaridzed variable names are used
#'
#' @return A dataframe
#' @export
#'
tg_catches_bio <- function(species = "mackerel", cn.standardized = FALSE) {

  if(length(species) > 1) {
    stop(message("Only one species can be specified, 'mackerel' or 'herring'"))
  }

  if(!any(species %in% c("herring", "mackerel"))) {
    stop(message("species has to be either 'mackerel' or 'herring'"))
  }

  d <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/BioRawdataCatches/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(catch_date = lubridate::ymd_hms(catch_date),
                  lon = geo::ir2d(ices_rectangle)$lon,
                  lat = geo::ir2d(ices_rectangle)$lat,
                  species = species[1]) %>%
    dplyr::rename(maturity = mauturity)

  if(!cn.standardized) {
    return(d)
  } else {
    return(d)
  }
}
