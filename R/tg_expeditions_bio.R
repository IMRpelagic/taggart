#' Expeditions biology
#'
#' @param species "mackerel", "herring" or both (default)
#' @return A dataframe
#' @export
#'
tg_expeditions_bio <- function(species = c("mackerel", "herring")) {

  if(!any(species %in% c("herring", "mackerel"))) {
    stop(message("species has to be either 'mackerel' or 'herring' or both"))
  }

  # TODO: use purrr::map

  d1 <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/BiosampleExpeditions/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(catch_date = lubridate::ymd_hms(catch_date),
                  fish = species[1]) %>%
    dplyr::rename(lon = lo,
                  lat = la,
                  maturity = mauturity)

  if(length(species) == 2) {
    d2 <-
      jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/BiosampleExpeditions/", species[2])) %>%
      dplyr::as_tibble() %>%
      dplyr::select_all(tolower) %>%
      dplyr::mutate(catch_date = lubridate::ymd_hms(catch_date),
                    fish = species[2]) %>%
      dplyr::rename(lon = lo,
                    lat = la,
                    maturity = mauturity)
    dplyr::bind_rows(d1, d2) %>% return()
  } else {
    d1 %>% return()
  }
}
