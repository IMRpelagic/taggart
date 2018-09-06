#' Expeditions
#'
#' @param species "mackerel", "herring" or both (default)
#' @return A dataframe
#' @export
#'
tg_expeditions <- function(species = c("mackerel", "herring")) {

  if(!any(species %in% c("herring", "mackerel"))) {
    stop(message("species has to be either 'mackerel' or 'herring' or both"))
  }

  # TODO: use purrr::map

  d1 <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/expeditions/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(when = lubridate::ymd_hms(when),
                  relesedate = lubridate::ymd_hms(relesedate),
                  lon = stringr::str_replace(lo, ",", ".") %>% as.numeric(),
                  lat = stringr::str_replace(la, ",", ".") %>% as.numeric())

  if(length(species) == 2) {
    d2 <-
      jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/expeditions/", species[2])) %>%
      dplyr::as_tibble() %>%
      dplyr::select_all(tolower) %>%
      dplyr::mutate(when = lubridate::ymd_hms(when),
                    relesedate = lubridate::ymd_hms(relesedate),
                    lon = stringr::str_replace(lo, ",", ".") %>% as.numeric(),
                    lat = stringr::str_replace(la, ",", ".") %>% as.numeric())
    dplyr::bind_rows(d1, d2) %>% return()
  } else {
    d1 %>% return()
  }
}
