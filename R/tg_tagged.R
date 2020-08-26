#' Expeditions
#'
#' @param species "mackerel" or "herring"
#' @param cn.standardized Boolean, if FALSE (default) retains variable names as
#' delivered by the webserver otherwise mri-standaridzed variable names are used
#' @return A dataframe
#' @export
#'
tg_tagged <- function(species = "mackerel", cn.standardized = FALSE) {

  if(length(species) > 1) {
    stop(message("Only one species can be specified, 'mackerel' or 'herring'"))
  }

  if(!any(species %in% c("herring", "mackerel"))) {
    stop(message("species has to be either 'mackerel' or 'herring'"))
  }

  d <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/expeditions/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(when = lubridate::ymd_hms(when),
                  relesedate = lubridate::ymd_hms(relesedate),
                  tYear = lubridate::year(relesedate),
                  recapturedate = lubridate::ymd_hms(recapturedate),
                  rYear = lubridate::year(recapturedate),
                  lo = stringr::str_replace(lo, ",", ".") %>% as.numeric(),
                  la = stringr::str_replace(la, ",", ".") %>% as.numeric(),
                  length = length * 100)

  if(!cn.standardized) {
    return(d)
  } else {
    d %>%
      dplyr::rename(tDate = relesedate,
                    rDate = recapturedate,
                    rRect = ices,
                    birds = release_birds,
                    waves = release_waves,
                    mortality = release_mortality,
                    pid = plantid,
                    species = fish,
                    tLon = lo,
                    tLat = la,
                    tLength = length) %>%
      return()
  }
}
