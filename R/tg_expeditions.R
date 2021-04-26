#' Expeditions
#'
#' @param species "mackerel" or "herring"
#' @param cn.standardized Boolean, if FALSE (default) retains variable names as
#' delivered by the webserver otherwise mri-standaridzed variable names are used
#' @param lowercase Boolean, if TRUE, variable names are set to lower case. If FALSE,
#' names will be consitent with documentation.
#'
#' @return A dataframe
#' @export
#' @importFrom rlang .data
#'
#' @examples df <- tg_expeditions()
tg_expeditions <- function(species = "mackerel", cn.standardized = FALSE,
                           lowercase = FALSE) {

  if(length(species) > 1) {
    stop(message("Only one species can be specified, 'mackerel' or 'herring'"))
  }

  if(!any(species %in% c("herring", "mackerel"))) {
    stop(message("species has to be either 'mackerel' or 'herring'"))
  }

  d <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/expeditions/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(#when = lubridate::ymd_hms(when),
                  ReleseDate      = lubridate::ymd_hms(.data$ReleseDate),
                  RecaptureDate   = lubridate::ymd_hms(.data$RecaptureDate),
                  Longitude = stringr::str_replace(.data$Longitude, ",", ".") %>% as.numeric(),
                  Latitude = stringr::str_replace(.data$Latitude, ",", ".") %>% as.numeric(),
                  Length = .data$Length * 100,
                  species = species[1])

  if(cn.standardized){
    d <- d %>%
      dplyr::rename(tDate = .data$ReleseDate,
                    rDate = .data$RecaptureDate,
                    tLon = .data$Longitude,
                    tLat = .data$Latitude,
                    tLength = .data$Length)

  }
  if(lowercase)
    d <- dplyr::select_all(d, tolower)
  return(d)
}
