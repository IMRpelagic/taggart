#' Expeditions
#'
#' @param cn.standardized Boolean, if FALSE (default) retains variable names as
#' delivered by the webserver otherwise mri-standaridzed variable names are used
#' @param lowercase Boolean, if TRUE, variable names are set to lower case. If FALSE,
#' names will be consitent with documentation.
#' @param species character, either "mackerel" or "herring"
#'
#' @return A dataframe
#' @export
#' @importFrom rlang .data
#'
#' @examples df <- tg_expeditions()
tg_expeditions <- function(cn.standardized = FALSE,
                           lowercase = FALSE,
                           species = "mackerel") {

  d <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/expeditions/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(#when = lubridate::ymd_hms(when),
                  ReleaseDate      = lubridate::ymd_hms(.data$ReleaseDate),
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
