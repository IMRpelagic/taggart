#' Catches
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
#' @examples df <- tg_catches()
tg_catches <- function(species = "mackerel", cn.standardized = FALSE,
                       lowercase = FALSE) {

  if(length(species) > 1) {
    stop(message("Only one species can be specified, 'mackerel' or 'herring'"))
  }

  if(!any(species %in% c("herring", "mackerel"))) {
    stop(message("species has to be either 'mackerel' or 'herring'"))
  }

  d <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/Catches/", species[1])) %>%
    dplyr::as_tibble() %>%

    dplyr::mutate(ProcessingDate = lubridate::ymd_hms(.data$ProcessingDate),
                  cLon = geo::ir2d(.data$ICES_Rectangle)$lon,
                  cLat = geo::ir2d(.data$ICES_Rectangle)$lat,
                  pLon = geo::ir2d(.data$FactoryICES_Rectangle)$lon,
                  pLat = geo::ir2d(.data$FactoryICES_Rectangle)$lat,
                  nation = dplyr::case_when(.data$Nation == "Eire" ~ "IE",
                                            .data$Nation == "Norway" ~ "NO",
                                            .data$Nation == "Sweden" ~ "SE",
                                            .data$Nation == "GB" ~ "UK",
                                            TRUE ~ .data$Nation),
                  species = species[1])

  if(cn.standardized) {
    d <- d %>%
      dplyr::rename(ices = .data$ICES_Rectangle,
                    year = .data$CatchDate,
                    pid = .data$FactoryID,
                    pname = .data$Factory,
                    pices = .data$FactoryICES_Rectangle,
                    area = .data$ICES_Rectangle,
                    pdate = .data$ProcessingDate)
  }
  if(lowercase)
    d <- dplyr::select_all(d, tolower)
  return(d)

}
