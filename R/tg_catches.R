#' Catches
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
#' @examples df <- tg_catches()
tg_catches <- function(cn.standardized = FALSE,
                       lowercase = FALSE,
                       species = "mackerel") {
  d <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/Catches/", species[1])) %>%
    dplyr::as_tibble() %>%

    dplyr::mutate(ProcessingDate = lubridate::ymd_hms(.data$ProcessingDate),
                  cLon = taggart::ir2d(.data$ICES_Rectangle)$lon,
                  cLat = taggart::ir2d(.data$ICES_Rectangle)$lat,
                  pLon = taggart::ir2d(.data$FactoryICES_Rectangle)$lon,
                  pLat = taggart::ir2d(.data$FactoryICES_Rectangle)$lat,
                  # Nation = dplyr::case_when(.data$Nation == "Eire" ~ "IE",
                  #                           .data$Nation == "Norway" ~ "NO",
                  #                           .data$Nation == "Sweden" ~ "SE",
                  #                           .data$Nation == "GB" ~ "UK",
                  #                           TRUE ~ .data$Nation),
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
