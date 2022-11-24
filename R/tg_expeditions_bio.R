#' Expeditions biology
#'

#' @param lowercase Boolean, if TRUE, variable names are set to lower case. If FALSE,
#' names will be consitent with documentation.
#'
#' @return A dataframe
#' @export
#' @importFrom rlang .data
#'
#' @examples df <- tg_expeditions_bio()
tg_expeditions_bio <- function(lowercase = FALSE) {
  species <- "mackerel"

  d <- jsonlite::fromJSON(
    curl::curl(
    paste0("http://smartfishsvc.hi.no/api/data/BioRawdataExpeditions/",
           species[1])))%>%
    dplyr::as_tibble() %>%
    dplyr::mutate(CatchDate = lubridate::ymd_hms(.data$CatchDate),
                  species = species[1])
  if(lowercase)
    d <- dplyr::select_all(d, tolower)
  return(d)

}
