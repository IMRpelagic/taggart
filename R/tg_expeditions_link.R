#' BiosamplesExpeditions
#'
#' @param cn.standardized Boolean, if FALSE (default) retains variable names as
#' delivered by the webserver otherwise mri-standaridzed variable names are used
#' @param lowercase Boolean, if TRUE, variable names are set to lower case. If FALSE,
#' names will be consitent with documentation.
#'
#' @return A data frame
#' @export
#' @importFrom rlang .data
#'
#' @examples tg_expeditions_link()
tg_expeditions_link <- function(cn.standardized = FALSE,
                                lowercase = FALSE) {
  species <- "mackerel"

  d <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/BiosamplesExpeditions/", species[1])) %>%
    dplyr::as_tibble()

  if(cn.standardized) {
    d<-d %>%
      dplyr::rename(ref_biosample = .data$BioSampleID)
  }
  if(lowercase)
    d <- dplyr::select_all(d, tolower)
  return(d)

}