#' List of factories to exclude for certain periods
#'
#' @param lowercase Boolean, if TRUE, variable names are set to lower case. If FALSE,
#' names will be consitent with documentation.
#'
#' @return A data frame
#' @export
#' @importFrom rlang .data
#'
#' @examples tg_outoforder()
tg_outoforder <- function(lowercase = FALSE) {
  d <-
    jsonlite::fromJSON("http://smartfishsvc.hi.no/api/data/OutOfOrder/") %>%
    dplyr::as_tibble()
  if(lowercase)
    d <- dplyr::select_all(d, tolower)
  return(d)
}