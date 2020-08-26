tg_tagged_link <- function(species = "mackerel", cn.standardized = FALSE) {


  if(length(species) > 1) {
    stop(message("Only one species can be specified, 'mackerel' or 'herring'"))
  }

  if(!any(species %in% c("herring", "mackerel"))) {
    stop(message("species has to be either 'mackerel' or 'herring'"))
  }

  d <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/BiosamplesExpeditions/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower)

  if(!cn.standardized) {
    return(d)
  } else {
    d %>%
      dplyr::rename(ref_biosample = id) %>%
      return()
  }

}