#' Catches
#'
#' @param species "mackerel" or "herring"
#' @param cn.standardized Boolean, if FALSE (default) retains variable names as
#' delivered by the webserver otherwise mri-standaridzed variable names are used
#'
#' @return A dataframe
#' @export
#'
tg_recaptured <- function(species = "mackerel", cn.standardized = FALSE) {

  if(length(species) > 1) {
    stop(message("Only one species can be specified, 'mackerel' or 'herring'"))
  }

  if(!any(species %in% c("herring", "mackerel"))) {
    stop(message("species has to be either 'mackerel' or 'herring'"))
  }

  d <-
    jsonlite::fromJSON(paste0("http://smartfishsvc.hi.no/api/data/Catches/", species[1])) %>%
    dplyr::as_tibble() %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(processing_date = lubridate::ymd_hms(processing_date ),
                  rLon = geo::ir2d(ices_rectangle)$lon,
                  rLat = geo::ir2d(ices_rectangle)$lat,
                  pLon = geo::ir2d(plant_ices_rectangle)$lon,
                  pLat = geo::ir2d(plant_ices_rectangle)$lat,
                  nation = dplyr::case_when(nation == "Eire" ~ "IE",
                                            nation == "Norway" ~ "NO",
                                            nation == "Sweden" ~ "SE",
                                            nation == "GB" ~ "UK",
                                            TRUE ~ nation),
                  species = species[1])

  if(!cn.standardized) {
    return(d)
  } else {
    d %>%
      dplyr::rename(rRect = ices_rectangle,
                    rYear = catchdate,
                    pid   = reference_plant,
                    pName = plant_name,
                    pArea = plant_ices_rectangle,
                    rArea = recatch_ices_rectangle,
                    pDate = processing_date) %>%
      return()
  }

}
