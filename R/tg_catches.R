#' Catches
#'
#' @param species "mackerel" or "herring"
#' @param cn.standardized Boolean, if FALSE (default) retains variable names as
#' delivered by the webserver otherwise mri-standaridzed variable names are used
#'
#' @return A dataframe
#' @export
#'
tg_catches <- function(species = "mackerel", cn.standardized = FALSE) {

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
                  cLon = geo::ir2d(ices_rectangle)$lon,
                  cLat = geo::ir2d(ices_rectangle)$lat,
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
      dplyr::rename(ices = ices_rectangle,
                    year = catchdate,
                    pid = reference_plant,
                    pname = plant_name,
                    pices = plant_ices_rectangle,
                    area = recatch_ices_rectangle,
                    pdate = processing_date) %>%
      return()
  }

}
