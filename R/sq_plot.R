sq_plot <- function(d) {

  xlim = range(d$lon)
  ylim = range(d$lat)

  m <- ggplot2::map_data("world",
                         xlim = xlim * c(0.9, 1.1),
                         ylim = ylim * c(0.9, 1.1))

  p <-
    ggplot2::ggplot() +
    ggplot2::geom_polygon(data = m, ggplot2::aes(long, lat, group = group), fill = "grey") +
    ggplot2::geom_raster(data = d, ggplot2::aes(lon, lat, fill = value)) +
    ggplot2::coord_quickmap(xlim = xlim, ylim = ylim)

  return(p)

}
