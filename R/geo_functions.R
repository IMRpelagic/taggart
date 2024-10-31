#' Convert between Geographic Coordinates and ICES Rectangles (copied from geo R package)
#'
#' This function converts geographic coordinates to ICES North Atlantic
#' statistical rectangles and rectangle codes to rectangle center coordinates.
#'
#' The default \code{useI=FALSE} is in accordance with the prescription in ICES CM77/Gen:3, but \code{useI=TRUE} has been done on occasion.
#'
#' @aliases d2ir ir2d
#' @param lat vector of latitudes, or a list containing \code{lat} and
#' \code{lon}.
#' @param lon vector of longitudes (ignored if \code{lat} is a list).
#' @param useI whether to use the letter \sQuote{\samp{I}} in statistical
#' rectangle codes.
#' @param ir ICES rectangle code, e.g. \samp{37F3}
#' @return Vector of strings containing ICES statistical rectangle codes or conversely, center coordinates of rectangles which codes were given as input.
#' @examples
#'
#' d2ir(54.25, 3.5)
#' d2ir(c(50,60), c(-20,-10))
#'
#' ir2d(d2ir(54, 3))
#' ## center positions for bottom left and approx top right rects
#' ir2d("01A0")
#' ir2d("99M7")
#' ir2d(c("01A0","99M7"))
#' ## note that ICES CM1977/Gen:3 indicates half-size rects on eastern margin!
#'
#' @export d2ir
d2ir <-
  function(lat, lon = NULL, useI = FALSE)
  {
    if(is.null(lon)) {
      lon <- lat$lon
      lat <- lat$lat
    }
    lat <- lat + 1e-06
    lon <- lon + 1e-06
    outside <- lat < 36 | lat >= 85.5 | lon <= -44 | lon > 68.5
    if(any(outside)) warning("Positions outside of ICES statistical area")
    lat <- floor(lat * 2) - 71
    lat <- ifelse(lat < 10, paste("0", lat, sep = ""), lat)
    if(useI) lettersUsed <- LETTERS[1:12]
    else lettersUsed <- LETTERS[c(1:8,10:13)]
    lon1 <- lettersUsed[(lon + 60) %/% 10]
    lon2 <- ifelse(lon1 == "A", floor(lon %% 4), floor(lon %% 10))
    ir <- paste(lat, lon1, lon2, sep = "")
    ir[outside] <- NA
    ir
  }
#' @export ir2d
#' @rdname d2ir
ir2d <-
  function(ir, useI = FALSE)
  {
    lat <- substring(ir, 1, 2)
    lat <- as.numeric(lat)
    lat <- (lat +71)/2 + 0.25
    lon1 <- substring(ir, 3, 3)
    lon1 <- toupper(lon1)
    lon1 <- match(lon1, LETTERS)
    if(!useI) lon1 <- ifelse(lon1 > 8, lon1 - 1, lon1)
    lon1 <- lon1 - 2
    lon2 <- substring(ir, 4)
    lon2 <- as.numeric(lon2)
    lon <- ifelse(lon1 < 0,
                  -44 + lon2 + 0.5,
                  -40 + 10*lon1 + lon2 + 0.5)
    data.frame(lat = lat, lon = lon)
  }