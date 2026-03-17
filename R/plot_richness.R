#' @title Plot a richness map for native refined records in 1 degree grids
#' @name plot_richness
#'
#' @description This optional module plots a richness map of native refined records on grids. It
#' has drawn on `lets.presab.points` and `plot.PresenceAbsence` from R package `letsR`
#' (see references below for more information), but fully leverages vectorization techniques
#' to avoid looping when filling large matrices, thus achieving nearly a hundredfold speedup.
#'
#' @param records_refined UltraGBIF_refine list from \code{\link{refine_records}}
#' @param main tittle of the plot
#' @param xlab label of x axis of the plot
#' @param ylab label of y axis of the plot
#'
#'
#' @return UltraGBIF_richness list include a presence table and a richness S4 object
#'
#' @import data.table
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' plot_richness(records_refined = refine, main = 'richness map',
#' xlab = "Longitude", ylab = "Latitude")
#'}
#' @references
#' Vilela, B. and Villalobos, F. (2015), letsR: a new R package for data handling and analysis in macroecology. Methods Ecol Evol, 6: 1229-1234. https://doi.org/10.1111/2041-210X.12401
#' @export
plot_richness <- function(records_refined=NA,
                          main='richness plot',
                          xlab = "Longitude",
                          ylab = "Latitude"){

  crs = "+proj=longlat +datum=WGS84"
  xy <-  records_refined$native_records[,.(UltraGBIF_wcvp_taxon_name,
                                           UltraGBIF_decimalLongitude,
                                           UltraGBIF_decimalLatitude)]%>%
    terra::vect(crs = crs,geom=c('UltraGBIF_decimalLongitude','UltraGBIF_decimalLatitude'))

  limits <- terra::ext(xy)
  xmn <- limits[1]
  xmx <- limits[2]
  ymn <- limits[3]
  ymx <- limits[4]
  resol <- terra::res(terra::project(terra::rast(), crs))
  ras <- terra::rast(xmin = xmn,
                     xmax = xmx,
                     ymin = ymn,
                     ymax = ymx,
                     crs = as.character(crs),
                     resolution = resol,
                     name='richness')

  terra::values(ras) <- 0

  ext_df <- terra::extract(ras, xy, cells = TRUE, ID = FALSE)%>%setDT()
  ext_df[,taxon:=xy$UltraGBIF_wcvp_taxon_name]
  ext_df <- unique(ext_df)
  Resultado <- dcast(ext_df, cell ~ taxon, fun.aggregate = length)
  richness <- ext_df[,.N,cell]%>%setnames(old='N',new='count')
  ras_richness <- ras
  ras_richness[] <- NA
  ras_richness[Resultado$cell] <- rowSums(Resultado[, -(1:3), with = FALSE])


  ### plot
  colfunc <- grDevices::colorRampPalette(c("#fff5f0", "#fb6a4a", "#67000d"))
  v <- terra::values(ras_richness$richness)
  c <- max(v, na.rm = TRUE)
  v[(v == 0)] <- NA
  terra::values(ras_richness$richness) <- v
  terra::plot(ras_richness$richness, col = colfunc(c + 1)[-1],  main=main, xlab = xlab, ylab = ylab)
  map <- terra::vect(rnaturalearthdata::countries110)
  terra::plot(map, add = TRUE)
  message('Map finished!')

  final=list(presence = Resultado,ras_richness = ras_richness)
  class(final) <- 'UltraGBIF_richness'
  return(final)

}
