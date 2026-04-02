#' @title Plot species richness map for native refined records on one-degree grids
#' @name plot_richness
#'
#' @description This optional module generates a species richness map by aggregating native refined
#' records onto a one-degree geographic grid. Inspired by \code{lets.presab.points} and
#' \code{plot.PresenceAbsence} from the \code{letsR} package (Vilela & Villalobos, 2015), this
#' implementation fully leverages vectorized \code{terra} operations to avoid iterative loops when
#' filling large presence-absence matrices, achieving nearly a hundredfold speedup over traditional
#' approaches.
#'
#' The function implements the following workflow:
#' \itemize{
#'   \item \strong{Grid construction}: Creates a one-degree resolution raster covering the
#'   full extent of the input occurrence coordinates, using the WGS84 coordinate reference system
#'   \item \strong{Cell assignment}: Extracts the raster cell ID for each occurrence point,
#'   then constructs a presence-absence matrix where rows represent grid cells and columns
#'   represent species
#'   \item \strong{Richness calculation}: Computes species richness per cell by summing the
#'   number of unique species present, then assigns these values back to the raster
#'   \item \strong{Visualization}: Renders the richness raster using a sequential color palette
#'   (light pink to dark red), with country boundaries overlaid for geographic context
#' }
#'
#' @param refined_records UltraGBIF_refine object from \code{\link{refine_records}}
#' @param main character string specifying the main title of the plot. Default is "richness plot".
#' @param xlab character string specifying the label for the x-axis. Default is "Longitude".
#' @param ylab character string specifying the label for the y-axis. Default is "Latitude".
#'
#' @details
#' \strong{Grid Resolution and Extent:}
#'
#' The raster grid is automatically constructed to cover the full spatial extent of the input
#' native records, with a resolution of one degree (approximately 111 km at the equator). This
#' resolution is commonly used in macroecological studies for continental-scale richness analyses.
#'
#' \strong{Presence-Absence Matrix:}
#'
#' The function uses \code{terra::extract} to assign each occurrence point to its corresponding
#' raster cell, then applies \code{data.table::dcast} to reshape the data into a wide-format
#' presence-absence matrix. Each cell in this matrix represents the count of occurrences for a
#' given species within a given grid cell. Richness is calculated as the row sum of non-zero
#' species counts.
#'
#' \strong{Color Palette:}
#'
#' The visualization uses a three-color sequential palette:
#' \itemize{
#'   \item \code{#fff5f0}: Light pink for low richness values
#'   \item \code{#fb6a4a}: Medium red for intermediate richness
#'   \item \code{#67000d}: Dark red for high richness hotspots
#' }
#' Zero-value cells are set to \code{NA} and rendered as transparent, allowing the basemap
#' (country boundaries) to show through in areas with no records.
#'
#' \strong{Performance Considerations:}
#'
#' By using \code{terra}'s vectorized extraction and \code{data.table}'s efficient reshaping
#' operations, this function avoids the nested loops that characterize traditional presence-absence
#' matrix construction, making it suitable for datasets with hundreds of thousands of occurrence
#' records.
#'
#' @return UltraGBIF_richness list containing:
#'   \itemize{
#'     \item \code{presence}: A data.table representing the presence-absence matrix, with columns
#'     for cell ID and one column per species, where values indicate the number of occurrences
#'     of that species within the cell
#'     \item \code{ras_richness}: A \code{SpatRaster} object containing the species richness values
#'     for each grid cell, suitable for further spatial analysis or export
#'   }
#'
#' @references
#' Vilela, B. and Villalobos, F. (2015). letsR: a new R package for data handling and analysis
#' in macroecology. \emph{Methods in Ecology and Evolution}, 6(10), 1229-1234.
#' \url{https://doi.org/10.1111/2041-210X.12401}
#'
#' @import data.table
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' richness <- plot_richness(refined_records = refined_records, main = 'richness map',
#' xlab = "Longitude", ylab = "Latitude")
#'}
#' @export
plot_richness <- function(refined_records=NA,
                          main='richness plot',
                          xlab = "Longitude",
                          ylab = "Latitude"){

  crs = "+proj=longlat +datum=WGS84"
  xy <-  refined_records$native_records[,.(UltraGBIF_wcvp_taxon_name,
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

  valid_cells <- Resultado$cell[!is.na(Resultado$cell) & Resultado$cell > 0]
  if(length(valid_cells) > 0){
    ras_richness[valid_cells] <- rowSums(Resultado[, -(1:3), with = FALSE])
  }


  ### plot
  colfunc <- grDevices::colorRampPalette(c("#fff5f0", "#fb6a4a", "#67000d"))
  v <- terra::values(ras_richness)
  c <- max(v, na.rm = TRUE)
  v[(v == 0)] <- NA
  terra::values(ras_richness) <- v
  terra::plot(ras_richness, col = colfunc(c + 1)[-1],  main=main, xlab = xlab, ylab = ylab)
  map <- terra::vect(rnaturalearthdata::countries110)
  terra::plot(map, add = TRUE)
  message('Finished!')

  final=list(presence = Resultado,ras_richness = ras_richness)
  class(final) <- 'UltraGBIF_richness'
  return(final)

}
