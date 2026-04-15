#' @title Visualize refined records on a dynamic interactive map
#' @name map_records
#'
#' @description This optional module renders refined GBIF occurrence records on a dynamic interactive
#' map, facilitating spatial exploration and quality assessment of the processed dataset. It employs
#' geohash-based deduplication to reduce visual clutter at various zoom levels, and displays records
#' color-coded by their native status classification.
#'
#' The function implements the following workflow:
#' \itemize{
#'   \item \strong{Record aggregation}: Combines native and non-native refined records that have
#'   passed spatial validation (\code{UltraGBIF_useful_for_spatial_analysis = TRUE})
#'   \item \strong{Geohash deduplication}: Applies geohash encoding at a user-specified precision
#'   level to collapse nearby points into single representatives, reducing overplotting and
#'   improving map rendering performance
#'   \item \strong{Interactive visualization}: Generates a multi-layer interactive map using
#'   \code{mapview}, with records color-coded by \code{wcvp_area_status} (native, introduced,
#'   extinct, location_doubtful)
#'   \item \strong{Multi-basemap support}: Provides three basemap options (OpenStreetMap,
#'   Esri World Imagery, Stadia Stamen Watercolor) for different visualization contexts
#' }
#'
#' @param refined_records UltraGBIF_refine object from \code{\link{refine_records}}
#' @param precision positive integer controlling the spatial resolution of geohash-based
#'   deduplication. Higher values produce finer-grained deduplication:
#'   \itemize{
#'     \item \code{4}: approximately 20 km resolution
#'     \item \code{3}: approximately 156 km resolution
#'     \item \code{2}: approximately 1250 km resolution
#'   }
#'   Default is 4.
#' @param cex numeric value controlling the point size of occurrence records on the map.
#'   Default is 4.
#'
#' @details
#' \strong{Geohash Deduplication Strategy:}
#'
#' Geohash encoding converts latitude-longitude pairs into alphanumeric strings representing
#' grid cells of varying sizes. By grouping records by species name, geohash cell, and native
#' status, then selecting the first record from each group, the function effectively reduces
#' visual overplotting while preserving the spatial distribution pattern. This is particularly
#' useful for densely sampled regions where thousands of records may cluster within small areas.
#'
#' \strong{Map Layers and Interactivity:}
#'
#' The generated map includes:
#' \itemize{
#'   \item A color-coded legend based on \code{wcvp_area_status} categories
#'   \item Popups displaying record attributes (GBIF ID, collection key, taxon name, etc.)
#'   \item Toggleable basemap layers for different visualization contexts
#'   \item Adjustable point transparency (\code{alpha.regions = 0.6}) for better density perception
#' }
#'
#' \strong{Record Selection Criteria:}
#'
#' Only records meeting the following criteria are included in the map:
#' \itemize{
#'   \item \code{UltraGBIF_useful_for_spatial_analysis = TRUE}: Coordinates passed GBIF validation
#'   \item For non-native records: \code{wcvp_area_status != "unknown"}: Must have a defined
#'   geographic classification
#' }
#'
#' @return A \code{mapview} interactive map object displaying refined occurrence records color-coded
#'   by \code{wcvp_area_status}. The map includes:
#'   \itemize{
#'     \item A legend showing the native status classification scheme
#'     \item Three switchable basemap layers (OpenStreetMap, Esri World Imagery, Stadia Stamen Watercolor)
#'     \item Clickable popups with record metadata
#'   }
#'
#' @seealso \code{\link[geohashTools]{gh_encode}}, \code{\link[mapview]{mapView}}
#'
#' @import data.table
#' @import geohashTools
#' @import mapview
#' @importFrom dplyr %>% filter mutate select slice ungroup group_by
#'
#' @examples
#' \dontrun{
#' # Make sur you have got refined_records from function refine_records
#' # This function may open your browser in the background; please be aware.
#' map_records(refined_records=refined_records,precision=4,cex=4)
#'}
#' @export
map_records <- function(refined_records=NA,
                        precision=4,
                        cex=4){

  all_records <- refined_records$native_records[UltraGBIF_useful_for_spatial_analysis==T,
                                            .(Ctrl_gbifID,
                                              Ctrl_key,
                                              UltraGBIF_wcvp_family,
                                              UltraGBIF_decimalLatitude,
                                              UltraGBIF_decimalLongitude,
                                              UltraGBIF_wcvp_taxon_status,
                                              UltraGBIF_wcvp_taxon_name,
                                              LEVEL3_COD,
                                              wcvp_area_status)]%>%
    rbind(refined_records$other_records[UltraGBIF_useful_for_spatial_analysis==T&wcvp_area_status!='unknown',
                                         .(Ctrl_gbifID,
                                           Ctrl_key,
                                           UltraGBIF_wcvp_family,
                                           UltraGBIF_decimalLatitude,
                                           UltraGBIF_decimalLongitude,
                                           UltraGBIF_wcvp_taxon_status,
                                           UltraGBIF_wcvp_taxon_name,
                                           LEVEL3_COD,
                                           wcvp_area_status)])

  dedup_by_geohash <- function(data, precision) {
    # 4 for 20 km, 3 for 156 km, 2 for 1250 km
    data <- data %>%
      filter(!is.na(UltraGBIF_decimalLatitude), !is.na(UltraGBIF_decimalLongitude)) %>%
      mutate(
        geohash = gh_encode(UltraGBIF_decimalLatitude, UltraGBIF_decimalLongitude, precision)
      ) %>%
      group_by(UltraGBIF_wcvp_taxon_name, geohash, wcvp_area_status) %>%
      slice(1) %>%
      ungroup() %>%
      select(-geohash)
    return(data)
  }

  dedup <- dedup_by_geohash(all_records, precision)%>%setDT()

  dedup_vect <- terra::vect(dedup,
                            geom = c("UltraGBIF_decimalLongitude","UltraGBIF_decimalLatitude"),
                            crs = "EPSG:4326")
  map=mapView(
    x = dedup_vect,
    zcol = "wcvp_area_status",
    legend = TRUE,
    layer.name = "wcvp_area_status",
    popup = T,
    cex = cex,
    alpha.regions = 0.6,
    map.types = c("OpenStreetMap","Esri.WorldImagery","Stadia.StamenWatercolor"),
    alpha=0.3
  )

  message("Finished!")
  return(map)
}
