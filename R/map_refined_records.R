#' @title Visualize records on a dynamic interactive map
#' @name map_refined_records
#'
#' @description This optional step could map your simplified refined GBIF occurrence records on dynamic maps
#'
#' @param results_refine the refined occurrence list from `coordinate_refine`
#' @param save_to_disk_path (optional and if you do not need save just ignore it) the local disk
#'  path where you want to save the filtered GBIF occurrence records by `UltraGBIF_wcvp_family`
#' @param precision positive integer scalar controlling the density
#'  of your refined GBIF occurrence records on dynamic maps. (i.e. 4 for 20 km,
#'  3 for 156 km, 2 for 1250 km.) Default is 4
#' @param draw if TRUE present the map
#' @param cex the point size of your refined GBIF occurrence records rendering on dynamic maps. Default is 4
#'
#'
#' @return A dynamic interactive map with `wcvp_area_status` legend.
#' Note that the map has 3 different layers for you to choose to show.
#'
#' @import data.table
#' @import geohashTools
#' @import mapview
#' @importFrom dplyr %>% filter mutate select distinct case_when if_else slice ungroup group_by
#'
#' @seealso \code{\link[geohashTools]{gh_encode}}
#' @examples
#' \donttest{
#' help(map_refined_records)
#'}
#' @export
map_refined_records <- function(results_refine=NA,
                                save_to_disk_path=NA,
                                precision=4,
                                draw=F,
                                cex=4){

  all_records <- results_refine$all_records[UltraGBIF_useful_for_spatial_analysis==T,
                                            .(Ctrl_gbifID,
                                              Ctrl_level0Name,
                                              UltraGBIF_wcvp_family,
                                              UltraGBIF_decimalLatitude,
                                              UltraGBIF_decimalLongitude,
                                              UltraGBIF_wcvp_taxon_status,
                                              UltraGBIF_wcvp_taxon_name,
                                              UltraGBIF_wcvp_taxon_rank,
                                              wcvp_area_status)]

  dedup_by_geohash <- function(data, precision) {
    # 4 for 20 km, 3 for 156 km, 2 for 1250 km
    data <- data %>%
      filter(!is.na(UltraGBIF_decimalLatitude), !is.na(UltraGBIF_decimalLongitude)) %>%
      mutate(
        geohash = gh_encode(UltraGBIF_decimalLatitude, UltraGBIF_decimalLongitude, precision)
      ) %>%
      group_by(UltraGBIF_wcvp_taxon_name, geohash) %>%
      slice(1) %>%
      ungroup() %>%
      select(-geohash)
    return(data)
  }

  dedup <- dedup_by_geohash(all_records, precision)%>%setDT()

  if (!is.na(save_to_disk_path)) {
    family <- unique(dedup$UltraGBIF_wcvp_family)
    for (fam in family) {
      id <- dedup[UltraGBIF_wcvp_family==fam,.(Ctrl_gbifID)]
      table <- all_records[id]
      fwrite(table,file = paste0(save_to_disk_path,"/",fam,".csv"))
    }
  }

  if (draw){
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
    message("Map finished!")

    return(map)}
  else{return("ok")}
}
