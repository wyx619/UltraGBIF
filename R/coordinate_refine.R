#' @title (Step 6) Refine the coordinates of the voucher and annotate their geographic status
#' @name coordinate_refine
#'
#' @description Step6 refines coordinates of the voucher and annotate their geographic status.
#'
#' @param voucher your processed voucher from step5
#' @param threads your threads requirement, a number >0. Default=4
#' @param save_to_disk_path the local disk path where you want to save the final result
#' @param tests CoordinateCleaner coordinates checks. Choose one or more from `c("capitals","centroids","equal","gbif","institutions","outliers","seas","zeros")`
#'
#' @details Step6 can automatically clean the coordinates of the voucher and annotate their geographic status
#'
#' @return A list with timer and 4 data.table: `all_records` for all cleaned occurrence records,
#' `native_records` for native cleaned occurrence records and `native_range` for their occurrence range,
#' `native_simplified` for simplified `native_records` by rounding the longitude and latitude coordinates of them to two decimal places
#'
#'
#' @import data.table
#' @importFrom dplyr %>% filter mutate select distinct case_when if_else inner_join
#' @import foreach
#' @import doParallel
#' @import rnaturalearthdata
#' @importFrom stats quantile
#'
#' @examples
#' \donttest{
#' help(coordinate_refine)
#'}
#' @export
coordinate_refine<-function(voucher = NA,
                            threads = 4,
                            save_to_disk_path = NA,
                            tests = c("capitals",
                                      "centroids",
                                      "equal",
                                      "gbif",
                                      "institutions",
                                      "outliers",
                                      "seas",
                                      "zeros"))
{
  start=Sys.time()

  voucher=voucher$occ_digital_voucher[UltraGBIF_dataset_result=="usable"]

  voucher[is.na(UltraGBIF_wcvp_taxon_name),`:=`(UltraGBIF_wcvp_taxon_rank=wcvp_taxon_rank,
                                                UltraGBIF_wcvp_taxon_status=wcvp_taxon_status,
                                                UltraGBIF_wcvp_family=wcvp_family,
                                                UltraGBIF_wcvp_taxon_name=wcvp_taxon_name,
                                                UltraGBIF_wcvp_taxon_authors=wcvp_taxon_authors,
                                                UltraGBIF_wcvp_reviewed=wcvp_reviewed)]

  voucher[,`:=`(Ctrl_georeferenceVerificationStatus=NULL,
                Ctrl_locationID=NULL,
                Ctrl_higherGeography=NULL,
                Ctrl_islandGroup=NULL,
                Ctrl_island=NULL,
                Ctrl_verbatimLocality=NULL,
                Ctrl_locationRemarks=NULL,
                Ctrl_identificationQualifier=NULL,
                Ctrl_typeStatus=NULL)]

  voucher[,Ctrl_gbifID:=as.character(Ctrl_gbifID)]
  numCores <- threads%>%as.numeric()%>%usecores()
  chunks_list <- voucher[,.(Ctrl_gbifID,
                            UltraGBIF_decimalLongitude,
                            UltraGBIF_decimalLatitude,
                            UltraGBIF_wcvp_plant_name_id,
                            Ctrl_countryCode,
                            UltraGBIF_wcvp_taxon_name)]%>%
    split(.,ceiling(seq_len(nrow(.)) / (nrow(.)/numCores)))

  seas_ref=seas_ref

  coord <- function(data){
    suppressWarnings(CoordinateCleaner::clean_coordinates(data,
                                         lon = "UltraGBIF_decimalLongitude",
                                         lat = "UltraGBIF_decimalLatitude",
                                         species = "UltraGBIF_wcvp_plant_name_id",
                                         tests = tests,
                                         seas_ref = seas_ref,
                                         value = "clean",
                                         verbose = FALSE))
  }

  cl <- parallel::makeCluster(numCores)
  registerDoParallel(cl)

  results_final_sf_ori <- foreach(data=chunks_list,
                                  .multicombine = T,
                                  .errorhandling = "remove",
                                  .packages = c("CoordinateCleaner","rnaturalearthdata"),
                                  .inorder = F) %dopar% {coord(data)}

  results_final_sf_ori <- results_final_sf_ori%>%rbindlist()

  parallel::stopCluster(cl)
  species=results_final_sf_ori$UltraGBIF_wcvp_taxon_name%>%unique()

  df <- rWCVPdata::wcvp_names %>%
    select(plant_name_id,taxon_name) %>%
    filter(plant_name_id%in%results_final_sf_ori$UltraGBIF_wcvp_plant_name_id)%>%
    inner_join(rWCVPdata::wcvp_distributions,by = "plant_name_id")%>%
    select(-continent_code_l1,-continent,-region_code_l2,-region,-plant_locality_id)%>%setDT()

  distribution <- rWCVP::wgsrpd3 %>%
    mutate(
      occurrence_type = case_when(
        LEVEL3_COD %in% df[location_doubtful == 1, area_code_l3] ~ "location_doubtful",
        LEVEL3_COD %in% df[location_doubtful == 0 & extinct == 0 & introduced == 0, area_code_l3] ~ "native",
        LEVEL3_COD %in% df[extinct == 1, area_code_l3] ~ "extinct",
        LEVEL3_COD %in% df[introduced == 1, area_code_l3] ~ "introduced",
        TRUE ~ "unknown")) %>% terra::vect()

  tmp_file <- tempfile(fileext = ".gpkg")
  terra::writeVector(distribution, tmp_file)

  powo_mark <- function(taxon_name) {
    distribution <- terra::vect(tmp_file)
    target_points <- results_final_sf_ori[UltraGBIF_wcvp_taxon_name == taxon_name]%>%
      terra::vect(
        geom = c("UltraGBIF_decimalLongitude", "UltraGBIF_decimalLatitude"),
        crs = "EPSG:4326")

    extracted_data <- terra::extract(distribution,target_points)%>%setDT()%>%unique(by="id.y")
    rm(distribution)
    extracted_data[,.(Ctrl_gbifID=target_points$Ctrl_gbifID,
                      wcvp_area_status=fifelse(is.na(occurrence_type),"unknown",occurrence_type))]
  }

  cluster <- parallel::makeCluster(numCores)
  registerDoParallel(cluster)

  area_final <- foreach(taxon_name=species,
                        .multicombine = T,
                        .errorhandling = "stop",
                        .inorder = F,
                        .packages = c("dplyr","data.table","terra"))%dopar%{powo_mark(taxon_name)}
  parallel::stopCluster(cluster)
  area_final <- area_final%>%rbindlist(fill = T)%>%unique()

  results <- merge(voucher,area_final, by = "Ctrl_gbifID")
  native_records <- results[wcvp_area_status == "native"]
  native_simplified <- native_records[, .(Ctrl_gbifID,
                                       Ctrl_recordedBy,
                                       Ctrl_eventDate,
                                       Ctrl_scientificName,
                                       UltraGBIF_wcvp_plant_name_id,
                                       UltraGBIF_wcvp_taxon_rank,
                                       UltraGBIF_wcvp_taxon_status,
                                       UltraGBIF_wcvp_family,
                                       UltraGBIF_wcvp_taxon_name,
                                       UltraGBIF_wcvp_taxon_authors,
                                       UltraGBIF_wcvp_reviewed,
                                       UltraGBIF_decimalLongitude = round(UltraGBIF_decimalLongitude, 2),
                                       UltraGBIF_decimalLatitude = round(UltraGBIF_decimalLatitude, 2))] %>%
    unique(by = c("UltraGBIF_wcvp_plant_name_id",
                  "UltraGBIF_wcvp_taxon_name",
                  "UltraGBIF_decimalLongitude",
                  "UltraGBIF_decimalLatitude"))

  native_range <- native_records[,.(Ctrl_gbifID,
                                 UltraGBIF_wcvp_plant_name_id,
                                 UltraGBIF_wcvp_taxon_rank,
                                 UltraGBIF_wcvp_taxon_status,
                                 UltraGBIF_wcvp_family,
                                 UltraGBIF_wcvp_taxon_name,
                                 UltraGBIF_wcvp_taxon_authors,
                                 UltraGBIF_wcvp_reviewed,
                                 UltraGBIF_decimalLongitude,
                                 UltraGBIF_decimalLatitude)]%>%
    unique(by=c("UltraGBIF_wcvp_plant_name_id",
                "UltraGBIF_decimalLongitude",
                "UltraGBIF_decimalLatitude"))

  native_range <- native_range[, .(`longitude_2.5%` = quantile(UltraGBIF_decimalLongitude, 0.025, na.rm = TRUE),
                                   `longitude_50%` = quantile(UltraGBIF_decimalLongitude, 0.50, na.rm = TRUE),
                                   `longitude_97.5%` = quantile(UltraGBIF_decimalLongitude, 0.975, na.rm = TRUE),
                                   `latitude_2.5%` = quantile(UltraGBIF_decimalLatitude, 0.025, na.rm = TRUE),
                                   `latitude_50%` = quantile(UltraGBIF_decimalLatitude, 0.50, na.rm = TRUE),
                                   `latitude_97.5%` = quantile(UltraGBIF_decimalLatitude, 0.975, na.rm = TRUE)),
                               by = .(UltraGBIF_wcvp_plant_name_id,
                                      UltraGBIF_wcvp_taxon_rank,
                                      UltraGBIF_wcvp_taxon_status,
                                      UltraGBIF_wcvp_family,
                                      UltraGBIF_wcvp_taxon_name,
                                      UltraGBIF_wcvp_taxon_authors,
                                      UltraGBIF_wcvp_reviewed)]

  if(!is.na(save_to_disk_path)){
    fwrite(results,
           file = paste0(save_to_disk_path,'/usable_data_refined_powo_checked.csv'),
           encoding = "UTF-8")
    fwrite(native_records,
           file = paste0(save_to_disk_path,'/native_data_refined_powo_checked.csv'),
           encoding = "UTF-8")
    fwrite(native_range,
           file = paste0(save_to_disk_path,'/native_distribution_range.csv'),
           encoding = "UTF-8")
    fwrite(native_simplified,
           file = paste0(save_to_disk_path,'/native_simplified.csv'),
           encoding = "UTF-8")

  }
  end=Sys.time()
  print(end-start)
  return(list(all_records=results,
              native_records=native_records,
              native_simplified=native_simplified,
              native_range=native_range,
              used_time=end-start))
}
