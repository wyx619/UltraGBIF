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
#' @return A list with duration and 3 data.table: `all_records` for all cleaned records,
#' `native_records` for native records of them and `native_simplified` for
#' simplified `native_records` by rounding the longitude and latitude coordinates of them to two decimal places
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
                                  .errorhandling = "stop",
                                  .packages = c("CoordinateCleaner","rnaturalearthdata"),
                                  .inorder = F) %dopar% {coord(data)}

  results_final_sf_ori <- results_final_sf_ori%>%rbindlist()

  parallel::stopCluster(cl)
  species <- results_final_sf_ori$UltraGBIF_wcvp_taxon_name%>%unique()

  local <- rWCVPdata::wcvp_names %>%
    select(plant_name_id,taxon_name) %>%
    filter(taxon_name%in%results_final_sf_ori$UltraGBIF_wcvp_taxon_name)%>%
    inner_join(rWCVPdata::wcvp_distributions,by = "plant_name_id")%>%
    select(-continent_code_l1,-continent,-region_code_l2,-region,-plant_locality_id)%>%setDT()

  local[,wcvp_area_status := fcase(
    location_doubtful == 1,"location_doubtful",
    introduced == 1, "introduced",
    extinct == 1,"extinct",
    introduced == 0 & extinct == 0 & location_doubtful == 0, "native",
    default = "unknown")][,c("introduced", "extinct", "location_doubtful") := NULL]

  kt <- terra::vect(rWCVP::wgsrpd3)

  powo_mark <- function(taxon=NA_character_,results_final_sf_ori="") {

    species_df <- local[taxon_name == taxon,.(area_code_l3, wcvp_area_status)]

    if (nrow(species_df) == 0) {
      return(data.table(
        LEVEL3_CODE="",
        wcvp_area_status = "unknown",
        Ctrl_gbifID = usable[UltraGBIF_wcvp_taxon_name == taxon]$Ctrl_gbifID
      ))
    }

    occurrence_points <- results_final_sf_ori[UltraGBIF_wcvp_taxon_name == taxon,.(Ctrl_gbifID,UltraGBIF_decimalLongitude,UltraGBIF_decimalLatitude)] %>%
      terra::vect(geom = c("UltraGBIF_decimalLongitude", "UltraGBIF_decimalLatitude"), crs = "EPSG:4326")

    distribution <-  terra::merge(kt,species_df,
                                  by.x = "LEVEL3_COD",
                                  by.y = "area_code_l3")%>%.[, c("LEVEL3_COD", "wcvp_area_status")]

    extracted_data <- terra::extract(distribution,occurrence_points)%>%setDT()%>%unique(by="id.y")

    extracted_data[,.(Ctrl_gbifID=results_final_sf_ori[UltraGBIF_wcvp_taxon_name == taxon,Ctrl_gbifID],
                      LEVEL3_COD=LEVEL3_COD,
                      wcvp_area_status=fifelse(is.na(wcvp_area_status),"unknown",wcvp_area_status))]

  }

  area_final <- list()
  message("extracting WGSRPD information")
  for (i in 1:length(species)) {
    area_final[[i]] <- powo_mark(taxon = species[i],results_final_sf_ori=results_final_sf_ori)
    if (i%%1000==0) {
      message(paste0(i,"/",length(species)))
    }
  }

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


  if(!is.na(save_to_disk_path)){
    fwrite(results,
           file = paste0(save_to_disk_path,'/usable_data_refined_powo_checked.csv'),
           encoding = "UTF-8")
    fwrite(native_records,
           file = paste0(save_to_disk_path,'/native_data_refined_powo_checked.csv'),
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
              used_time=end-start))
}
