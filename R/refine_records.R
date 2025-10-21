#' @title Restore duplicate records from voucher, validate coordinates and extract their World Geographical Scheme for Recording Plant Distributions
#' @name refine_records
#'
#' @description Restore key usable information for vouchers by consolidating data from duplicate records belonging to identical collection events, validate coordinates and extract their World Geographical Scheme for Recording Plant Distributions.
#'
#' @param voucher voucher from `set_digital_voucher`
#' @param threads threads requirement, a positive real number, default is 4
#' @param save_path the local path where you want to save the final result
#' @param tests CoordinateCleaner checks. Choose one or more from `c("capitals","centroids","equal","gbif","institutions","outliers","seas","zeros")`
#'
#' @details It can restore key usable information for vouchers by consolidating data from duplicate records belonging to identical collection events, validate coordinates and extract their World Geographical Scheme for Recording Plant Distributions information
#'
#' @return A list with duration and 2 data.table: `all_records` for all refined records,
#' `native_records` for native records of them.
#'
#' @import data.table
#' @importFrom dplyr %>% filter mutate select distinct case_when if_else inner_join
#' @import foreach
#' @import doParallel
#' @import rnaturalearthdata
#' @import stringi
#' @importFrom stats quantile
#'
#' @examples
#' \donttest{
#' help(refine_records)
#'}
#' @export
refine_records<-function(voucher = NA,
                         threads = 4,
                         save_path = NA,
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

  restore_duplicates <- function(occ_digital_voucher = NULL) {

    MAX_FIELD_LENGTH <- 10000
    fields_to_merge <- c(
      'Ctrl_fieldNotes', 'Ctrl_year', 'Ctrl_stateProvince', 'Ctrl_municipality',
      'Ctrl_locality', 'Ctrl_countryCode', 'Ctrl_eventDate', 'Ctrl_habitat',
      'Ctrl_level0Name', 'Ctrl_level1Name', 'Ctrl_level2Name', 'Ctrl_level3Name'
    )
    fields_to_parse <- c(
      'Ctrl_gbifID', 'Ctrl_bibliographicCitation', 'Ctrl_language',
      'Ctrl_institutionCode', 'Ctrl_collectionCode', 'Ctrl_datasetName',
      'Ctrl_basisOfRecord', 'Ctrl_catalogNumber', 'Ctrl_recordNumber',
      'Ctrl_recordedBy', 'Ctrl_occurrenceStatus', 'Ctrl_eventDate',
      'Ctrl_year', 'Ctrl_month', 'Ctrl_day', 'Ctrl_habitat',
      'Ctrl_fieldNotes', 'Ctrl_eventRemarks', 'Ctrl_countryCode',
      'Ctrl_stateProvince', 'Ctrl_municipality', 'Ctrl_county',
      'Ctrl_locality', 'Ctrl_issue', 'Ctrl_level0Name', 'Ctrl_level1Name',
      'Ctrl_level2Name', 'Ctrl_level3Name', 'Ctrl_identifiedBy',
      'Ctrl_dateIdentified', 'Ctrl_scientificName', 'Ctrl_taxonRank',
      'Ctrl_decimalLatitude', 'Ctrl_decimalLongitude',
      'Ctrl_nameRecordedBy_Standard', 'Ctrl_recordNumber_Standard',
      'Ctrl_key_family_recordedBy_recordNumber', 'Ctrl_geospatial_quality',
      'Ctrl_verbatim_quality', 'Ctrl_moreInformativeRecord',
      'Ctrl_coordinates_validated_by_gbif_issue',
      "wcvp_plant_name_id", "wcvp_taxon_rank", "wcvp_taxon_status",
      "wcvp_family", "wcvp_taxon_name", "wcvp_taxon_authors",
      "wcvp_reviewed", "wcvp_searchedName", "wcvp_searchNotes",
      'UltraGBIF_digital_voucher', 'UltraGBIF_duplicates',
      'UltraGBIF_num_duplicates', 'UltraGBIF_non_groupable_duplicates',
      'UltraGBIF_duplicates_grouping_status', 'UltraGBIF_unidentified_sample',
      'UltraGBIF_sample_taxon_name', 'UltraGBIF_sample_taxon_name_status',
      'UltraGBIF_number_taxon_names', 'UltraGBIF_useful_for_spatial_analysis',
      'UltraGBIF_decimalLatitude', 'UltraGBIF_decimalLongitude',
      'UltraGBIF_wcvp_plant_name_id', 'UltraGBIF_wcvp_taxon_rank',
      'UltraGBIF_wcvp_taxon_status', 'UltraGBIF_wcvp_family',
      'UltraGBIF_wcvp_taxon_name', 'UltraGBIF_wcvp_taxon_authors',
      'UltraGBIF_wcvp_reviewed', 'UltraGBIF_dataset_result'
    )

    occ_tmp <- occ_digital_voucher[, ..fields_to_parse][, UltraGBIF_merged := FALSE]
    occ_in <- occ_tmp[UltraGBIF_dataset_result == "usable"]
    occ_dup <- occ_tmp[UltraGBIF_dataset_result == "duplicate"]
    merge_keys <- occ_in[UltraGBIF_duplicates == TRUE, unique(Ctrl_key_family_recordedBy_recordNumber)]
    rm(occ_tmp)
    setkeyv(occ_dup, "Ctrl_key_family_recordedBy_recordNumber")
    setkeyv(occ_in, "Ctrl_key_family_recordedBy_recordNumber")
    all_relevant_dups <- occ_dup[.(merge_keys)]
    for (field in fields_to_merge) {
      empty_keys <- occ_in[
        (is.na(get(field)) | get(field) == "" | get(field) == "NA") &
          UltraGBIF_duplicates == TRUE,
        unique(Ctrl_key_family_recordedBy_recordNumber)
      ]
      if (length(empty_keys) == 0) next

      candidates <- all_relevant_dups[
        .(empty_keys), on = "Ctrl_key_family_recordedBy_recordNumber"][
          !is.na(get(field)) &
            get(field) != "" &
            get(field) != "NA" &
            stringi::stri_length(get(field)) <= MAX_FIELD_LENGTH &
            stringi::stri_length(get(field)) > 0
        ][, .(best_value = head(get(field), 1)), by = Ctrl_key_family_recordedBy_recordNumber]

      if (nrow(candidates) == 0) next

      if (field == 'Ctrl_year') {
        candidates[, clean_value := suppressWarnings(as.integer(
          stringi::stri_trans_toupper(
            stringi::stri_replace_all_regex(best_value, "[\\{\\}\\[\\]\\(\\)\\\\\\*]", "")
          )
        ))]
      } else {
        candidates[, clean_value := stringi::stri_trans_toupper(
          stringi::stri_replace_all_regex(best_value, "[\\{\\}\\[\\]\\(\\)\\\\\\*]", "")
        )]
      }

      occ_in[candidates,
             c(field, "UltraGBIF_merged") := .(i.clean_value, TRUE),
             on = "Ctrl_key_family_recordedBy_recordNumber"]
      message(paste("Restoring",stri_replace_all_fixed(field,"Ctrl_","")))
    }
    return(occ_in)
  }

  voucher <- restore_duplicates(voucher$occ_digital_voucher)

  voucher[is.na(UltraGBIF_wcvp_taxon_name),`:=`(UltraGBIF_wcvp_taxon_rank=wcvp_taxon_rank,
                                                UltraGBIF_wcvp_taxon_status=wcvp_taxon_status,
                                                UltraGBIF_wcvp_family=wcvp_family,
                                                UltraGBIF_wcvp_taxon_name=wcvp_taxon_name,
                                                UltraGBIF_wcvp_taxon_authors=wcvp_taxon_authors,
                                                UltraGBIF_wcvp_reviewed=wcvp_reviewed)]

  voucher[,Ctrl_gbifID:=as.character(Ctrl_gbifID)]
  message("Validating coordinates")
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

  results_final_sf_ori <- rbindlist(results_final_sf_ori,fill = T)

  parallel::stopCluster(cl)
  rm(chunks_list)
  species <- results_final_sf_ori[, unique(UltraGBIF_wcvp_taxon_name)]

  local <- rWCVPdata::wcvp_names %>%
    select(plant_name_id,taxon_name) %>%
    filter(taxon_name %chin% results_final_sf_ori$UltraGBIF_wcvp_taxon_name)%>%
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
        LEVEL3_COD = NA_character_,
        wcvp_area_status = "unknown",
        Ctrl_gbifID = results_final_sf_ori[UltraGBIF_wcvp_taxon_name == taxon,Ctrl_gbifID]
      ))
    }

    occurrence_points <- results_final_sf_ori[UltraGBIF_wcvp_taxon_name == taxon,
                                              .(Ctrl_gbifID,UltraGBIF_decimalLongitude,UltraGBIF_decimalLatitude)] %>%
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

  message("Extracting WGSRPD information")

  for (i in 1:length(species)) {
    area_final[[i]] <- powo_mark(taxon = species[i],results_final_sf_ori=results_final_sf_ori)
    if (i%%1000==0|i==length(species)) {
      message(paste("Extracting",i,"/",length(species)))
    }
  }

  area_final <- area_final%>%rbindlist(fill = T)%>%unique()

  results <- merge(voucher,area_final, by = "Ctrl_gbifID")
  native_records <- results[wcvp_area_status == "native"]
  # native_simplified <- native_records[, .(Ctrl_gbifID,
  #                                      Ctrl_recordedBy,
  #                                      Ctrl_eventDate,
  #                                      Ctrl_scientificName,
  #                                      UltraGBIF_wcvp_plant_name_id,
  #                                      UltraGBIF_wcvp_taxon_rank,
  #                                      UltraGBIF_wcvp_taxon_status,
  #                                      UltraGBIF_wcvp_family,
  #                                      UltraGBIF_wcvp_taxon_name,
  #                                      UltraGBIF_wcvp_taxon_authors,
  #                                      UltraGBIF_wcvp_reviewed,
  #                                      UltraGBIF_decimalLongitude = round(UltraGBIF_decimalLongitude, 2),
  #                                      UltraGBIF_decimalLatitude = round(UltraGBIF_decimalLatitude, 2))] %>%
  #   unique(by = c("UltraGBIF_wcvp_plant_name_id",
  #                 "UltraGBIF_wcvp_taxon_name",
  #                 "UltraGBIF_decimalLongitude",
  #                 "UltraGBIF_decimalLatitude"))


  if(!is.na(save_path)){
    message("Exporting refined records")
    fwrite(results,
           file = paste0(save_path,'/usable_refined_records.csv.gz'),
           encoding = "UTF-8")
    fwrite(native_records,
           file = paste0(save_path,'/native_refined_records.csv.gz'),
           encoding = "UTF-8")
  }

  end=Sys.time()
  print(end-start)
  return(list(all_records=results,
              native_records=native_records,
              used_time=end-start))
}
