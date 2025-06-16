#' @title (Step 5) Set master digital voucher
#' @name make_digital_voucher
#'
#' @description ### Grouping Duplicates and Selecting the Digital Voucher
#'
#' #### 1. Handling Complete Collection Event Keys
#' - Unique collection events often generate multiple duplicate GBIF records. One duplicate is designated as the master digital voucher, integrating data from other duplicates.
#' - When the collection event key is complete, duplicates are grouped and parsed based on record completeness and geospatial quality:
#'   - *Record Completeness* : Assessed using data-quality scores for fields: `recordedBy`, `recordNumber`, `year`, `institutionCode`, `catalogNumber`, `locality`, `municipality`, `countryCode`, `stateProvince`, and `fieldNotes`.
#'   - *Geospatial Quality* : Ranked via a score derived from geospatial issues in `EnumOccurrenceIssue` (GBIF table).
#' - The duplicate with the highest total score (sum of record completeness + geospatial quality) is assigned as the master voucher. Missing data from other duplicates are merged into this voucher.
#'
#' #### 2. Handling Incomplete Collection Event Keys
#' - When the collection event key is incomplete, duplicates cannot be parsed. Each record is treated as a unique collection event, with no duplicates identified.
#' - Record completeness and geospatial quality are still evaluated as described below to assess data integrity.
#'
#' #### 3. Quality Score Calculation
#' **UltraGBIF_digital_voucher** : The duplicate with the highest total score, calculated as
#'
#' ### Total Score = Record Completeness + Quality of Geospatial Information
#'
#' #### 4. Record Completeness Calculation
#' - Measured as the sum of binary flags (TRUE = 1, FALSE = 0) for the following fields:
#'   - Is `recordedBy` present?
#'   - Is `recordNumber` present?
#'   - Is `year` present?
#'   - Is `institutionCode` present?
#'   - Is `catalogNumber` present?
#'   - Is `locality` present?
#'   - Is `municipality` present?
#'   - Is `countryCode` present?
#'   - Is `stateProvince` present?
#'   - Is `fieldNotes` present?
#'
#' #### 5. Geospatial Quality Calculation
#' - Based on GBIF geospatial issues in `EnumOccurrenceIssue`, classified into three categories:
#'   - *No Impact* : Issues not affecting coordinate accuracy (`selection_score = -1`).
#'   - *Potential Impact* : Issues that may affect coordinate accuracy (`selection_score = -3`).
#'   - *Exclusion* : Records with severe issues (`selection_score = -9`) are excluded.
#'
#' **This structured approach ensures systematic grouping of duplicates and selection of the master digital voucher without altering the original logic or variables.**
#'
#' @param occ_import imported GBIF occurrence data from step1
#' @param names.checked your checked species names from step2
#' @param collection_key your collection mark from step4
#' @param threads your threads requirement, a number >0. Default=4
#'
#' @details
#' `UltraGBIF_duplicates_grouping_status` :
#' * `groupable`: Complete key, duplicates parsed.
#' * `not groupable: no recordedBy and no recordNumber`: Missing both fields.
#' * `not groupable: no recordNumber`: Missing recordNumber.
#' * `not groupable: no recordedBy`: Missing recordedBy.
#'
#' `UltraGBIF_num_duplicates`: Integer count of duplicate records.
#'
#' `UltraGBIF_duplicates`: TRUE if duplicates exist, FALSE otherwise.
#'
#' `UltraGBIF_non_groupable_duplicates`: TRUE if duplicates cannot be grouped, FALSE otherwise.
#'
#' @return A list with duration and  2 data.table:
#' "occ_digital_voucher" for all data processing fields and "occ_results" for only result fields.
#'
#' @import data.table
#' @import stringi
#' @import foreach
#' @import doParallel
#' @importFrom dplyr %>% filter mutate select distinct case_when if_else
#'
#' @examples
#' \donttest{
#' help(make_digital_voucher)
#'}
#' @export
make_digital_voucher <-  function(occ_import = NA,
                                  names.checked = NA,
                                  collection_key = NA,
                                  threads = 4)
{
  start=Sys.time()
  {
    EnumOccurrenceIssue <- EnumOccurrenceIssue
    occ <- occ_import$occ%>%setDT()%>%setorder(Ctrl_gbifID)
    occ_in <- occ%>%setorder(Ctrl_gbifID)
    occ_gbif_issue <- occ_import$occ_gbif_issue%>%setorder(Ctrl_gbifID)
    occ_wcvp_check_name <- names.checked$occ_wcvp_check_name%>%setorder(Ctrl_gbifID)
    occ_collectorsDictionary <- collection_key$occ_collectorsDictionary%>%setorder(Ctrl_gbifID)

    occ <- cbind(occ_gbif_issue, occ_in, occ_wcvp_check_name, occ_collectorsDictionary)
    setDT(occ)
    occ[is.na(wcvp_taxon_rank), wcvp_taxon_rank := '']
    occ[is.na(wcvp_taxon_status), wcvp_taxon_status := '']

    index_tmp1 <- EnumOccurrenceIssue$score == 1 & EnumOccurrenceIssue$type == 'geospatial' %>%
      ifelse(is.na(.), FALSE,.)
    index_tmp2 <- EnumOccurrenceIssue$score == 2 & EnumOccurrenceIssue$type == 'geospatial'%>%
      ifelse(is.na(.), FALSE,.)
    index_tmp3 <- EnumOccurrenceIssue$score == 3 & EnumOccurrenceIssue$type == 'geospatial'%>%
      ifelse(is.na(.), FALSE,.)

    occ[, `:=`(
      temAnoColeta = !is.na(Ctrl_year) & (Ctrl_year == "" | Ctrl_year == 0 | Ctrl_year <= 10),
      temCodigoInstituicao = !is.na(Ctrl_institutionCode) & Ctrl_institutionCode != "",
      temNumeroCatalogo = !is.na(Ctrl_catalogNumber) & Ctrl_catalogNumber != "",
      temColetor = !is.na(Ctrl_recordedBy) & Ctrl_recordedBy != "",
      temNumeroColeta = !is.na(Ctrl_recordNumber) & Ctrl_recordNumber != "",
      temPais = !COUNTRY_INVALID,
      temUF = !is.na(Ctrl_stateProvince) & Ctrl_stateProvince != "",
      temMunicipio = !is.na(Ctrl_municipality) & Ctrl_municipality != "",
      temLocalidade = !is.na(Ctrl_locality) & Ctrl_locality != "",
      temNotas = !is.na(Ctrl_fieldNotes) & Ctrl_fieldNotes != ""
    )]
  }

  {
    a3=(EnumOccurrenceIssue$constant[index_tmp3 == TRUE])
    a2=(EnumOccurrenceIssue$constant[index_tmp2 == TRUE])
    a1=(EnumOccurrenceIssue$constant[index_tmp1 == TRUE])

    occ[, `:=`(
      Ctrl_geospatial_quality = 0,
      Ctrl_verbatim_quality = 0,
      Ctrl_moreInformativeRecord = 0,
      UltraGBIF_digital_voucher = FALSE,
      UltraGBIF_duplicates = FALSE,
      UltraGBIF_non_groupable_duplicates = FALSE,
      UltraGBIF_num_duplicates = 0,
      UltraGBIF_duplicates_grouping_status = '',
      Ctrl_coordinates_validated_by_gbif_issue = FALSE,
      UltraGBIF_unidentified_sample = TRUE,
      UltraGBIF_wcvp_plant_name_id = '',
      UltraGBIF_sample_taxon_name = '',
      UltraGBIF_sample_taxon_name_status = '',
      UltraGBIF_number_taxon_names = 0,
      UltraGBIF_useful_for_spatial_analysis = FALSE,
      UltraGBIF_decimalLatitude = NA_real_,
      UltraGBIF_decimalLongitude = NA_real_
    )]
    occ[, Ctrl_coordinates_validated_by_gbif_issue := rowSums(.SD) == 0, .SDcols = a3]

    occ[, Ctrl_coordinates_validated_by_gbif_issue := ifelse(Ctrl_hasCoordinate == FALSE | Ctrl_decimalLatitude == 0 | Ctrl_decimalLongitude == 0, FALSE, Ctrl_coordinates_validated_by_gbif_issue)]
    occ[, Ctrl_coordinates_validated_by_gbif_issue := ifelse(is.na(Ctrl_coordinates_validated_by_gbif_issue),  FALSE, Ctrl_coordinates_validated_by_gbif_issue)]
    occ[,Ctrl_geospatial_quality := fcase(
        rowSums(.SD[, a3, with = FALSE]) > 0, -9,
        rowSums(.SD[, a2, with = FALSE]) > 0, -3,
        rowSums(.SD[, a1, with = FALSE]) > 0, -1,
        default = 0
        )]

    occ[Ctrl_hasCoordinate == FALSE, Ctrl_geospatial_quality := -9]

    occ[, Ctrl_verbatim_quality := temColetor + temNumeroColeta + temAnoColeta +
        temCodigoInstituicao + temNumeroCatalogo + temLocalidade +
        temMunicipio + temUF + temPais + temNotas]
    occ[, Ctrl_moreInformativeRecord := Ctrl_geospatial_quality + Ctrl_verbatim_quality]

    occ <- occ[, .(Ctrl_gbifID,
                   Ctrl_key_family_recordedBy_recordNumber,
                   wcvp_plant_name_id,
                   wcvp_taxon_name,
                   wcvp_taxon_status,
                   wcvp_searchNotes,
                   Ctrl_taxonRank,
                   Ctrl_geospatial_quality,
                   Ctrl_verbatim_quality,
                   Ctrl_moreInformativeRecord,
                   UltraGBIF_digital_voucher,
                   UltraGBIF_duplicates,
                   UltraGBIF_num_duplicates,
                   UltraGBIF_non_groupable_duplicates,
                   UltraGBIF_duplicates_grouping_status,
                   Ctrl_coordinates_validated_by_gbif_issue,
                   Ctrl_decimalLatitude,
                   Ctrl_decimalLongitude,
                   UltraGBIF_unidentified_sample,
                   UltraGBIF_wcvp_plant_name_id,
                   UltraGBIF_sample_taxon_name,
                   UltraGBIF_sample_taxon_name_status,
                   UltraGBIF_number_taxon_names,
                   UltraGBIF_useful_for_spatial_analysis,
                   UltraGBIF_decimalLatitude,
                   UltraGBIF_decimalLongitude)]

    occ[stri_endswith_fixed(Ctrl_key_family_recordedBy_recordNumber, "_NA"),
        Ctrl_key_family_recordedBy_recordNumber := stri_replace_last_regex(Ctrl_key_family_recordedBy_recordNumber, "_NA$", "_")]

    process_record <- function(r) {
      FAMILY__ <- FAMILY__recordNumber <- FAMILY_recordedBy_ <- FALSE

      occ_adj <- occ[Ctrl_key_family_recordedBy_recordNumber == r]
      occ_adj[is.na(wcvp_taxon_status),wcvp_taxon_status:=""]

      num_records <- nrow(occ_adj)
      if (num_records == 0) {
        print(r)
        print("table")
        return()
      }

      if (stri_endswith_fixed(r, "_") | stri_detect_fixed(r, "__") | stri_detect_regex(r, "UNKNOWN")) {
        FAMILY__ <- stri_detect_fixed(r, "__") & stri_endswith_fixed(r, "__") %>% ifelse(is.na(.), FALSE,.)

        if (!FAMILY__) {
          FAMILY_recordedBy_ <- (stri_detect_fixed(r, "__") & !stri_endswith_fixed(r, "__")) |
            stri_detect_regex(r, "UNKNOWN") %>% ifelse(is.na(.), FALSE,.)

          if (!FAMILY_recordedBy_) {
            FAMILY__recordNumber <- stri_endswith_fixed(r, "_") & stri_sub(r, -2, -2) != "_" %>% ifelse(is.na(.), FALSE,.)
          }
        }
      }

      if (FAMILY__ == TRUE | FAMILY__recordNumber == TRUE | FAMILY_recordedBy_ == TRUE) {

        occ_adj[, `:=`(
          UltraGBIF_digital_voucher = TRUE,
          UltraGBIF_non_groupable_duplicates = TRUE,
          UltraGBIF_duplicates = FALSE,
          UltraGBIF_num_duplicates = 1,
          UltraGBIF_wcvp_plant_name_id = fifelse(wcvp_taxon_status == "Accepted",as.character(wcvp_plant_name_id),""),
          UltraGBIF_sample_taxon_name = fifelse(wcvp_taxon_status == "Accepted",wcvp_taxon_name,""),
          UltraGBIF_unidentified_sample = wcvp_taxon_status != "Accepted",
          UltraGBIF_duplicates_grouping_status = fifelse(FAMILY__, "not groupable: no recordedBy and no recordNumber",
                                                         fifelse(FAMILY__recordNumber, "not groupable: no recordNumber",
                                                                 fifelse(FAMILY_recordedBy_, "not groupable: no recordedBy",
                                                                         "not groupable"))),
          UltraGBIF_number_taxon_names = fifelse(wcvp_taxon_status != "Accepted", 0, 1),
          UltraGBIF_sample_taxon_name_status = fifelse(wcvp_taxon_status != "Accepted", "unidentified", "identified"),
          UltraGBIF_decimalLatitude = fifelse(Ctrl_coordinates_validated_by_gbif_issue, Ctrl_decimalLatitude, NA_real_),
          UltraGBIF_decimalLongitude = fifelse(Ctrl_coordinates_validated_by_gbif_issue, Ctrl_decimalLongitude, NA_real_),
          UltraGBIF_useful_for_spatial_analysis = Ctrl_coordinates_validated_by_gbif_issue)]
        return(occ_adj)
      } else {
        occ_adj[, `:=`(
          UltraGBIF_duplicates_grouping_status = "groupable",
          UltraGBIF_duplicates = num_records > 1,
          UltraGBIF_num_duplicates = num_records,
          UltraGBIF_digital_voucher = Ctrl_moreInformativeRecord == max(Ctrl_moreInformativeRecord))]
        occ_adj[,UltraGBIF_digital_voucher := seq_len(.N) == which(UltraGBIF_digital_voucher)[1] & UltraGBIF_digital_voucher]

        # bind wcvp_taxon_name & wcvp_plant_name_id
        occ_adj[, wcvp_taxon_name_and_wcvp_plant_name_id := paste0(wcvp_taxon_name, ";", wcvp_plant_name_id)]
        taxon_name_sample <- occ_adj[,.N,by =.(wcvp_taxon_name_and_wcvp_plant_name_id, wcvp_taxon_status)][N > 0][order(-N)]
        num_taxon_name <- nrow(taxon_name_sample)
        if (num_taxon_name == 0) {
          print(occ_adj$wcvp_taxon_name)
          print("0 - Error")
          return()
        }
        if (num_taxon_name == 1 && taxon_name_sample$wcvp_taxon_status[1] == "Accepted") {
          sp_name_id <- stri_split_fixed(taxon_name_sample$wcvp_taxon_name_and_wcvp_plant_name_id[1], ";")%>%unlist()
          occ_adj[, `:=`(
            UltraGBIF_sample_taxon_name = sp_name_id[1],
            UltraGBIF_wcvp_plant_name_id = sp_name_id[2],
            UltraGBIF_sample_taxon_name_status = "identified",
            UltraGBIF_unidentified_sample = FALSE )]}
        if (num_taxon_name > 1) {
          accepted_taxon <- taxon_name_sample[wcvp_taxon_status == "Accepted"]
          if (nrow(accepted_taxon) > 0) {
            sp_name_id <- stri_split_fixed(accepted_taxon$wcvp_taxon_name_and_wcvp_plant_name_id[1], ";")%>%unlist()
            occ_adj[, `:=`(
              UltraGBIF_sample_taxon_name = sp_name_id[1],
              UltraGBIF_wcvp_plant_name_id = sp_name_id[2],
              UltraGBIF_sample_taxon_name_status = "divergent identifications",
              UltraGBIF_unidentified_sample = FALSE )]
          }
        }

        if (any(occ_adj[UltraGBIF_digital_voucher==T,Ctrl_coordinates_validated_by_gbif_issue])) {
          idx <- which(occ_adj[UltraGBIF_digital_voucher==T,Ctrl_coordinates_validated_by_gbif_issue])
          occ_adj[, `:=`(
            UltraGBIF_decimalLatitude = Ctrl_decimalLatitude[idx],
            UltraGBIF_decimalLongitude = Ctrl_decimalLongitude[idx],
            UltraGBIF_useful_for_spatial_analysis = TRUE )]
        } else {
          valid_indices <- which(occ_adj[,Ctrl_coordinates_validated_by_gbif_issue])
          if (length(valid_indices) > 1) {
            best_idx <- valid_indices[which.max(occ_adj[valid_indices,Ctrl_geospatial_quality])]
            occ_adj[, `:=`(
              UltraGBIF_decimalLatitude = Ctrl_decimalLatitude[best_idx],
              UltraGBIF_decimalLongitude = Ctrl_decimalLongitude[best_idx],
              UltraGBIF_useful_for_spatial_analysis = TRUE )]
          }
          if (length(valid_indices) == 1) {
            occ_adj[, `:=`(
              UltraGBIF_decimalLatitude = Ctrl_decimalLatitude[valid_indices],
              UltraGBIF_decimalLongitude = Ctrl_decimalLongitude[valid_indices],
              UltraGBIF_useful_for_spatial_analysis = TRUE )]
          }
        }
      }
      return(occ_adj)
    }

    recordedBy_unique <-  occ[,Ctrl_key_family_recordedBy_recordNumber] %>% unique()
    message("data prepared")

    numCores <- threads %>% as.numeric() %>%usecores()
    cl <- parallel::makeCluster(numCores)
    registerDoParallel(cl)

    results <- foreach(r = recordedBy_unique,
                       .multicombine = T,
                       .errorhandling = "remove",
                       .packages = c("data.table","dplyr","stringi"),
                       .inorder = F)%dopar%{process_record(r)}

    parallel::stopCluster(cl)

    occ <- rbindlist(results,fill = T)[,.(Ctrl_gbifID,
                                       Ctrl_geospatial_quality,
                                       Ctrl_verbatim_quality,
                                       Ctrl_moreInformativeRecord,
                                       UltraGBIF_digital_voucher,
                                       UltraGBIF_duplicates,
                                       UltraGBIF_num_duplicates,
                                       UltraGBIF_non_groupable_duplicates,
                                       UltraGBIF_duplicates_grouping_status,
                                       Ctrl_coordinates_validated_by_gbif_issue,
                                       UltraGBIF_unidentified_sample,
                                       UltraGBIF_wcvp_plant_name_id,
                                       UltraGBIF_sample_taxon_name,
                                       UltraGBIF_sample_taxon_name_status,
                                       UltraGBIF_number_taxon_names,
                                       UltraGBIF_useful_for_spatial_analysis,
                                       UltraGBIF_decimalLatitude,
                                       UltraGBIF_decimalLongitude)]%>%setorder(Ctrl_gbifID)
  }

  {
    occ_all <- cbind(occ_in, occ_wcvp_check_name, occ_collectorsDictionary, occ)%>%setDT()
    col_tmp <- unique(names(occ_all))
    occ_all <- occ_all[,..col_tmp]

    occ_all[, UltraGBIF_dataset_result := fcase(
      UltraGBIF_digital_voucher == TRUE & UltraGBIF_unidentified_sample == FALSE & UltraGBIF_useful_for_spatial_analysis == TRUE & is.na(UltraGBIF_decimalLongitude) == FALSE, "usable",
      UltraGBIF_digital_voucher == FALSE, "duplicate",
      UltraGBIF_digital_voucher == TRUE & (UltraGBIF_unidentified_sample == TRUE | UltraGBIF_useful_for_spatial_analysis == FALSE | is.na(UltraGBIF_decimalLongitude) == TRUE), "unusable"
    )]

    name_tmp <- occ_wcvp_check_name[, .(wcvp_plant_name_id = as.character(wcvp_plant_name_id),
                                        wcvp_taxon_rank,
                                        wcvp_taxon_status,
                                        wcvp_family,
                                        wcvp_taxon_name,
                                        wcvp_taxon_authors,
                                        wcvp_reviewed)]%>%unique(by = "wcvp_plant_name_id")%>%
      setnames(paste0('UltraGBIF_', names(.))) %>% stats::na.omit()

    occ_all <- merge(occ_all, name_tmp, by = 'UltraGBIF_wcvp_plant_name_id', all.x = TRUE)

    cols_to_keep <- c("Ctrl_gbifID", "Ctrl_bibliographicCitation", "Ctrl_language",
                      "Ctrl_institutionCode", "Ctrl_collectionCode", "Ctrl_datasetName",
                      "Ctrl_basisOfRecord", "Ctrl_catalogNumber", "Ctrl_recordNumber",
                      "Ctrl_recordedBy", "Ctrl_georeferenceVerificationStatus",
                      "Ctrl_occurrenceStatus", "Ctrl_eventDate", "Ctrl_year", "Ctrl_month",
                      "Ctrl_day", "Ctrl_habitat", "Ctrl_fieldNotes", "Ctrl_eventRemarks",
                      "Ctrl_locationID", "Ctrl_higherGeography", "Ctrl_islandGroup",
                      "Ctrl_island", "Ctrl_countryCode", "Ctrl_stateProvince",
                      "Ctrl_municipality", "Ctrl_county", "Ctrl_locality",
                      "Ctrl_verbatimLocality", "Ctrl_locationRemarks", "Ctrl_level0Name",
                      "Ctrl_level1Name", "Ctrl_level2Name", "Ctrl_level3Name",
                      "Ctrl_identifiedBy", "Ctrl_dateIdentified", "Ctrl_scientificName",
                      "Ctrl_decimalLatitude", "Ctrl_decimalLongitude", "Ctrl_identificationQualifier",
                      "Ctrl_typeStatus", "Ctrl_family", "Ctrl_taxonRank", "Ctrl_issue",
                      "Ctrl_nameRecordedBy_Standard", "Ctrl_recordNumber_Standard",
                      "Ctrl_key_family_recordedBy_recordNumber", "Ctrl_geospatial_quality",
                      "Ctrl_verbatim_quality", "Ctrl_moreInformativeRecord",
                      "Ctrl_coordinates_validated_by_gbif_issue", "wcvp_plant_name_id",
                      "wcvp_taxon_rank", "wcvp_taxon_status", "wcvp_family",
                      "wcvp_taxon_name", "wcvp_taxon_authors", "wcvp_reviewed",
                      "wcvp_searchedName", "wcvp_searchNotes", "UltraGBIF_digital_voucher",
                      "UltraGBIF_duplicates", "UltraGBIF_num_duplicates",
                      "UltraGBIF_non_groupable_duplicates", "UltraGBIF_duplicates_grouping_status",
                      "UltraGBIF_unidentified_sample", "UltraGBIF_sample_taxon_name",
                      "UltraGBIF_sample_taxon_name_status", "UltraGBIF_number_taxon_names",
                      "UltraGBIF_useful_for_spatial_analysis", "UltraGBIF_decimalLatitude",
                      "UltraGBIF_decimalLongitude", "UltraGBIF_dataset_result","UltraGBIF_wcvp_plant_name_id",
                      "UltraGBIF_wcvp_taxon_rank", "UltraGBIF_wcvp_taxon_status",
                      "UltraGBIF_wcvp_family", "UltraGBIF_wcvp_taxon_name",
                      "UltraGBIF_wcvp_taxon_authors", "UltraGBIF_wcvp_reviewed")

    occ_all <- occ_all[,..cols_to_keep]
    }
  end=Sys.time()
  print(end-start)

  return(list(occ_digital_voucher = occ_all,
              occ_results = occ,
              used_time=end-start))
}

