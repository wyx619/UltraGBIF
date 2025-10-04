#' @title Set master digital voucher
#' @name set_digital_voucher
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
#' **This optimized version eliminates O(n²) complexity by using vectorized data.table operations.**
#'
#' @param occ_import imported GBIF records
#' @param names.checked your checked taxon names from `check_occ_name`
#' @param collection_key your collection mark from `generate_collection_mark`
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
#' @return A list with duration and 2 data.table:
#' "occ_digital_voucher" for all data processing fields and "occ_results" for only result fields.
#'
#' @import data.table
#' @import stringi
#' @importFrom dplyr %>% filter mutate select distinct case_when if_else
#'
#' @examples
#' \donttest{
#' help(set_digital_voucher)
#'}
#' @export
set_digital_voucher <- function(occ_import = NA,
                                 names.checked = NA,
                                 collection_key = NA)
{
  start <- Sys.time()

  # ============================================================================
  # SECTION 1: Data Preparation and Quality Field Calculation
  # ============================================================================
  message("Starting voucher preparation...")

  {
    EnumOccurrenceIssue <- EnumOccurrenceIssue
    occ <- occ_import$occ %>% setDT() %>% setorder(Ctrl_gbifID)
    occ_in <- occ %>% setorder(Ctrl_gbifID)
    occ_gbif_issue <- occ_import$occ_gbif_issue %>% setorder(Ctrl_gbifID)
    occ_wcvp_check_name <- names.checked$occ_wcvp_check_name %>% setorder(Ctrl_gbifID)
    occ_collectorsDictionary <- collection_key$occ_collectorsDictionary %>% setorder(Ctrl_gbifID)

    # Combine all data sources
    occ <- cbind(occ_gbif_issue %>% select(-Ctrl_gbifID),
                 occ_in, occ_wcvp_check_name %>% select(-Ctrl_gbifID),
                 occ_collectorsDictionary %>% select(-Ctrl_gbifID))
    setDT(occ)
    occ[is.na(wcvp_taxon_rank), wcvp_taxon_rank := '']
    occ[is.na(wcvp_taxon_status), wcvp_taxon_status := '']
    occ[,Ctrl_gbifID]
    # Identify geospatial issue categories from EnumOccurrenceIssue
    index_tmp1 <- EnumOccurrenceIssue$score == 1 & EnumOccurrenceIssue$type == 'geospatial' %>%
      ifelse(is.na(.), FALSE, .)
    index_tmp2 <- EnumOccurrenceIssue$score == 2 & EnumOccurrenceIssue$type == 'geospatial' %>%
      ifelse(is.na(.), FALSE, .)
    index_tmp3 <- EnumOccurrenceIssue$score == 3 & EnumOccurrenceIssue$type == 'geospatial' %>%
      ifelse(is.na(.), FALSE, .)

    # Calculate record completeness flags
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

  # ============================================================================
  # SECTION 2: Calculate Quality Scores
  # ============================================================================
  message("Calculating quality scores...")

  {
    # Get column names for each issue severity level
    a3 <- (EnumOccurrenceIssue$constant[index_tmp3 == TRUE])
    a2 <- (EnumOccurrenceIssue$constant[index_tmp2 == TRUE])
    a1 <- (EnumOccurrenceIssue$constant[index_tmp1 == TRUE])

    # Initialize result columns
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

    # Validate coordinates based on GBIF issues
    occ[, Ctrl_coordinates_validated_by_gbif_issue := rowSums(.SD) == 0, .SDcols = a3]
    occ[, Ctrl_coordinates_validated_by_gbif_issue := ifelse(
      Ctrl_hasCoordinate == FALSE | Ctrl_decimalLatitude == 0 | Ctrl_decimalLongitude == 0,
      FALSE,
      Ctrl_coordinates_validated_by_gbif_issue)]
    occ[, Ctrl_coordinates_validated_by_gbif_issue := ifelse(
      is.na(Ctrl_coordinates_validated_by_gbif_issue),
      FALSE,
      Ctrl_coordinates_validated_by_gbif_issue)]

    # Calculate geospatial quality score
    occ[, Ctrl_geospatial_quality := fcase(
      rowSums(.SD[, a3, with = FALSE]) > 0, -9,
      rowSums(.SD[, a2, with = FALSE]) > 0, -3,
      rowSums(.SD[, a1, with = FALSE]) > 0, -1,
      default = 0
    )]
    occ[Ctrl_hasCoordinate == FALSE, Ctrl_geospatial_quality := -9]

    # Calculate verbatim quality score (record completeness)
    occ[, Ctrl_verbatim_quality := temColetor + temNumeroColeta + temAnoColeta +
          temCodigoInstituicao + temNumeroCatalogo + temLocalidade +
          temMunicipio + temUF + temPais + temNotas]

    # Calculate total quality score
    occ[, Ctrl_moreInformativeRecord := Ctrl_geospatial_quality + Ctrl_verbatim_quality]

    # Clean up collection key format
    occ[stri_endswith_fixed(Ctrl_key_family_recordedBy_recordNumber, "_NA"),
        Ctrl_key_family_recordedBy_recordNumber := stri_replace_last_regex(
          Ctrl_key_family_recordedBy_recordNumber, "_NA$", "_")]
  }


  # Step 1: Pre-compute grouping key patterns using vectorized string operations
  # This identifies records that cannot be grouped due to missing information
  message("Pre-compute grouping key patterns...")
  occ[, grouping_pattern := fcase(
    # Condition 1: FAMILY__ (ends with __, with neither recordedBy nor recordNumber)
    stri_detect_fixed(Ctrl_key_family_recordedBy_recordNumber, "__") &
      stri_locate_last_fixed(Ctrl_key_family_recordedBy_recordNumber, "__")[,2] ==
      stri_length(Ctrl_key_family_recordedBy_recordNumber),
    "FAMILY__",

    # Condition 2: FAMILY_recordedBy_ (contains "__" but not at the end, or contains UNKNOWN-COLLECTOR)
    (stri_detect_fixed(Ctrl_key_family_recordedBy_recordNumber, "__") &
       stri_locate_last_fixed(Ctrl_key_family_recordedBy_recordNumber, "__")[,2] !=
       stri_length(Ctrl_key_family_recordedBy_recordNumber)) |
      stri_detect_fixed(Ctrl_key_family_recordedBy_recordNumber, "UNKNOWN-COLLECTOR")|
      stri_detect_fixed(Ctrl_key_family_recordedBy_recordNumber, "UNKNOWN"),
    "FAMILY_recordedBy_",

    # Condition 3: FAMILY__recordNumber (ends with a single _, not __)
    stri_sub(Ctrl_key_family_recordedBy_recordNumber, -1) == "_" &
      stri_sub(Ctrl_key_family_recordedBy_recordNumber, -2, -2) != "_",
    "FAMILY__recordNumber",

    # Default: Groupable
    default = "groupable"
  )]

  # Create logic flag column
  occ[, `:=`(
    FAMILY__ = grouping_pattern == "FAMILY__",
    FAMILY_recordedBy_ = grouping_pattern == "FAMILY_recordedBy_",
    FAMILY__recordNumber = grouping_pattern == "FAMILY__recordNumber",
    is_non_groupable = grouping_pattern != "groupable"
  )]

  # Step 2: Handle non-groupable records in batch
  message("Handle non-groupable records")
  # These records are treated as individual samples with no duplicates
  occ[is_non_groupable == TRUE, `:=`(
    UltraGBIF_digital_voucher = TRUE,
    UltraGBIF_non_groupable_duplicates = TRUE,
    UltraGBIF_duplicates = FALSE,
    UltraGBIF_num_duplicates = 1L,

    # Set taxon information based on acceptance status
    UltraGBIF_wcvp_plant_name_id = fifelse(wcvp_taxon_status == "Accepted",
                                           as.character(wcvp_plant_name_id),
                                           ""),
    UltraGBIF_sample_taxon_name = fifelse(wcvp_taxon_status == "Accepted",
                                          as.character(wcvp_taxon_name),
                                          ""),
    UltraGBIF_unidentified_sample = fifelse(wcvp_taxon_status == "Accepted", FALSE, TRUE),
    UltraGBIF_number_taxon_names = fifelse(wcvp_taxon_status == "Accepted", 1L, 0L),
    UltraGBIF_sample_taxon_name_status = fifelse(wcvp_taxon_status == "Accepted",
                                                 "identified",
                                                 "unidentified"),

    # Set coordinate information based on validation status
    UltraGBIF_decimalLatitude = fifelse(Ctrl_coordinates_validated_by_gbif_issue == TRUE,
                                        Ctrl_decimalLatitude,
                                        NA_real_),
    UltraGBIF_decimalLongitude = fifelse(Ctrl_coordinates_validated_by_gbif_issue == TRUE,
                                         Ctrl_decimalLongitude,
                                         NA_real_),
    UltraGBIF_useful_for_spatial_analysis = Ctrl_coordinates_validated_by_gbif_issue,

    # Set grouping status description
    UltraGBIF_duplicates_grouping_status = fcase(
      FAMILY__ == TRUE, "not groupable: no recordedBy and no recordNumber",
      FAMILY__recordNumber == TRUE, "not groupable: no recordNumber",
      FAMILY_recordedBy_ == TRUE, "not groupable: no recordedBy",
      default = "not groupable"
    )
  )]

  # Step 3: Process groupable records
  message("Process groupable records...")
  # Set key for efficient grouping operations
  setkey(occ, Ctrl_key_family_recordedBy_recordNumber)

  # Calculate group statistics and identify digital vouchers
  occ[is_non_groupable == FALSE,
      num_duplicates := .N,
      by = Ctrl_key_family_recordedBy_recordNumber]

  occ[is_non_groupable == FALSE,
      max_info_score := max(Ctrl_moreInformativeRecord),
      by = Ctrl_key_family_recordedBy_recordNumber]

  occ[is_non_groupable == FALSE,
      UltraGBIF_digital_voucher := Ctrl_moreInformativeRecord == max_info_score &
        .I == .I[which.max(Ctrl_moreInformativeRecord)],
      by = Ctrl_key_family_recordedBy_recordNumber]

  # Set basic grouping information
  occ[is_non_groupable == FALSE, `:=`(
    UltraGBIF_duplicates_grouping_status = "groupable",
    UltraGBIF_duplicates = num_duplicates > 1,
    UltraGBIF_num_duplicates = num_duplicates
  )]

  # Step 4: Process taxonomic information for groupable records
  message("Process taxonomic information for groupable records...")
  # Create combined taxon identifier for accurate counting
  occ[, taxon_id := paste0(wcvp_taxon_name, ";", wcvp_plant_name_id)]

  # Count accepted taxon names per group
  occ[is_non_groupable == FALSE,
      num_unique_taxa := uniqueN(taxon_id[wcvp_taxon_status == "Accepted" & !is.na(wcvp_taxon_name)]),
      by = Ctrl_key_family_recordedBy_recordNumber]

  # Select the most frequent taxon for the sample
  occ[is_non_groupable == FALSE,
      selected_taxon := {
        accepted <- taxon_id[wcvp_taxon_status == "Accepted" & !is.na(wcvp_taxon_name)]
        if(length(accepted) > 0) names(which.max(table(accepted))) else NA_character_ },
      by = Ctrl_key_family_recordedBy_recordNumber]

  # Split the selected taxon back into name and ID
  occ[is_non_groupable == FALSE, `:=`(
    # Split Name and ID
    UltraGBIF_sample_taxon_name = fifelse(
      !is.na(selected_taxon),
      stri_split_fixed(selected_taxon, ";", simplify = TRUE)[,1],
      ""
    ),
    UltraGBIF_wcvp_plant_name_id = fifelse(
      !is.na(selected_taxon),
      stri_split_fixed(selected_taxon, ";", simplify = TRUE)[,2],
      ""
    ),

    # Set the number of category names
    UltraGBIF_number_taxon_names = fifelse(is.na(num_unique_taxa), 0L, num_unique_taxa),

    # Set identification status based on the number of unique names
    UltraGBIF_sample_taxon_name_status = fcase(
      is.na(selected_taxon), "unidentified",
      num_unique_taxa == 1L, "identified",
      num_unique_taxa > 1L, "divergent identifications",
      default = "unidentified"
    ),

    # Set unidentified flag
    UltraGBIF_unidentified_sample = is.na(selected_taxon)
  )]

  # Step 5: Assign coordinates for groupable records
  message("Assign coordinates for groupable records...")
  # Create coordinate priority score (higher for digital voucher and better quality)
  occ[is_non_groupable == FALSE,
      coord_priority := fifelse(
        Ctrl_coordinates_validated_by_gbif_issue == TRUE,
        UltraGBIF_digital_voucher * 1000000 + Ctrl_geospatial_quality,
        -1
      )]

  # Get the coordinates from the record with highest priority
  occ[is_non_groupable == FALSE,
      `:=`(
        best_lat = fifelse(
          any(Ctrl_coordinates_validated_by_gbif_issue == TRUE),
          Ctrl_decimalLatitude[which.max(coord_priority)],
          NA_real_
        ),
        best_lon = fifelse(
          any(Ctrl_coordinates_validated_by_gbif_issue == TRUE),
          Ctrl_decimalLongitude[which.max(coord_priority)],
          NA_real_
        )
      ),
      by = Ctrl_key_family_recordedBy_recordNumber]

  # Assign the selected coordinates to all records in the group
  occ[is_non_groupable == FALSE, `:=`(
    UltraGBIF_decimalLatitude = best_lat,
    UltraGBIF_decimalLongitude = best_lon,
    UltraGBIF_useful_for_spatial_analysis = !is.na(best_lat)
  )]

  # Step 6: Clean up temporary columns
  message("Clean up temps...")
  occ[, `:=`(
    grouping_pattern = NULL,
    FAMILY__ = NULL,
    FAMILY_recordedBy_ = NULL,
    FAMILY__recordNumber = NULL,
    is_non_groupable = NULL,
    num_duplicates = NULL,
    max_info_score = NULL,
    taxon_id = NULL,
    num_unique_taxa = NULL,
    selected_taxon = NULL,
    coord_priority = NULL,
    best_lat = NULL,
    best_lon = NULL
  )]


  # Select result columns
  occ_results <- occ[, .(Ctrl_gbifID,
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
                         UltraGBIF_decimalLongitude)] %>% setorder(Ctrl_gbifID)


  # ============================================================================
  # SECTION 8: Combine with Original Data and Add Final Classifications
  # ============================================================================
  message("Merging...")

  {
    # Combine all data
    occ_all <- cbind(occ_in, occ_wcvp_check_name, occ_collectorsDictionary, occ_results) %>% setDT()
    col_tmp <- unique(names(occ_all))
    occ_all <- occ_all[, ..col_tmp]

    # Add final dataset result classification
    occ_all[, UltraGBIF_dataset_result := fcase(
      UltraGBIF_digital_voucher == TRUE &
        UltraGBIF_unidentified_sample == FALSE &
        UltraGBIF_useful_for_spatial_analysis == TRUE , "usable",
      UltraGBIF_digital_voucher == FALSE, "duplicate",
      UltraGBIF_digital_voucher == TRUE &
        (UltraGBIF_unidentified_sample == TRUE |
           UltraGBIF_useful_for_spatial_analysis == FALSE |
           is.na(UltraGBIF_decimalLongitude) == TRUE), "unusable"
    )]

    # Merge with WCVP accepted name details
    name_tmp <- occ_wcvp_check_name[, .(wcvp_plant_name_id = as.character(wcvp_plant_name_id),
                                        wcvp_taxon_rank,
                                        wcvp_taxon_status,
                                        wcvp_family,
                                        wcvp_taxon_name,
                                        wcvp_taxon_authors,
                                        wcvp_reviewed)] %>%
      unique(by = "wcvp_plant_name_id") %>%
      setnames(paste0('UltraGBIF_', names(.))) %>%
      stats::na.omit()

    occ_all <- merge(occ_all, name_tmp, by = 'UltraGBIF_wcvp_plant_name_id', all.x = TRUE)

    # Select final columns
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
                      "UltraGBIF_decimalLongitude", "UltraGBIF_dataset_result", "UltraGBIF_wcvp_plant_name_id",
                      "UltraGBIF_wcvp_taxon_rank", "UltraGBIF_wcvp_taxon_status",
                      "UltraGBIF_wcvp_family", "UltraGBIF_wcvp_taxon_name",
                      "UltraGBIF_wcvp_taxon_authors", "UltraGBIF_wcvp_reviewed")

    occ_all <- occ_all[, ..cols_to_keep]
  }

  # ============================================================================
  # SECTION 9: Return Results
  # ============================================================================

  end <- Sys.time()

  print(end-start)

  return(list(
    occ_digital_voucher = occ_all,
    occ_results = occ_results,
    used_time = end - start
  ))
}

# ============================================================================
# PERFORMANCE NOTES
# ============================================================================
#
# This optimized version eliminates the O(n²) complexity by:
#
# 1. Removing the foreach loop that processed each collection event separately
# 2. Using data.table's grouped operations (.SD, by=) for vectorized processing
# 3. Pre-calculating all flags and classifications in vectorized operations
# 4. Using efficient merge operations instead of iterative rbindlist
#
# Expected performance improvements:
# - Time complexity: O(n²) → O(n log n)
# - Memory usage: Reduced (no intermediate list storage)
# - Speed: 10-100x faster depending on dataset size
#
# Key optimizations:
# - Section 3: Vectorized identification of non-groupable records
# - Section 4: Group-wise selection of digital vouchers using which.max
# - Section 5: Efficient aggregation of taxon names per collection event
# - Section 6: Vectorized coordinate selection with priority logic
#
# ============================================================================


