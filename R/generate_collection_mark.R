#' @title Generate unique collection event
#'
#' @name generate_collection_mark
#'
#' @description This function creates a key to identify the physical and digital duplicates of a given collection event.
#' It combines the last name of primary collector with the collector number and the botanical family that groups the duplicates of the same unique collection event.
#'
#' Include **`recordedByStandardized`** field with verified last name of the primary collector and **`recordNumber_Standard`** field with only numbers from **`recordNumber`**.
#'
#' This function creates the collection event key to group duplicates in the **`key_family_recordedBy_recordNumber`** field
#' following the fields: **`family` + `recordedByStandardized` + `recordNumber_Standard`**.
#'
#' @param occ_import imported GBIF records from \code{\link{import_records}}
#' @param dictionary dictionary from \code{\link{prepare_collectors_dictionary}}
#'
#' @details Fields created for each incident record: `nameRecordedBy_Standard`, `recordNumber_Standard`, `key_family_recordedBy_recordNumber`, `key_year_recordedBy_recordNumber`
#'
#' To parse duplicate records involves generating a robust key for each unique collecting
#' event (i.e., *gathering*) to facilitate duplicate recognition. By concatenating the **taxon family**,
#' the **last name of the primary collector**, and the **collection number**.
#'
#' @return UltraGBIF_collection_key list with duration and 2 data.table:
#' `occ_collectorsDictionary` for update result fields only, `summary` for its summary.
#'
#' @encoding UTF-8
#'
#' @examples
#' \dontrun{
#' collection_key <- generate_collection_mark(occ_import = occ_import,
#' dictionary = dictionary)
#' }
#'
#' @import data.table
#' @importFrom dplyr %>% filter mutate select distinct case_when if_else
#' @import stringi
#'
#' @export
generate_collection_mark <- function(occ_import = NA,
                                     dictionary = NA)
{
  start <- Sys.time()

  occ=occ_import$occ%>%setDT()
  occ=occ[,Ctrl_recordedBy:=stri_trans_toupper(Ctrl_recordedBy)][
    ,.(Ctrl_gbifID,Ctrl_recordNumber, Ctrl_family, Ctrl_recordedBy, Ctrl_year)]

  collectorDictionary_checked <- dictionary$my_dictionary[,.(
    Ctrl_recordedBy = stri_trans_toupper(Ctrl_recordedBy),
    Ctrl_nameRecordedBy_Standard = stri_trans_toupper(Ctrl_nameRecordedBy_Standard))]%>%
    unique(by = "Ctrl_recordedBy")

  collectorDictionary_checked <- collectorDictionary_checked[is.na(Ctrl_nameRecordedBy_Standard),Ctrl_nameRecordedBy_Standard:=""]

  collectorDictionary <- dictionary$ref_dictionary[
    ,.(Ctrl_recordedBy, Ctrl_nameRecordedBy_Standard_CNCFlora = Ctrl_nameRecordedBy_Standard_x)]

  #collectorDictionary_checked_new <- collectorDictionary_checked[!collectorDictionary, on = 'Ctrl_recordedBy']

  occ <- merge(occ,
               collectorDictionary_checked,
               by = "Ctrl_recordedBy",all.x = TRUE )

  occ[, Ctrl_recordNumber_Standard := stri_replace_all_regex(Ctrl_recordNumber, "[^0-9]", "")]
  occ[, Ctrl_recordNumber_Standard := fifelse(
    is.na(Ctrl_recordNumber_Standard) | Ctrl_recordNumber_Standard == "",
    "", Ctrl_recordNumber_Standard%>%strtoi()%>%as.character()
    )]

  occ[, `:=`(
    Ctrl_key_family_recordedBy_recordNumber = paste(
      stri_trans_toupper(stri_trim(Ctrl_family)),
      Ctrl_nameRecordedBy_Standard,
      Ctrl_recordNumber_Standard,
      sep = "_"
    ),
    Ctrl_key_year_recordedBy_recordNumber = paste(
      fifelse(is.na(Ctrl_year), "noYear", stri_trim(Ctrl_year)),
      Ctrl_nameRecordedBy_Standard,
      Ctrl_recordNumber_Standard,
      sep = "_"
    )
  )]

  occ_collectorsDictionary = occ[,.(Ctrl_gbifID,
                                    Ctrl_nameRecordedBy_Standard,
                                    Ctrl_recordNumber_Standard,
                                    Ctrl_key_family_recordedBy_recordNumber,
                                    Ctrl_key_year_recordedBy_recordNumber)]

  summary <- occ[, .N, by = paste0(Ctrl_key_family_recordedBy_recordNumber)] %>%
    setnames(c('Key', 'number_of_Records')) %>%
    setorder(-number_of_Records)

  end=Sys.time()
  used=end-start
  message(paste('used',used%>%round(1),attributes(used)$units))
  collection_key <- list(occ_collectorsDictionary = occ_collectorsDictionary,
              summary = summary,
              #collectorsDictionary_add = collectorDictionary_checked_new,
              duration = end-start)
  class(collection_key) <- 'UltraGBIF_collection_key'
  return(collection_key)
}

