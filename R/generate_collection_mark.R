#' @title Generate unique collection event
#'
#' @name generate_collection_mark
#'
#' @description It creates a key to identify the physical and digital duplicates of a given collection event.
#' It combines the last name of primary collector with the collector number and the botanical family (family +
#' recordByStandardized + recordNumber_Standard) that groups the duplicates of the same unique collection event.
#' It also identifies new collectors to be added to the collector dictionary and that can be reused in the future.
#'
#' Include **`recordedByStandardized`** field with verified last name of the primary collector.
#'
#' Include **`recordNumber_Standard`** field with only numbers from **`recordNumber`**.
#'
#' Create the collection event key to group duplicates in the **`key_family_recordedBy_recordNumber`** field
#' following the fields:
#'
#' **`family` + `recordedByStandardized` + `recordNumber_Standard`**.
#'
#' @param occ_import imported GBIF records
#' @param dictionary your processed dictionary from `prepare_collectors_dictionary`
#'
#' @details Fields created for each incident record:
#'
#' `nameRecordedBy_Standard`
#'
#' `recordNumber_Standard`
#'
#' `key_family_recordedBy_recordNumber`
#'
#' `key_year_recordedBy_recordNumber`
#'
#' #' A critical step in parsing duplicate records involves generating a robust key for each unique collecting
#' event (i.e., *gathering*) to facilitate duplicate recognition. To achieve this, a string is created by
#' concatenating the **taxon family**, the **last name of the primary collector**, and the **collection number**.
#'
#' @return A list with duration and 3 data.table:
#' "occ_collectorsDictionary" for update result fields only, "summary" for summary and
#' "CollectorsDictionary_add" for new collectors that can be added to the collector dictionary
#' that can be reused in the future.
#'
#' @encoding UTF-8
#'
#' @examples
#' \donttest{
#' help(generate_collection_mark)
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

  collectorDictionary_checked_new <- collectorDictionary_checked[!collectorDictionary, on = 'Ctrl_recordedBy']

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
  print(end-start)
  return(list(occ_collectorsDictionary = occ_collectorsDictionary,
              summary = summary,
              collectorsDictionary_add = collectorDictionary_checked_new,
              used_time = end-start))
}

