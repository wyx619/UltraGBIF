#' @title Generate unique collection event keys for duplicate identification
#'
#' @name set_collection_mark
#'
#' @description This function constructs a composite key to identify physical and digital duplicates
#' originating from the same collection event. By concatenating the taxon family name, the cleaned
#' primary collector name, and the collection number/ eventDate, it generates a robust identifier that groups
#' duplicate records associated with a single gathering event.
#'
#' The function performs the following operations:
#' \itemize{
#'   \item \strong{Collector name cleaning}: Merges occurrence records with the collector
#'   table from \code{\link{check_collectors}} to assign cleaned collector names
#'   \item \strong{Collection number normalization}: Extracts numeric components from the raw
#'   \code{recordNumber} field, removing non-numeric characters and converting to integer format
#'   \item \strong{Composite key generation}: Constructs a hierarchical key in the format
#'   \code{FAMILY_COLLECTORNAME_RECORDNUMBER} to uniquely identify collection events
#'   \item \strong{Key completeness classification}: Categorizes keys as "full" (complete with
#'   all three fields) or "incomplete" (missing any components)
#' }
#'
#' @param occ_import imported GBIF records from \code{\link{import_records}}
#' @param collectors_dictionary collectors dictionary from \code{\link{check_collectors}}
#'
#' @details
#' \strong{Key Construction Logic:}
#'
#' The composite key (\code{Ctrl_key}) is constructed by concatenating three components separated
#' by underscores:
#' \enumerate{
#'   \item \strong{Taxon family}: Uppercase botanical family name from the \code{Ctrl_family} field
#'   \item \strong{cleaned collector name}: Primary collector name from the collector
#'   table, or "UNKNOWN" if missing
#'   \item \strong{Standardized collection number}: Numeric-only extraction from \code{Ctrl_recordNumber};
#'   if absent, falls back to \code{Ctrl_eventDate} when month information is available
#' }
#'
#' \strong{Key Completeness Classification:}
#'
#' Keys are classified based on the presence and validity of their components:
#' \itemize{
#'   \item \strong{Full keys}: Keys that contains all three fields
#'   \item \strong{Incomplete keys}: Keys missing any one of the three fiels
#' }
#'
#' \strong{Fallback Strategy:}
#'
#' When the collection number is unavailable but month information exists, the function attempts
#' to use the event date (\code{Ctrl_eventDate}) as a surrogate identifier. This mitigates the
#' loss of grouping capability for records lacking explicit collection numbers.
#'
#' @return UltraGBIF_collection_key list containing:
#'   \itemize{
#'     \item \code{collection_key}: A data.table with columns \code{Ctrl_gbifID},
#'     \code{Ctrl_nameRecordedBy_Standard}, \code{Ctrl_recordNumber_Standard}, and \code{Ctrl_key},
#'     providing the key mapping for each occurrence record
#'     \item \code{full_keys}: A summary data.table of complete keys with their record counts,
#'     ordered by descending frequency
#'     \item \code{incomplete_keys}: A summary data.table of incomplete keys with their record counts,
#'     representing records with missing or unidentifiable components
#'     \item \code{runtime}: Execution time of the function
#'   }
#'
#' @encoding UTF-8
#'
#' @examples
#' \dontrun{
#' collectors_dictionary <- check_collectors(occ_import = occ_import,
#' min_char = 2)
#'
#' collection_key <- set_collection_mark(occ_import = occ_import,
#' collectors_dictionary = collectors_dictionary)
#' }
#' @references
#' De Melo, Pablo Hendrigo Alves, Nadia Bystriakova, Eve Lucas, and Alexandre K. Monro. 2024.
#'   "A New R Package to Parse Plant Species Occurrence Records into Unique Collection Events
#'   Efficiently Reduces Data Redundancy." \emph{Scientific Reports} 14 (1): 5450.
#'   \doi{10.1038/s41598-024-56158-3}.
#' @import data.table
#' @importFrom dplyr %>%
#' @import stringi
#'
#' @export
set_collection_mark <- function(occ_import = NA,
                                collectors_dictionary = NA)
{
  start <- Sys.time()

  occ=occ_import$occ%>%setDT()
  occ=occ[,Ctrl_recordedBy:=stri_trans_toupper(Ctrl_recordedBy)][
    ,.(Ctrl_gbifID,Ctrl_recordNumber, Ctrl_family, Ctrl_recordedBy, Ctrl_eventDate,Ctrl_month)]

  collectors_dictionary <- collectors_dictionary$collectors_dictionary[,.(
    Ctrl_recordedBy = stri_trans_toupper(Ctrl_recordedBy),
    Ctrl_nameRecordedBy_Standard = stri_trans_toupper(Ctrl_nameRecordedBy_Standard))]%>%
    unique(by = "Ctrl_recordedBy")

  collectors_dictionary[is.na(Ctrl_nameRecordedBy_Standard),Ctrl_nameRecordedBy_Standard:="UNKNOWN"]

  occ <- merge(occ,
               collectors_dictionary,
               by = "Ctrl_recordedBy",all.x = TRUE )

  occ[, Ctrl_recordNumber_Standard := stri_replace_all_regex(Ctrl_recordNumber, "[^0-9]", "")]
  occ[, Ctrl_recordNumber_Standard := fifelse(
    is.na(Ctrl_recordNumber_Standard) | Ctrl_recordNumber_Standard == "",
    "", Ctrl_recordNumber_Standard%>%strtoi()%>%as.character()
    )]
  occ[Ctrl_recordNumber_Standard==""&!is.na(Ctrl_month),Ctrl_recordNumber_Standard:=Ctrl_eventDate]
  occ[is.na(Ctrl_recordNumber_Standard),Ctrl_recordNumber_Standard:='']
  occ[, `:=`(
    Ctrl_key = paste(
      stri_trans_toupper(stri_trim(Ctrl_family)),
      Ctrl_nameRecordedBy_Standard,
      Ctrl_recordNumber_Standard,
      sep = "_"
    ))]

  collection_key = occ[,.(Ctrl_gbifID,
                          Ctrl_nameRecordedBy_Standard,
                          Ctrl_recordNumber_Standard,
                          Ctrl_key)]

  summary <- occ[, .N, by = paste0(Ctrl_key)] %>%
    setnames(c('Key', 'Records')) %>% setorder(-Records)

  full_keys <- summary[!stri_endswith_fixed(Key, "_") &
      !(vapply(strsplit(Key, "_"), `[`, "", 2) %chin%
          c("UNKNOWN", "CAPTURED", "ANONYMOUS"))]
  incomplete_keys <- fsetdiff(summary, full_keys, all=TRUE)

  end=Sys.time()
  used=end-start
  message(paste('used',used%>%round(1),attributes(used)$units))
  collection_key <- list(collection_key = collection_key,
                         full_keys = full_keys,
                         incomplete_keys = incomplete_keys,
                         runtime = end-start)
  class(collection_key) <- 'UltraGBIF_collection_key'
  return(collection_key)
}

