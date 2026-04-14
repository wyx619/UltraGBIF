#' @title Standardize and extract primary collector names in batch
#' @name check_collectors
#'
#' @description This module simplifies and extracts the name of the primary collector from the `recordedBy` field,
#' then constructs a standardized collector dictionary to mitigate inconsistencies that may fragment
#' single collection events. By reducing identification errors, this module enhances the accuracy of
#' subsequent duplication checks.
#'
#' The function processes the `recordedBy` field through the following steps:
#' \itemize{
#'   \item \strong{Text normalization}: Converts non-ASCII characters to ASCII equivalents, removes special
#'   characters, and standardizes formatting (e.g., uppercase conversion, whitespace normalization)
#'   \item \strong{Primary collector extraction}: Identifies and extracts the most informative token
#'   (typically the longest word, representing the primary collector's surname) from multi-collector entries
#'   \item \strong{Dictionary construction}: Compiles a unique mapping between raw `recordedBy` strings
#'   and their standardized collector names, facilitating consistent representation across the dataset
#' }
#'
#' @param occ_import imported GBIF records from \code{\link{import_records}}
#' @param min_char minimum number of characters required for a valid collector name.
#'   Names shorter than this threshold are classified as "UNKNOWN". See \code{\link{get_collectors_name}} for details.
#'
#' @details
#' \strong{Processing Logic:}
#'
#' The function applies \code{\link{get_collectors_name}} to each unique `recordedBy` entry, which performs:
#' \enumerate{
#'   \item \strong{Character encoding normalization}: Transliterates Cyrillic and other non-Latin scripts
#'   to Latin characters, then converts to ASCII
#'   \item \strong{Special character removal}: Strips punctuation, symbols, and institutional keywords
#'   (e.g., "HERBARIUM", "EXPEDITION", "ET AL")
#'   \item \strong{Primary name selection}: Extracts the longest token from the cleaned string,
#'   assuming it represents the primary collector's surname
#'   \item \strong{Threshold filtering}: Names shorter than `min_char` characters or matching a
#'   predefined exclusion list are classified as "UNKNOWN"
#' }
#'
#'
#'
#'
#' @return UltraGBIF_collectors_dictionary list with runtime and data.table:
#'   \itemize{
#'     \item \code{collectors_dictionary}: A data.table mapping raw `recordedBy` strings to standardized collector names.
#'     \item \code{runtime}: Execution time of the function
#'   }
#'
#'  The \code{collectors_dictionary} contains two columns:
#'    \itemize{
#'      \item \code{Ctrl_recordedBy}: Original raw collector strings from GBIF
#'      \item \code{Ctrl_nameRecordedBy_Standard}: Standardized primary collector names (uppercase, ASCII-only)
#'    }
#'
#' @importFrom dplyr %>% filter mutate select distinct case_when if_else
#' @import stringi
#' @import data.table
#'
#' @examples
#' \dontrun{
#' collectors_dictionary <- check_collectors(occ_import = occ_import,
#' min_char = 2)
#'}
#' @export
check_collectors <- function(occ_import = NA,
                             min_char = 2)
{
  start = Sys.time()

  occ = occ_import$occ %>% setDT()

  if (NROW(occ) == 0) {
    stop("Occurrence is empty!")
  }

  get_collectors_name_vec <- function(x, min_char = min_char) {
    vapply(
      X = x,
      FUN = get_collectors_name,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      min_char = min_char
    )
  }
  recordedBy_Std <- occ[, .(Ctrl_recordedBy)] %>% unique()
  recordedBy_Std[, Ctrl_nameRecordedBy_Standard := get_collectors_name_vec(x = Ctrl_recordedBy, min_char = min_char) %>% stri_trans_toupper()]

  end = Sys.time()
  used = end - start
  message(paste('used', used %>% round(1), attributes(used)$units))
  Dictionary <- list(collectors_dictionary = recordedBy_Std,
                     runtime = end - start)
  class(Dictionary) <- 'UltraGBIF_collectors_dictionary'
  return(Dictionary)
}
