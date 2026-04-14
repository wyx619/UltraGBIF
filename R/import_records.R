#' @title Import GBIF occurrence records
#' @name import_records
#'
#' @description This function loads initial GBIF occurrence records from a downloaded zip file
#' and automatically extracts GBIF issue flags for downstream quality assessment. It serves as
#' the entry point of the UltraGBIF workflow, transforming raw GBIF data into a structured
#' format ready for taxonomic and spatial processing.
#'
#' The function implements the following workflow:
#' \itemize{
#'   \item \strong{File validation}: Verifies that the input is a valid GBIF zip file
#'   \item \strong{Decompression}: Automatically extracts the \code{occurrence.txt} file from
#'   the GBIF download archive
#'   \item \strong{Data loading}: Reads occurrence records using \code{fread} with optimized
#'   column selection (60 core fields including taxonomy, geography, collection metadata,
#'   and GBIF issues)
#'   \item \strong{Issue extraction}: Parses the \code{issue} field to identify all GBIF
#'   quality flags (e.g., \code{COORDINATE_ROUNDED}, \code{COUNTRY_COORDINATE_MISMATCH},
#'   \code{BASIS_OF_RECORD_INVALID}) for each record
#'   \item \strong{Summary compilation}: Generates a frequency table of all detected issues,
#'   sorted by occurrence count
#' }
#'
#' @param GBIF_file path to the zip file downloaded from GBIF. Must be a valid \code{.zip} file
#'   containing an \code{occurrence.txt} file.
#' @param only_PRESERVED_SPECIMEN logical; if \code{TRUE}, filters occurrence records to retain
#'   only those with \code{basisOfRecord = "PRESERVED_SPECIMEN"}. Default is \code{FALSE}.
#'
#' @details
#' \strong{Column Selection:}
#'
#' The function loads core fields from the GBIF occurrence dataset, including:
#' \itemize{
#'   \item \strong{Identifiers}: \code{gbifID}, \code{occurrenceID}, \code{catalogNumber},
#'   \code{recordNumber}
#'   \item \strong{Taxonomy}: \code{scientificName}, \code{family}, \code{taxonRank},
#'   \code{taxonomicStatus}, \code{verbatimIdentification}
#'   \item \strong{Geography}: \code{decimalLatitude}, \code{decimalLongitude}, \code{countryCode},
#'   \code{stateProvince}, \code{locality}, \code{verbatimLocality}
#'   \item \strong{Collection metadata}: \code{recordedBy}, \code{eventDate}, \code{year},
#'   \code{month}, \code{day}, \code{institutionCode}, \code{collectionCode}
#'   \item \strong{Quality flags}: \code{issue}, \code{hasCoordinate}, \code{hasGeospatialIssues}
#'   \item \strong{Others}: other useful fields.
#' }
#'
#' All column names are prefixed with \code{Ctrl_} to avoid naming conflicts with R reserved
#' words and to clearly distinguish UltraGBIF-processed fields.
#'
#' \strong{GBIF Issue Extraction:}
#'
#' The \code{issue} field in GBIF downloads contains a pipe-separated list of quality flags
#' (e.g., \code{"COORDINATE_ROUNDED|COUNTRY_COORDINATE_MISMATCH"}). This function parses
#' these flags using \code{stringi::stri_detect_fixed()} to create a binary indicator matrix
#' where each column represents a specific issue type and each row indicates whether that
#' issue is present for the corresponding record.
#'
#' Common issue types include:
#' \itemize{
#'   \item \code{COORDINATE_ROUNDED}: Coordinates have been rounded to fewer decimal places
#'   \item \code{COUNTRY_COORDINATE_MISMATCH}: Coordinate does not match the recorded country
#'   \item \code{COORDINATE_UNCERTAINTY_METERS}: Coordinate uncertainty is specified
#'   \item \code{GEODETIC_DATUM_ASSUMED_WGS84}: Geodetic datum was assumed to be WGS84
#'   \item \code{BASIS_OF_RECORD_INVALID}: Basis of record is not a recognized value
#' }
#'
#' \strong{Performance Considerations:}
#'
#' The function uses \code{data.table::fread()} for fast file reading, which is significantly
#' faster than \code{read.csv()} for large GBIF datasets. Column selection (\code{select}
#' parameter) reduces memory usage by loading only the fields required for downstream processing.
#'
#' @return UltraGBIF_import list containing:
#'   \itemize{
#'     \item \code{occ}: A data.table containing the processed occurrence records with all
#'     column names prefixed by \code{Ctrl_}
#'     \item \code{occ_gbif_issue}: A data.table containing binary indicators for each GBIF
#'     issue type (one column per issue), plus the \code{Ctrl_gbifID} column for record linkage
#'     \item \code{summary}: A data.table summarizing the frequency of each GBIF issue type,
#'     sorted in descending order by occurrence count
#'     \item \code{runtime}: Execution time of the function
#'   }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{unzip}} for automatic decompression of GBIF downloads
#'   \item Saxifraga occurrence records example: \doi{10.15468/dl.bythb4}
#' }
#'
#' @import data.table
#' @import stringi
#' @importFrom dplyr %>%
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' # Download and import Saxifraga occurrence records from GBIF
#' url <- 'https://api.gbif.org/v1/occurrence/download/request/0021523-250402121839773.zip'
#' gbif_occurrence_file <- paste0(getwd(), '/Saxifraga.zip')
#' curl::curl_download(url, gbif_occurrence_file, quiet = FALSE)
#'
#' # Import records (filtered to preserved specimens only)
#' occ_import <- import_records(GBIF_file = gbif_occurrence_file,
#'                              only_PRESERVED_SPECIMEN = TRUE)
#'
#' # Or use file.choose() to choose the zip file
#' occ_import <- import_records(GBIF_file = file.choose(),
#'                              only_PRESERVED_SPECIMEN = TRUE)
#'
#' # View summary of detected GBIF issues
#' head(occ_import$summary, 5)
#' }
#'
#' @export
import_records<-function(GBIF_file = '',only_PRESERVED_SPECIMEN=F)
{
  start=Sys.time()

  if (!is.character(GBIF_file)) stop('set path to GBIF zip file!')
  if (is.character(GBIF_file)){if (GBIF_file == '') stop('require GBIF_file!')}

  col_sel <- c("gbifID", "bibliographicCitation", "language", "institutionCode",
               "collectionCode", "datasetName", "basisOfRecord", "informationWithheld",
               "dataGeneralizations", "occurrenceID", "catalogNumber", "recordNumber",
               "recordedBy", "georeferenceVerificationStatus", "occurrenceStatus",
               "eventDate", "year", "month", "day", "habitat", "fieldNotes",
               "eventRemarks", "locationID", "higherGeography", "islandGroup",
               "island", "countryCode", "stateProvince", "county", "municipality",
               "locality", "verbatimLocality", "locationRemarks", "decimalLatitude",
               "decimalLongitude", "verbatimCoordinateSystem", "verbatimIdentification",
               "identificationQualifier", "typeStatus", "identifiedBy", "dateIdentified",
               "scientificName", "family", "taxonRank", "nomenclaturalCode",
               "taxonomicStatus", "issue", "mediaType", "hasCoordinate", "hasGeospatialIssues",
               "verbatimScientificName", "level0Name", "level1Name", "level2Name",
               "level3Name")

  if(tools::file_ext(GBIF_file)=="zip"){
    message("Decompressing")
    # Automatically decompressing occurrence.txt
    ex_path=dirname(GBIF_file)
    utils::unzip(GBIF_file, exdir = ex_path, files = "occurrence.txt",
                 list = FALSE, overwrite = TRUE,
                 junkpaths = FALSE, unzip = "internal", setTimes = FALSE)
    GBIF_file=paste0(ex_path,"/occurrence.txt")
    # Read data using fread
    message("Loading records")
    occ <- fread(GBIF_file,
                 sep = '\t',
                 encoding = 'UTF-8',
                 select = col_sel,
                 col.names = paste0('Ctrl_',col_sel),
                 quote="",
                 showProgress = FALSE)
    occ[is.na(Ctrl_hasCoordinate), Ctrl_hasCoordinate := FALSE]
    occ[,Ctrl_gbifID:=as.character(Ctrl_gbifID)]
  } else {stop('should be a zip file from GBIF!')}

  if (only_PRESERVED_SPECIMEN==T) occ <- occ[Ctrl_basisOfRecord=="PRESERVED_SPECIMEN",]

  # extract_gbif_issue

  EnumOccurrenceIssue <- EnumOccurrenceIssue
  issue_keys = EnumOccurrenceIssue[,constant]

  message("Compiling GBIF issues")

  fix=function(issue) stri_detect_fixed(occ[,Ctrl_issue], issue)
  occ_gbif_issue <- sapply(issue_keys, fix) %>% as.data.table()

  summary <- data.table(
    issue_keys = issue_keys,
    n_occ = colSums(occ_gbif_issue)
  )[order(-n_occ)]

  occ_gbif_issue[,Ctrl_gbifID:=occ$Ctrl_gbifID]

  end=Sys.time()
  used=end-start

  message(paste('used',used%>%round(1),attributes(used)$units))

  occ_import <- list(occ = occ,
                     occ_gbif_issue = occ_gbif_issue,
                     summary = summary,
                     runtime = used)
  class(occ_import) <- "UltraGBIF_import"
  return(occ_import)
}
