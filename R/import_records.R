#' @title Import GBIF occurrence records
#' @name import_records
#'
#' @description Load initial GBIF records and extract issues for downstream analysis.
#'
#' @param GBIF_file path to the downloaded zip file from GBIF
#' @param only_PRESERVED_SPECIMEN if TRUE, occurrence records are filtered by `basisOfRecord="PRESERVED_SPECIMEN"`
#'
#' @details This step is the beginning of UltraGBIF progress, which makes downloaded file into initial GBIF records.
#' @seealso
#'  \itemize{
#'   \item \code{\link{unzip}} for automatically decompressing
#'   \item Saxifraga occurrence records: \doi{https://doi.org/10.15468/dl.bythb4}
#' }
#'
#' @return UltraGBIF_import list with duration and 3 data.table: "occ" for processed occurrence data,"occ_gbif_issue" for checked GBIF issues and "summary" for import summary.
#'
#'
#' @import data.table
#' @import stringi
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' ## run as example for Saxifraga occurrence records
#' library(UltraGBIF)
#' url <- 'https://api.gbif.org/v1/occurrence/download/request/0021523-250402121839773.zip'
#' gbif_occurrence_file <- paste0(getwd(),'/Saxfriga.zip')
#' curl::curl_download(url,gbif_occurrence_file,quiet = F) ## cost time and total are 68692523 bytes
#' occ_import <- import_records(GBIF_file = gbif_occurrence_file,
#'                             only_PRESERVED_SPECIMEN = T)
#' head(occ_import$summary,5)
#'}
#' @export
import_records<-function(GBIF_file = '',only_PRESERVED_SPECIMEN=F)
{
  start=Sys.time()

  if (!is.character(GBIF_file)) stop('should be path!')
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
  } else {stop('should be a DwC zip file!')}

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
                     duration = used)
  class(occ_import) <- "UltraGBIF_import"
  return(occ_import)
}
