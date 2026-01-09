#' @title Import GBIF occurrence records
#' @name import_records
#'
#' @description Returns a list contains processed GBIF records and useful issues for downstream analysis.
#'
#' @param GBIF_file GBIF occurrence Darwin Core Archive. See details for more information
#' @param only_PRESERVED_SPECIMEN if TRUE, occurrence records are filtered by `basisOfRecord="PRESERVED_SPECIMEN"`
#'
#' @details GBIF_file is a path to your Darwin Core standard file which is downloaded from GBIF.
#' The Darwin Core Archive (DwC-A) is a compact package (a ZIP file) contains interconnected text files and enables data publishers to share their data using a common terminology.
#' GBIF_file could also be a path to "occurrence.txt" which is decompressed from your your Darwin Core standard file.
#'
#' @return A list with duration and 3 data.table: "occ" for processed occurrence data,"occ_gbif_issue" for checked GBIF issues and "summary" for import summary.
#'
#'
#' @import data.table
#' @import stringi
#' @importFrom dplyr %>%
#'
#' @examples
#' \donttest{
#'
#' help(import_records)
#'
#'
#'}
#' @export
import_records<-function(GBIF_file = '',only_PRESERVED_SPECIMEN=F)
{


  ex_path=dirname(GBIF_file)
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

  if (tools::file_ext(GBIF_file)=="zip"){
    message("Decompressing")
    utils::unzip(GBIF_file, exdir = ex_path, files = "occurrence.txt",
          list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, unzip = "internal", setTimes = FALSE)
    GBIF_file=paste0(ex_path,"/occurrence.txt")
  }
  start=Sys.time()
  # Read data using fread
  message("Loading records")
  occ <- fread(GBIF_file,
               sep = '\t',
               encoding = 'UTF-8',
               select = col_sel,
               col.names = paste0('Ctrl_',col_sel),
               quote="",
               showProgress = FALSE)[is.na(Ctrl_hasCoordinate), Ctrl_hasCoordinate := FALSE][,Ctrl_gbifID:=as.character(Ctrl_gbifID)]
  unlink(GBIF_file)

  if (only_PRESERVED_SPECIMEN) {
    occ <- occ[Ctrl_basisOfRecord=="PRESERVED_SPECIMEN",]
  }

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
  print(used)

  return(list(occ=occ,
              occ_gbif_issue=occ_gbif_issue,
              summary=summary,
              used_time=used))
}
