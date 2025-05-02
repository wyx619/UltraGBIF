#' @title (Step 1) Import the GBIF occurrence Darwin Core Archive data
#' @name import_GBIF_archive
#'
#' @description Returns a list contains processed GBIF occurrence data and GBIF issue data which is useful for downstream analysis.
#'
#' @param gbif_occurrence_file GBIF occurrence Darwin Core Archive data. See details for more information.
#' @param only_PRESERVED_SPECIMEN if TRUE, occurrence data will be filtered by `Ctrl_basisOfRecord="PRESERVED_SPECIMEN"`
#'
#' @details gbif_occurrence_file might be a path to your Darwin Core standard file which is downloaded from GBIF.
#' The Darwin Core Archive (DwC-A) is a compact package (a ZIP file) contains interconnected text files and enables data publishers to share their data using a common terminology.
#' gbif_occurrence_file could also be a path to "occurrence.txt" which is decompressed from your your Darwin Core standard file.
#'
#' @return A list with timer and 3 data.table: "occ" for processed occurrence data,"occ_gbif_issue" for checked GBIF issues and "summary" for import summary.
#'
#'
#' @import data.table
#' @import stringi
#' @importFrom dplyr %>%
#'
#' @examples
#' \donttest{
#'
#' help(import_GBIF_archive)
#'
#'
#'}
#' @export
import_GBIF_archive<-function(gbif_occurrence_file = '',only_PRESERVED_SPECIMEN=F)
{
  start=Sys.time()

  ex_path=dirname(gbif_occurrence_file)
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

  if (tools::file_ext(gbif_occurrence_file)=="zip"){
    utils::unzip(gbif_occurrence_file, exdir = ex_path, files = "occurrence.txt",
          list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, unzip = "internal", setTimes = FALSE)
    gbif_occurrence_file=paste0(ex_path,"/occurrence.txt")
  }

  # Read data using fread
  occ <- fread(gbif_occurrence_file,
               sep = '\t',
               encoding = 'UTF-8',
               select = col_sel,
               col.names = paste0('Ctrl_',col_sel),
               quote="",
               showProgress = FALSE)[is.na(Ctrl_hasCoordinate), Ctrl_hasCoordinate := FALSE][,Ctrl_gbifID:=as.character(Ctrl_gbifID)]
  unlink(gbif_occurrence_file)
  message("load fine")
  if (only_PRESERVED_SPECIMEN) {
    occ <- occ[Ctrl_basisOfRecord=="PRESERVED_SPECIMEN",]
  }
  # extract_gbif_issue
  fix=function(.) stri_detect_fixed(occ[,Ctrl_issue], .)

  EnumOccurrenceIssue <- EnumOccurrenceIssue
  issue_key = EnumOccurrenceIssue[,constant]

  occ_gbif_issue <- sapply(issue_key, fix)%>%as.data.table()%>%setnames(issue_key)

  summary <- occ_gbif_issue[, lapply(.SD, sum)]%>%
    transpose()%>%setnames("n_occ")%>%cbind(issue_key)%>%setorder(-n_occ)

  occ_gbif_issue <- cbind(occ_gbif_issue,occ[,.(Ctrl_gbifID)])

  end=Sys.time()
  used=end-start
  print(used)

  return(list(occ=occ,
              occ_gbif_issue=occ_gbif_issue,
              summary=summary,
              used_time=used))
}
