#'
#'
#' Enumeration GBIF issue
#' An enumeration of validation rules for single occurrence records.
#'
#' There are many things that can go wrong and we continously encounter unexpected data.
#' In order to help us and publishers improve the data, we flag records with various issues
#' that we have encountered. This is also very useful for data consumers as you can include
#' these issues as filters in occurrence searches. Not all issues indicate bad data.
#' Some are merley flagging the fact that GBIF has altered values during processing.
#' On the details page of any occurrence record you will see the list of issues in the notice at the bottom.
#'
#' @format A data.table with 69 rows and 9 columns
#' \describe{
#'   \item{constant}{GBIF issue constant}
#'   \item{description}{GBIF issue description}
#'   \item{definition}{Our definition for classifying geographic issues}
#'   \item{type}{Type issue}
#'   \item{priority}{Impact of the issue for the use of geospatial information}
#'   \item{score}{Impact, in number, of the issue for the use of geospatial information}
#'   \item{selection_score}{Value used to calculate the quality of the geospatial information according to the classification of the issue}
#'   \item{reasoning}{Reasoning of the impact of the theme for the use of geospatial information}
#'   \item{notes}{Notes}
#' }
#' @source
#' * [GBIF Infrastructure: Data processing](https://www.gbif.org/article/5i3CQEZ6DuWiycgMaaakCo/gbif-infrastructure-data-processing)
#' * [An enumeration of validation rules for single occurrence records](https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html)
#' @keywords internal
"EnumOccurrenceIssue"


#'
#'
#' seas_ref
#'
#'
#'
#' @format A simple features object
#' \describe{
#'   \item{featurecla}{Land area}
#'   \item{geometry}{Geometry unit}
#' }
#' @source rnaturalearth::ne_download(scale = 110,type = 'land',category = 'physical',returnclass = "sf")
#' @keywords internal
"seas_ref"



#'
#'
#' ref_dictionary
#'
#' @format A data.table with 115702 rows and 2 columns
#' \describe{
#'   \item{Ctrl_recordedBy}{original collectors name}
#'   \item{Ctrl_nameRecordedBy_Standard}{processed collectors name}
#' }
#' @source
#' * [collectorDictionary](https://github.com/pablopains/parseGBIF/tree/main/collectorDictionary)
#' @keywords internal
"ref_dictionary"


