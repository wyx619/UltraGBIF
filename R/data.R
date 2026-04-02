#' Enumeration GBIF issue
#'
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
"EnumOccurrenceIssue"
#'
#'
#' A simple features object of the world land map
#'
#' An integrated land map
#'
#'
#'
#' @format A simple features object
#' \describe{
#'   \item{featurecla}{Land area}
#'   \item{geometry}{Geometry unit}
#' }
#' @source \code{rnaturalearth::ne_download(scale = 110,type = 'land',category = 'physical',returnclass = "sf")}
"seas_ref"
#'
#' Biodiversity Information Standards (TDWG) World Geographical Scheme for Recording Plant Distributions (WGSRPD)
#'
#' Spatial data for WGSRPD Level 3
#'
#' @format An 'sf' object with 20 rows and 5 variables:
#' \describe{
#'   \item{LEVEL3_NAM}{Region name}
#'   \item{LEVEL3_COD}{Region code}
#'   \item{LEVEL2_COD}{Level 2 code}
#'   \item{LEVEL1_COD}{Level 1 code (continent)}
#'   \item{geometry}{sf geometry}
#' }
#' @source
#' * [level3](https://github.com/tdwg/wgsrpd/tree/master/level3)
#' @references
#' Govaerts, R., Nic Lughadha, E., Black, N. et al. The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity. \emph{Sci Data} 8, 215 (2021). \doi{10.1038/s41597-021-00997-6}
"wgsrpd3"



#' The World Checklist of Vascular Plants: distributions
#'
#' A processed dataset containing the distribution data from the WCVP, mapped to the
#' Biodiversity Information Standards (TDWG) World Geographical Scheme for
#' Recording Plant Distributions (WGSRPD)
#'
#' @format A data.table with 1,950,339 rows and 6 variables:
#' \describe{
#'   \item{plant_name_id}{World Checklist of Vascular Plants (WCVP) identifier}
#'   \item{area_code_l3}{WGSRPD Level 3 code}
#'   \item{area}{WGSRPD name}
#'   \item{introduced}{0 if native; 1 if introduced}
#'   \item{extinct}{1 if extinct; 0 if extant}
#'   \item{location_doubtful}{1 if doubtful; 0 otherwise}
#' }
#' @source \url{http://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip}
#' @references
#' Govaerts, R., Nic Lughadha, E., Black, N. et al. The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity. \emph{Sci Data} 8, 215 (2021). \doi{10.1038/s41597-021-00997-6}
"wcvp_distributions"

#' The World Checklist of Vascular Plants: names
#'
#' A reduced dataset containing the taxa names from the WCVP that have distributions information
#'
#' @format A data.table with 443,347 rows and 2 variables:
#' \describe{
#'   \item{plant_name_id}{World Checklist of Vascular Plants (WCVP) identifier}
#'   \item{taxon name}{Concatenation of genus with species and, where applicable, infraspecific epithets to make a binomial or trinomial name.}
#' }
#' @source \url{http://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip}
#' @references
#' Govaerts, R., Nic Lughadha, E., Black, N. et al. The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity. \emph{Sci Data}, 8, 215 (2021). \doi{10.1038/s41597-021-00997-6}
"ref_wcvp_names"
