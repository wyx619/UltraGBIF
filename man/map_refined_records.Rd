% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_refined_records.R
\name{map_refined_records}
\alias{map_refined_records}
\title{Visualize records on a dynamic interactive map}
\usage{
map_refined_records(
  records_refined = NA,
  save_to_disk_path = NA,
  precision = 4,
  draw = F,
  cex = 4
)
}
\arguments{
\item{records_refined}{the refined occurrence list from \code{coordinate_refine}}

\item{save_to_disk_path}{(optional and if you do not need save just ignore it) the local disk
path where you want to save the filtered GBIF occurrence records by \code{UltraGBIF_wcvp_family}}

\item{precision}{positive integer scalar controlling the density
of your refined GBIF occurrence records on dynamic maps. (i.e. 4 for 20 km,
3 for 156 km, 2 for 1250 km.) Default is 4}

\item{draw}{if TRUE present the map}

\item{cex}{the point size of your refined GBIF occurrence records rendering on dynamic maps. Default is 4}
}
\value{
A dynamic interactive map with \code{wcvp_area_status} legend.
Note that the map has 3 different layers for you to choose to show.
}
\description{
This optional step could map your simplified refined GBIF occurrence records on dynamic maps
}
\examples{
\donttest{
help(map_refined_records)
}
}
\seealso{
\code{\link[geohashTools]{gh_encode}}
}
