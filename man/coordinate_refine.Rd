% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordinate_refine.R
\name{coordinate_refine}
\alias{coordinate_refine}
\title{Refine coordinates in voucher and extract their World Geographical Scheme for Recording Plant Distributions}
\usage{
coordinate_refine(
  voucher = NA,
  threads = 4,
  save_to_disk_path = NA,
  tests = c("capitals", "centroids", "equal", "gbif", "institutions", "outliers", "seas",
    "zeros")
)
}
\arguments{
\item{voucher}{your processed voucher from \code{set_digital_voucher}}

\item{threads}{your threads requirement, a positive real number, default is 4}

\item{save_to_disk_path}{the local disk path where you want to save the final result}

\item{tests}{CoordinateCleaner coordinates checks. Choose one or more from \code{c("capitals","centroids","equal","gbif","institutions","outliers","seas","zeros")}}
}
\value{
A list with duration and 3 data.table: \code{all_records} for all cleaned records,
\code{native_records} for native records of them and \code{native_simplified} for
simplified \code{native_records} by rounding the longitude and latitude coordinates of them to two decimal places
}
\description{
Refines coordinates in voucher and extract their World Geographical Scheme for Recording Plant Distributions.
}
\details{
It can automatically clean the coordinates in voucher and extract their World Geographical Scheme for Recording Plant Distributions
}
\examples{
\donttest{
help(coordinate_refine)
}
}
