% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_name.R
\name{standardize_name}
\alias{standardize_name}
\title{Standardize scientific name (internal)}
\usage{
standardize_name(
  searchedName = "Lupinus polyphyllus var. pallidipes (A.Heller) C.P.Sm."
)
}
\arguments{
\item{searchedName}{the name of taxon you want to standardize}
}
\value{
A list with species name you input, standardized scientific name of it,
taxon authors of it and the last taxon author of it
}
\description{
Standardize scientific name of taxons
}
\details{
See help(standardize_name)
}
\seealso{
\code{\link[UltraGBIF]{wcvp_check_name}}
}
