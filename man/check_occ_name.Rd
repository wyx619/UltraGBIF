% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_occ_name.R
\name{check_occ_name}
\alias{check_occ_name}
\title{(Step 2) Use local or online taxon resolution service to check accepted taxon names and update synonyms at once}
\usage{
check_occ_name(
  occ_import = NA,
  local_taxon_resolution = TRUE,
  threads = 4,
  if_author_fails_try_without_combinations = TRUE
)
}
\arguments{
\item{occ_import}{imported GBIF occurrence data from step1}

\item{local_taxon_resolution}{if TRUE, resolve taxon names locally. Otherwise automatically use Taxonomic Name Resolution Service(TNRS) instead.}

\item{threads}{your threads requirement, a number >0. Default=4}

\item{if_author_fails_try_without_combinations}{option for partial verification of the authorship of the species.
Remove the authors of combinations, in parentheses.}
}
\value{
A list with duration and two data.table: "summary" with species list and "occ_wcvp_check_name" with WCVP fields
}
\description{
Names of species can be checked against WCVP database.
To verify individual names, the function wcvp_check_name is used.
}
\details{
See help(wcvp_check_name)
\itemize{
\item \href{http://sftp.kew.org/pub/data-repositories/WCVP/}{about WCVP database}
\item \href{https://powo.science.kew.org//}{World Checklist of Vascular Plants}
\item \href{https://github.com/matildabrown/rWCVPdata/}{rWCVPdata}
}
}
\examples{
\donttest{
help(check_occ_name)
}

}
\seealso{
\code{\link[UltraGBIF]{wcvp_check_name}}
}
