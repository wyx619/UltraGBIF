% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_collectors_name.R
\name{get_collectors_name}
\alias{get_collectors_name}
\title{Extract last name of the main collector (internal)}
\usage{
get_collectors_name(
  x = NA,
  surname_selection_type = "largest_string",
  max_words_name = 6,
  min_characters_in_name = 2
)
}
\arguments{
\item{x}{collectors_name in recordedBy field}

\item{surname_selection_type}{allows you to select two types of results
for the main collector's last name:

\strong{\code{large_string}} = word with the largest number of characters.

\strong{\code{last_name}} = literally the last name of the main collector, with more than two characters.}

\item{max_words_name}{assist in checking the length of name}

\item{min_characters_in_name}{min characters in name}
}
\value{
last name of the main collector
}
\description{
Extract last name of the main collector in recordedBy field
}
\details{
Returns the last name
}
