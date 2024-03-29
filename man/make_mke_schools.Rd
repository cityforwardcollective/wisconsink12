% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common_filters.R
\name{make_mke_schools}
\alias{make_mke_schools}
\alias{make_mke_rc}
\alias{make_wi_rc}
\alias{make_mke_enrollment}
\title{Make common dataframes}
\usage{
make_mke_schools(school_years = "all")

make_mke_rc(private_type = "choice")

make_wi_rc(exclude_milwaukee = TRUE, private_type = "choice")

make_mke_enrollment(agency_type = "broad")
}
\arguments{
\item{private_type}{Select which type of Report Card to be included for choice schools.
Options are 'choice' for the 'Private - Choice Students' and 'all' for 'Private - All Students'
Report Card types.}

\item{exclude_milwaukee}{Logical. If TRUE (default value), Milwaukee schools will be
excluded from \code{wi_rc}.}
}
\description{
Creates commonly-needed dataframes of \code{wisconsink12} data,
such as city of Milwaukee schools and their report cards.
}
\section{Functions}{
\itemize{
\item \code{make_mke_rc()}: Make a dataframe of Milwaukee schools' Report Card data.

\item \code{make_wi_rc()}: Make a dataframe of Wisconsin schools' Report Card data.

\item \code{make_mke_enrollment()}: Make a dataframe of overall enrollment of Milwaukee resident children.

}}
