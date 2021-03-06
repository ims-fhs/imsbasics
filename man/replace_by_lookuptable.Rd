% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imsbasics.R
\name{replace_by_lookuptable}
\alias{replace_by_lookuptable}
\title{Replace columns in data.frame using lookup table}
\usage{
replace_by_lookuptable(df, col, lookup, ...)
}
\arguments{
\item{df}{A data.frame}

\item{col}{A character(array)}

\item{lookup}{A lookup table containing columns "old" and "new"}
}
\value{
df A data.frame
}
\description{
Replace a string through another one in one or more columns of a data.frame.
If for a given row there is no match found in the old-value-colum to map to
the new-value-column, NA will be inserted instead.
}
\details{
Comment: Method has been taken from zh911::my_match and renamed as ims_match.
}
