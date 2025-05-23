% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCRMs_by_coord.R
\name{getCRMs_by_coord}
\alias{getCRMs_by_coord}
\title{Get CRMs by coordinates (getCRMs_by_coord)}
\usage{
getCRMs_by_coord(chromosome, start, end)
}
\arguments{
\item{chromosome}{chromosome name (string). Example: "chr-1".}

\item{start}{start position (integer).}

\item{end}{end position (integer).}
}
\value{
A data.frame of located CRMs in the window of coordinates.
}
\description{
Function to obtain the cis-regulatory modules (CRMs) located in a range of coordinates.
}
\examples{
\dontrun{
getCRMs_by_coord("mitochondrial", 1, 500)
}

}
