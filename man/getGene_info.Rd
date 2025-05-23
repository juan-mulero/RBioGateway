% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getGene_info.R
\name{getGene_info}
\alias{getGene_info}
\title{Get gene information (getGene_info)}
\usage{
getGene_info(gene, taxon)
}
\arguments{
\item{gene}{Gene symbol (string). Example: "Brca1".}

\item{taxon}{Taxon name (string) or taxon ID (string or integer). Example: "Mus musculus" or 10090.}
}
\value{
A list containing the start, end, strand, chromosome, assembly, definition, and alternative gene sources for the gene. If no data is available, a list with an error message is returned.
}
\description{
Function to obtain the main data for a specific gene, including coordinates, definition, and alternative sources, filtered by taxon.
}
\examples{
\dontrun{
getGene_info("Brca1", "Mus musculus")
}

}
