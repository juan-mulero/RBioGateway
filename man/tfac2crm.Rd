% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tfac2crm.R
\name{tfac2crm}
\alias{tfac2crm}
\title{Transcription factor to CRMs (tfac2crm)}
\usage{
tfac2crm(tfac)
}
\arguments{
\item{tfac}{Transcription factor label (string). Example: "FOSL2_HUMAN".}
}
\value{
A data.frame containing CRM names, databases, articles, biological samples, and evidence, sorted by CRM name. If no data is available, a message is returned.
}
\description{
Function to obtain cis-regulatory modules (CRMs) associated with a specific transcription factor, including their sources.
}
\examples{
\dontrun{
tfac2crm("NKX31_HUMAN")
}

}
