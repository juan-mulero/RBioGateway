% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCRM_add_info.R
\name{getCRM_add_info}
\alias{getCRM_add_info}
\title{Get additional CRM information (getCRM_add_info)}
\usage{
getCRM_add_info(crm)
}
\arguments{
\item{crm}{Name of the CRM (string). Example: "crm/CRMHS00000005387".}
}
\value{
A list containing unique values for alternative sources, databases, biological samples, articles, and methods associated with the CRM. If no data is available, a message is returned.
}
\description{
Function to obtain additional data for a specific cis-regulatory module (CRM), including databases, alternative sources, biological samples, articles, and experimental evidence.
}
\examples{
\dontrun{
getCRM_add_info("crm/CRMHS00000005387")
}

}
