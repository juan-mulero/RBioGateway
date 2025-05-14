#' Gene to CRM (gene2crm)
#'
#' Function to obtain the CRMs (Cis-Regulatory Modules) associated with a gene.
#'
#' @param gene Gene label (string). Example: "TOX3".
#'
#' @return A data.frame of CRMs associated with the gene and their sources, ordered by CRM name.
#'
#' @examples
#' \dontrun{
#' gene2crm("TOX3")
#' }
#'
#' @export

gene2crm <- function(gene) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # SPARQL prefixes used in the query
  prefixes <- "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdfs: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX sio: <http://semanticscience.org/resource/>
  "

  # SPARQL query to retrieve CRMs associated with a gene, including evidence
  query <- sprintf(
    "
    %s
    SELECT DISTINCT ?crm_name ?database ?articles
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/gene> {
        ?gene skos:prefLabel \"%s\" .
      }
      GRAPH <http://rdf.biogateway.eu/graph/crm2gene> {
        ?crm obo:RO_0002429 ?gene .
        ?uri rdfs:object ?gene .
        ?instance rdf:type ?uri ;
                  sio:SIO_000772 ?articles ;
                  sio:SIO_000253 ?database .
      }
      GRAPH <http://rdf.biogateway.eu/graph/crm> {
        ?crm skos:prefLabel ?crm_name .
      }
    }
    ORDER BY ?crm_name
    ", prefixes, gene
  )

  # Execute the SPARQL query
  results <- SPARQL(endpoint_sparql, query)$results

  # Handle empty results
  if (nrow(results) == 0) {
    return("No data available for the introduced gene or the instance is not recognized as a gene.")
  }

  # Clean and simplify URI values for readability
  results$articles <- gsub("<|>", "", results$articles)
  results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
  results$database <- gsub("<|>", "", results$database)

  # Return results as a clean, sorted data.frame
  results <- results[order(results$crm_name), ]
  return(results)
}
