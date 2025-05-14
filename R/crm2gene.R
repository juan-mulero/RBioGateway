#' CRM to gene (crm2gene)
#'
#' Function to obtain the genes associated with a CRM (Cis-Regulatory Module).
#'
#' @param crm CRM label (string). Example: "crm/CRMHS00000005764".
#'
#' @return A data.frame of genes associated with the CRM and their sources.
#'
#' @examples
#' \dontrun{
#' crm2gene("crm/CRMHS00000005764")
#' }
#'
#' @export

crm2gene <- function(crm) {
  # Define the SPARQL endpoint
  endpoint_sparql <- "https://semantics.inf.um.es/biogateway"

  # SPARQL prefixes used in the query
  prefixes <- "
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdfs: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX sio: <http://semanticscience.org/resource/>
  "

  # SPARQL query to retrieve genes associated with a CRM, including evidence
  query <- sprintf(
    "
    %s
    SELECT DISTINCT ?gene_name ?database ?articles
    WHERE {
      GRAPH <http://rdf.biogateway.eu/graph/crm> {
        ?crm skos:prefLabel '%s' .
      }
      GRAPH <http://rdf.biogateway.eu/graph/crm2gene> {
        ?crm obo:RO_0002429 ?gene .
        ?uri_relation rdf:subject ?crm ;
          rdf:predicate ?relation ;
          rdf:object ?gene .
        ?instance rdf:type ?uri_relation ;
          sio:SIO_000772 ?articles ;
          sio:SIO_000253 ?database .
      }
      GRAPH <http://rdf.biogateway.eu/graph/gene> {
        ?gene skos:prefLabel ?gene_name .
      }
    }
    ORDER BY ?gene_name
    ", prefixes, crm
  )

  # Execute the SPARQL query
  results <- SPARQL(endpoint_sparql, query)$results

  # If no results, try with altLabel
  if (nrow(results) < 1) {
    query <- sprintf(
      "
      %s
      SELECT DISTINCT ?gene_name ?database ?articles
      WHERE {
        GRAPH <http://rdf.biogateway.eu/graph/crm> {
          ?crm skos:altLabel '%s' .
        }
        GRAPH <http://rdf.biogateway.eu/graph/crm2gene> {
          ?crm obo:RO_0002429 ?gene .
          ?uri_relation rdf:subject ?crm ;
            rdf:predicate ?relation ;
            rdf:object ?gene .
          ?instance rdf:type ?uri_relation ;
            sio:SIO_000772 ?articles ;
            sio:SIO_000253 ?database .
        }
        GRAPH <http://rdf.biogateway.eu/graph/gene> {
          ?gene skos:prefLabel ?gene_name .
        }
      }
      ORDER BY ?gene_name
      ", prefixes, crm
    )

    results <- SPARQL(endpoint_sparql, query)$results
  }

  # Handle empty results
  if (nrow(results) == 0) {
    return("No data available for the introduced CRM. Check that the CRM ID is correct.")
  }

  # Simplify URI values
  results$articles <- gsub("<|>", "", results$articles)
  results$articles <- gsub("http://identifiers.org/pubmed/", "PMID:", results$articles)
  results$database <- gsub("<|>", "", results$database)

  # Return results as a data.frame
  return(results)
}
